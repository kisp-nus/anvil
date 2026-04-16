open CompileHelpers

(* Compilation steps *)

(** Parses the input files and returns a list of compilation units. Raises CompileError if any error is encountered. *)
let _parse config =
  let open Config in
  let toplevel_filename = if config.stdin && List.length config.input_filenames = 0
    then "-"
    else List.hd config.input_filenames
  in
  let cunits = ref [] in
  (
    try parse_recursive cunits (ref Utils.StringSet.empty) config toplevel_filename
    with
      | Except.TypeError msg ->
        (Except.Text "Type error:")::msg
          |> raise_compile_error None
  );
  (* attach the compilation unit filename to all top-level definitions for definition lookup later *)
  List.iter (fun (_, cunit) -> AstAnnotator.attach_def_cunit_fname cunit) !cunits;
  !cunits


(** Performs type and lifetime checks, and returns the graph collection queue *)
let _check config cunits =

  (* collect all channel class and type definitions *)
  let all_channel_classes = List.concat_map (fun (_, cunit) -> let open Lang in cunit.channel_classes) cunits in
  let all_type_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.type_defs) cunits in
  let all_procs = List.concat_map (fun (file_name, cunit) -> let open Lang in List.map (fun p -> (file_name, p)) cunit.procs) cunits in
  let all_func_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.func_defs) cunits in
  let all_macro_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.macro_defs) cunits in
  let proc_map = List.map (fun (file_name, proc) -> (let open Lang in (proc:proc_def).name, (proc, file_name))) all_procs
    |> Utils.StringMap.of_list in
  let sched = BuildScheduler.create () in

  (* annotate top-level constructs that don't need graph traversal with their definitions *)
  (
    let proc_lookup_table = List.map (fun ((_, proc): string * Lang.proc_def) -> (proc.name, proc)) all_procs in

    let annotate_proc_def (proc: Lang.proc_def) =
      (* annotate endpoint defs with their channel class definitions *)
      let annotate_ep_chan_cls_def (ep: Lang.endpoint_def Lang.ast_node) =
        match MessageCollection.lookup_channel_class all_channel_classes ep.d.channel_class with
        | Some cc -> AstAnnotator.attach_def_from_top_level_channel_class ep cc
        | _ -> ()
      in
      List.iter (fun ep -> annotate_ep_chan_cls_def ep) proc.args;

      (* annotate channel defs with their channel class definitions *)
      let annotate_chan_chan_cls_def (chan: Lang.channel_def Lang.ast_node) =
        match MessageCollection.lookup_channel_class all_channel_classes chan.d.channel_class with
        | Some cc -> AstAnnotator.attach_def_from_top_level_channel_class chan cc
        | _ -> ()
      in
      (match proc.body with
        | Native body -> List.iter (fun chan -> annotate_chan_chan_cls_def chan) body.channels
        | Extern _ -> ()
      );

      (* annotate spawn defs with their proc definitions *)
      let annotate_spawn_proc_def (spawn: Lang.spawn_def Lang.ast_node) =
        match List.assoc_opt spawn.d.proc proc_lookup_table with
        | Some proc_def -> AstAnnotator.attach_def_from_top_level_proc spawn proc_def;
        | None -> AstAnnotator.attach_def_from_code_span spawn spawn.span None
      in
      (match proc.body with
        | Native body -> List.iter (fun spawn -> annotate_spawn_proc_def spawn) body.spawns
        | Extern _ -> ()
      );
    in

    List.iter (fun (_, proc) -> annotate_proc_def proc) all_procs;
  );

  (* add processes that are concrete *)
  List.iter (fun (file_name, proc) ->
    let open Lang in
    if proc.params = [] then
      let _ = BuildScheduler.add_proc_task sched file_name Lang.code_span_dummy proc.name [] in ()
  ) all_procs;
  let modules_visited = ref Utils.StringSet.empty in
  let event_graph_complete = ref false in
  let graph_collection_queue = Queue.create () in
  while not !event_graph_complete do
    match BuildScheduler.next sched with
    | None -> event_graph_complete := true
    | Some task -> (
      if Utils.StringSet.mem task.module_name !modules_visited |> not then (
        modules_visited := Utils.StringSet.add task.module_name !modules_visited;
        let proc_file_opt = Utils.StringMap.find_opt
          (let open BuildScheduler in task.proc_name)
          proc_map in
        let open Except in
        match proc_file_opt with
        | None ->
          let msg_text = Printf.sprintf "Process '%s' not found!" task.proc_name in
          raise_compile_error None
            [
              Text msg_text;
              codespan_in task.file_name task.codespan
            ]
        | Some (proc, file_name) ->
          let cunit = let open Lang in
            (* hacky *)
            {cunit_file_name = Some file_name;
            channel_classes = all_channel_classes; type_defs = all_type_defs;
            procs = [proc]; imports = []; _extern_procs = [];
            func_defs = all_func_defs;
            macro_defs = all_macro_defs} in
          let graph_collection = (
            match GraphBuildDriver.build config sched task.module_name task.param_values cunit
            with
            | res -> res
            | exception exc ->
                raise (convert_intermediate_anvil_errors exc (Some file_name))
          ) in

          ErrorCollector.map_collected_errors (fun e ->
            convert_intermediate_anvil_errors e (Some file_name)
          );
          Queue.add graph_collection graph_collection_queue
      )
    )
  done;
  graph_collection_queue

(** Performs code generation from checked results and graph *)
let _codegen config out cunits graph_collection_queue =
  begin
    (* generate preamble *)
    Codegen.generate_preamble out;
    (* pull code from imported external files *)
    let visited_extern_files = ref Utils.StringSet.empty in
    let open Lang in
    List.iter (fun (file_name, cunit) ->
      List.iter (fun {is_extern; file_name = imp_file_name; span = _} ->
        if is_extern then
          let imp_file_name_canonical = canonicalise_file_name file_name imp_file_name in
          if Utils.StringSet.mem imp_file_name_canonical !visited_extern_files |> not then (
            visited_extern_files := Utils.StringSet.add imp_file_name_canonical !visited_extern_files;
            try Codegen.generate_extern_import out imp_file_name_canonical
            with Sys_error msg -> raise_compile_error (Some file_name) [Except.Text msg]
          )
      ) cunit.imports
    ) cunits;
    (* generate the code from event graphs *)
    let all_collections = Queue.to_seq graph_collection_queue |> List.of_seq in
    let all_event_graphs = List.concat_map (fun collection -> let open EventGraph in collection.event_graphs) all_collections in
    List.iter (fun graphs ->
      Codegen.generate out config
       {graphs with EventGraph.external_event_graphs = all_event_graphs}) all_collections
  end



(* Module methods *)

(** Parses the input files and returns the resulting list of compilation units,
    and optionally the graph collections if parsing and checking succeed or any partial compile errors. *)
let parse config =
  let cunits = _parse config in

  let graph_collections =

    ErrorCollector.collect_errors := true;

    let check_result =
      try Some (_check config cunits)
      with | CompileError x ->
        ErrorCollector.push_new_error (CompileError x);
        None
    in

    ErrorCollector.collect_errors := false;

    let assoc_list = match check_result with
      | Some gc_queue ->
          let gc_list = Queue.to_seq gc_queue |> List.of_seq in
          AstAnnotator.clear_delay_to_exec_annotations_cache ();
          List.iter AstAnnotator.annotate_delay_to_exec gc_list;
          let gcol_to_lookup (gcol : EventGraph.event_graph_collection) =
            match gcol.cunit_file_name with
              | Some fname -> Some (fname, gcol)
              | None -> None
          in
          List.filter_map gcol_to_lookup gc_list
      | None -> []
    in
    assoc_list
  in

  ErrorCollector.dedup_collected_errors ();

  let collected_errors = ErrorCollector.get_collected_errors () in

  ErrorCollector.clear_collected_errors ();

  let mapped e =
    match convert_intermediate_anvil_errors e None with
    | CompileError msg -> CompileError msg
    | e -> e
  in

  cunits, graph_collections, List.map mapped collected_errors


(** Performs the end-to-end compilation process, including parsing, checking, and code generation,
    based on the config, and outputs the generated code to the given output channel *)
let compile out config =
  let cunits = _parse config in
  let graph_collection_queue = _check config cunits in
  if config.just_check then ()
  else _codegen config out cunits graph_collection_queue
