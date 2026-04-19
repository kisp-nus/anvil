open Lang
open GraphBuildContext
open EventGraph
open ErrorCollector

(** Build, check, and optimise the event graph for a single thread body.

    @param config   Compilation configuration (controls lt-checks, two-round,
                    disable_lt_checks flags).
    @param ci       Compilation-unit info (typedefs, channel classes, …).
    @param shared_vars_info  Shared-variable table for the enclosing process.
    @param construct_graphIR        AST→IR traversal from {!GraphBuilder}.
    @param graph    A freshly-constructed, empty event-graph skeleton for this
                    thread (thread_id, channels, regs, messages already set).
    @param e        The thread body expression to compile. *)
let build_thread config ci shared_vars_info construct_graphIR (graph : event_graph) (e : Lang.expr_node) =
  (* Round 1: optional lifetime-check pass on a disposable graph clone *)
  let graph_opt =
    if (not config.Config.disable_lt_checks) || config.Config.two_round_graph then (
      let tmp_graph = {graph with last_event_id = -1} in
      let unrolled_e =
        GraphAnalysis.recurse_unfold_for_checks construct_graphIR ci shared_vars_info graph e
      in
      let td  = construct_graphIR tmp_graph ci
        (Typing.BuildContext.create_empty tmp_graph shared_vars_info true)
        unrolled_e in
      tmp_graph.last_event_id <- (EventGraphOps.find_last_event tmp_graph).id;
      tmp_graph.is_general_recursive <- tmp_graph.last_event_id <> td.ld.lt.live.id;
      (* Optimise before lifetime check so the check sees the simplified graph *)
      let tmp_graph = GraphOpt.optimize config true ci tmp_graph in
      if not config.Config.disable_lt_checks then
        LifetimeCheck.lifetime_check config ci tmp_graph;
      if config.Config.two_round_graph then
        Some tmp_graph
      else
        None
    ) else None
  in
  (* Round 2: codegen graph (possibly reusing Round 1 result) *)
  match graph_opt with
  | Some g -> g
  | None ->
    (* discard after type checking *)
    let ctx = Typing.BuildContext.create_empty graph shared_vars_info false in
    let td  = construct_graphIR graph ci ctx e in
    graph.last_event_id <- (EventGraphOps.find_last_event graph).id;
    graph.is_general_recursive <- graph.last_event_id <> td.ld.lt.live.id;
    let g' = GraphOpt.optimize config false ci graph in
    GraphOpt.combinational_codegen config g' ci


(* Builds the graph representation for each process To Do: Add support for commands outside loop (be executed once or continuosly)*)
let build_proc (config : Config.compile_config) sched module_name param_values
              (ci' : cunit_info) (proc : proc_def) : proc_graph =
  let proc =
    if param_values = [] then
      proc
    else
      ParamConcretise.concretise_proc param_values proc
  in
  let macro_defs_extended =
    if List.length param_values <> List.length proc.params then
      raise_fatal (Except.TypeError [Text (Printf.sprintf "Expected %d parameters but got %d in %s instantation"
        (List.length proc.params) (List.length param_values) module_name)])
    else
      List.fold_left2 (fun acc (p : Lang.param) (pval : param_value) ->
        match p.param_ty, pval with
        | IntParam, IntParamValue v ->
          {
            id = p.param_name;
            value = v;
            span = p.span;
            cunit_file_name = Some ci'.file_name;
          } :: acc
        | _ -> acc
      ) ci'.macro_defs proc.params param_values 
    in

  let ci = {
    file_name = ci'.file_name;
    typedefs = ci'.typedefs;
    channel_classes = ci'.channel_classes;
    macro_defs = macro_defs_extended;
    func_defs = ci'.func_defs;
    weak_typecasts = ci'.weak_typecasts;
  } in




  match proc.body with
  | Native body ->
    let msg_collection = MessageCollection.create body.channels
                                      proc.args body.spawns ci.channel_classes proc.params param_values in
    let spawns =
    List.map (fun (s : spawn_def ast_node) ->
      let module_name = BuildScheduler.add_proc_task sched ci.file_name s.span s.d.proc s.d.compile_params in
      (module_name, s)
    ) body.spawns in
    let shared_vars_info = Hashtbl.create (List.length body.shared_vars) in
    List.iter (fun sv ->
      let v = {
        w = None;
        glt = sv.d.shared_lifetime;
        gdtype = unit_dtype; (* explicitly annotate? *)
      } in
      let r = {
        assigning_thread = sv.d.assigning_thread;
        value = v;
        assigned_at = None;
      } in
        Hashtbl.add shared_vars_info sv.d.ident r
      ) body.shared_vars;
      if body.threads = [] then
        raise (Except.TypeError [Text (Printf.sprintf "Process %s must have at least one thread!" proc.name)]);

      let proc_threads = List.mapi (fun i ((e, reset_by) : expr_node * message_specifier option) ->
        let graph = {
          thread_id = i;
          events = [];
          wires = WireCollection.empty;
          channels = body.channels;
          messages = msg_collection;
          spawns = body.spawns;
          regs = List.map (fun (reg : Lang.reg_def ast_node) ->
                (reg.d.name, reg)) body.regs |> Utils.StringMap.of_list;
          last_event_id = -1;
          is_general_recursive = false;
          thread_codespan = e.span;
          comb = false;
        } in
        let g = build_thread config ci shared_vars_info GraphBuilder.construct_graphIR graph e in
        (g, reset_by)
      ) body.threads in
      {name = module_name; extern_module = None;
        threads = proc_threads; shared_vars_info; messages = msg_collection;
        proc_body = proc.body; spawns = List.map (fun (ident, spawn) -> (ident, spawn)) spawns}
    | Extern (extern_mod, _extern_body) ->
      let msg_collection = MessageCollection.create [] proc.args [] ci.channel_classes [] [] in
      {name = module_name; extern_module = Some extern_mod; threads = [];
        shared_vars_info = Hashtbl.create 0; messages = msg_collection;
        proc_body = proc.body; spawns = []}

let build (config : Config.compile_config) sched module_name param_values (cunit : compilation_unit) =
  let macro_defs = cunit.macro_defs in
  let typedefs = TypedefMap.of_list cunit.type_defs in
  let func_defs = cunit.func_defs in
  let wty = config.weak_typecasts in
  let ci = {
    file_name = Option.get cunit.cunit_file_name;
    typedefs =typedefs;
    channel_classes = cunit.channel_classes;
    macro_defs = macro_defs;
    func_defs =func_defs;
    weak_typecasts = wty
  } in
  let graphs = List.map (build_proc config sched module_name param_values ci ) cunit.procs in
  {
    cunit_file_name = cunit.cunit_file_name;
    event_graphs = graphs;
    typedefs;
    macro_defs;
    channel_classes = cunit.channel_classes;
    external_event_graphs = [];
  }
