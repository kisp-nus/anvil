(** This module defines methods to assist in annotating nodes within the AST *)

let enabled = ref false

let delay_symbol_counter = ref 0
let seq_delay_symbol_map : ((int * int * int), string) Hashtbl.t = Hashtbl.create 128

let clear_delay_to_exec_annotations_cache () =
  delay_symbol_counter := 0;
  Hashtbl.clear seq_delay_symbol_map

let fresh_symbolic_delay_var () =
  let n = !delay_symbol_counter in
  delay_symbol_counter := n + 1;
  Printf.sprintf "n%d" n

let lookup_seq_delay_symbol tid from_eid to_eid =
  Hashtbl.find_opt seq_delay_symbol_map (tid, from_eid, to_eid)

let annotate_delay_to_exec (gcol : EventGraph.event_graph_collection) =
  let update_nodes_for_seq_delay (thread_id : int) (start_eid : int) (end_eid : int) (delay : Lang.exec_delay) (nodes : Lang.expr_node list) =
    List.iter (fun (node : Lang.expr_node) ->
      match node.action_event with
      | Some (tid, eid, Some ueid, _) when tid = thread_id && eid = start_eid && ueid = end_eid ->
        node.action_event <- Some (tid, eid, Some ueid, delay)
      | _ -> ()
    ) nodes
  in
  let annotate_event (lookup_message : Lang.message_specifier -> Lang.message_def option) (all_nodes : Lang.expr_node list) (ev : EventGraph.event) =
    match ev.source with
    | `Seq (start_ev, `Send msg_spec) ->
      (match lookup_message msg_spec with
      | Some msg when not (GraphAnalysis.message_is_immediate msg true) ->
        let sym = fresh_symbolic_delay_var () in
        let delay = [Lang.DelaySym sym] in
        Hashtbl.replace seq_delay_symbol_map (ev.graph.thread_id, start_ev.id, ev.id) sym;
        update_nodes_for_seq_delay ev.graph.thread_id start_ev.id ev.id delay all_nodes
      | _ -> ())
    | `Seq (start_ev, `Recv msg_spec) ->
      (match lookup_message msg_spec with
      | Some msg when not (GraphAnalysis.message_is_immediate msg false) ->
        let sym = fresh_symbolic_delay_var () in
        let delay = [Lang.DelaySym sym] in
        Hashtbl.replace seq_delay_symbol_map (ev.graph.thread_id, start_ev.id, ev.id) sym;
        update_nodes_for_seq_delay ev.graph.thread_id start_ev.id ev.id delay all_nodes
      | _ -> ())
    | `Seq (start_ev, `Sync _) ->
      let sym = fresh_symbolic_delay_var () in
      let delay = [Lang.DelaySym sym] in
      Hashtbl.replace seq_delay_symbol_map (ev.graph.thread_id, start_ev.id, ev.id) sym;
      update_nodes_for_seq_delay ev.graph.thread_id start_ev.id ev.id delay all_nodes
    | _ -> ()
  in
  List.iter (fun (pg : EventGraph.proc_graph) ->
    let lookup_message msg_spec = MessageCollection.lookup_message pg.messages msg_spec gcol.channel_classes in
    List.iter (fun ((g : EventGraph.event_graph), _) ->
      let all_nodes = List.concat_map (fun (ev : EventGraph.event) -> ev.expr_nodes) g.events in
      List.iter (annotate_event lookup_message all_nodes) g.events
    ) pg.threads
  ) gcol.event_graphs


let to_def_span (code_span : Lang.code_span) (cunit_fname : string option) : Lang.def_span =
  { st = code_span.st; ed = code_span.ed; cunit = cunit_fname }

let to_code_span (def_span : Lang.def_span) : Lang.code_span =
  { st = def_span.st; ed = def_span.ed }

let merge_def_spans (extra: Lang.def_span list) (base: Lang.def_span list) : Lang.def_span list =
  let append item base =
    if List.exists ((=) item) base then base
    else item :: base
  in List.fold_right append extra base

(** attaches the compilation unit filename to all top-level scoped definitions within the given target compilation unit *)
let attach_def_cunit_fname (target : Lang.compilation_unit) =
  if not !enabled then () else

  let n = target.cunit_file_name in

  let apply (cc: Lang.channel_class_def) = cc.cunit_file_name <- n in
  let _ = List.iter apply target.channel_classes in

  let apply (td: Lang.type_def) = td.cunit_file_name <- n in
  let _ = List.iter apply target.type_defs in

  let apply (md: Lang.macro_def) = md.cunit_file_name <- n in
  let _ = List.iter apply target.macro_defs in

  let apply (fd: Lang.func_def) = fd.cunit_file_name <- n in
  let _ = List.iter apply target.func_defs in

  let apply (pd: Lang.proc_def) = pd.cunit_file_name <- n in
  let _ = List.iter apply target.procs in
  let _ = List.iter apply target._extern_procs in

  let apply (md: Lang.message_def) = md.cunit_file_name <- n in
  let apply_to_chan_msgs (cc: Lang.channel_class_def) = List.iter apply cc.messages in
  let _ = List.iter apply_to_chan_msgs target.channel_classes in

  ()



(** Scoped definition helpers **)
(**
   Dev Notes:

   All definition information should be attached from the closest source node first.

   For example, if x has definition y which has definition z, and z's definition is useful to x,
   then x should have y attached first before z.
  *)

(** attaches definition information to the target (1st arg) from the source (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_span_expr (target : 'a Lang.ast_node) (source : 'b Lang.ast_node) (source_cunit_fname: string option) =
  if not !enabled then () else

  let base_def_span = source.def_span in
  let source_def_span = to_def_span source.span source_cunit_fname in
  let target_def_span = target.def_span in

  let merged_def_span = merge_def_spans [source_def_span] base_def_span |> merge_def_spans target_def_span in
  target.def_span <- merged_def_span

(** attaches definition information to the target (1st arg) from the source code span (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_from_code_span (target : 'a Lang.ast_node) (source_span : Lang.code_span) (source_cunit_fname: string option) =
  if not !enabled then () else

  let source_def_span = to_def_span source_span source_cunit_fname in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source def span (2nd arg) *)
let attach_def_span (target : 'a Lang.ast_node) (source_span : Lang.def_span) =
  target.def_span <- merge_def_spans [source_span] target.def_span



(** Top-level helpers **)

(** attaches definition information to the target (1st arg) from the source top-level channel_class_def (2nd arg) *)
let attach_def_from_top_level_channel_class (target : 'a Lang.ast_node) (source : Lang.channel_class_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type (target : 'a Lang.ast_node) (source : Lang.type_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level macro_def (2nd arg) *)
let attach_def_from_top_level_macro (target : 'a Lang.ast_node) (source : Lang.macro_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level func_def (2nd arg) *)
let attach_def_from_top_level_func (target : 'a Lang.ast_node) (source : Lang.func_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level proc_def (2nd arg) *)
let attach_def_from_top_level_proc (target : 'a Lang.ast_node) (source : Lang.proc_def) =
  if not !enabled then () else

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level message_def (2nd arg) *)
let attach_def_from_top_level_message (target : 'a Lang.ast_node) (source : Lang.message_def) (spec: Lang.message_specifier) (graph: EventGraph.event_graph) =
  if not !enabled then () else

  let ep = spec.endpoint in
  let located_defs =
    let is_match (e : Lang.endpoint_def) = e.name = ep in
    List.find_opt is_match (graph.messages.endpoints @ graph.messages.args)
  in
  (
    match located_defs with
    | Some ep ->
      attach_def_from_code_span target ep.span (source.cunit_file_name);
    | _ -> ()
  );

  let source_def_span = to_def_span source.span source.cunit_file_name in
  target.def_span <- merge_def_spans [source_def_span] target.def_span


(** attaches definition information to the fields (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type_fields (target_fields: (Lang.identifier * 'a Lang.ast_node) list) (source : Lang.type_def) =
  if not !enabled then () else

  let record_fields = match source.body with
    | `Record fields -> List.map (fun (n: 'c Lang.ast_node) -> (fst n.d, n)) fields
    | _ -> []
  in

  let variant_fields = match source.body with
    | `Variant (_, variants) -> List.map (fun (n: 'd Lang.ast_node) -> let id, _, _ = n.d in (id, n)) variants
    | _ -> []
  in

  let annotator def_fields = (fun (field_ident, field_expr) ->
    match (List.assoc_opt field_ident def_fields) with
    | Some field_type_data ->
        attach_def_from_top_level_type field_expr source;
        attach_def_span_expr field_expr field_type_data source.cunit_file_name
    | None -> ()
  )
  in

  List.iter (annotator record_fields) target_fields;
  List.iter (annotator variant_fields) target_fields;
  ()

(** attaches definition information to the target (1st arg) from the source top-level type_def (2nd arg) and its fields (3rd arg) *)
let attach_def_from_top_level_type_with_fields (target : 'a Lang.ast_node) (source : Lang.type_def) (fields: (Lang.identifier * 'b Lang.ast_node) list) =
  if not !enabled then () else

  attach_def_from_top_level_type target source;
  attach_def_from_top_level_type_fields fields source



(** Event helpers **)

(** attaches event information to the target (1st arg) from the source (2nd arg), optionally sustained until the given event (3rd arg); optionally adding delay_to_exec (4th arg) *)
let attach_event (target : 'a Lang.ast_node) (source : EventGraph.event) (sustained_until : EventGraph.event option) (delay_to_exec : Lang.exec_delay option) =
  if not !enabled then () else

  target.action_event <- Some (
    source.graph.thread_id,
    source.id,
    Option.map (fun (e: EventGraph.event) -> e.id) sustained_until,
    match delay_to_exec with | Some d -> d | None -> []
  );
  source.expr_nodes <- target :: source.expr_nodes
