(** This module defines methods to assist in annotating nodes within the AST *)

let enabled = ref false

let annotate_delay_to_exec (gcol : EventGraph.event_graph_collection) =
  let update_nodes_for_seq_delay (thread_id : int) (start_eid : int) (delay : Lang.exec_delay) (nodes : Lang.expr_node list) =
    List.iter (fun (node : Lang.expr_node) ->
      match node.action_event with
      | Some (tid, eid, _) when tid = thread_id && eid = start_eid ->
        node.action_event <- Some (tid, eid, delay)
      | _ -> ()
    ) nodes
  in
  List.iter (fun (pg : EventGraph.proc_graph) ->
    let delay_symbol_counter = ref 0 in
    let fresh_symbolic_delay_var () =
      let n = !delay_symbol_counter in
      delay_symbol_counter := n + 1;
      Printf.sprintf "n%d" n
    in
    let annotate_event (lookup_message : Lang.message_specifier -> Lang.message_def option) (all_nodes : Lang.expr_node list) (ev : EventGraph.event) =
      let annotate_seq_delay (start_ev : EventGraph.event) =
        let sym = fresh_symbolic_delay_var () in
        let delay = [Lang.DelaySym sym] in
        ev.seq_delay_symbol <- Some sym;
        update_nodes_for_seq_delay ev.graph.thread_id start_ev.id delay all_nodes
      in
      match ev.source with
      | `Seq (start_ev, `Send msg_spec) ->
        (match lookup_message msg_spec with
        | Some msg when not (GraphAnalysis.message_is_immediate msg true) -> annotate_seq_delay start_ev
        | _ -> ())
      | `Seq (start_ev, `Recv msg_spec) ->
        (match lookup_message msg_spec with
        | Some msg when not (GraphAnalysis.message_is_immediate msg false) -> annotate_seq_delay start_ev
        | _ -> ())
      | `Seq (start_ev, `Sync _) -> annotate_seq_delay start_ev
      | _ -> ()
    in
    let lookup_message msg_spec = MessageCollection.lookup_message pg.messages msg_spec gcol.channel_classes in
    List.iter (fun ((g : EventGraph.event_graph), _) ->
      let all_nodes = List.concat_map (fun (ev : EventGraph.event) -> ev.expr_nodes) g.events in
      List.iter (annotate_event lookup_message all_nodes) g.events
    ) pg.threads
  ) gcol.event_graphs


let merge_def_spans (extra: Lang.code_span list) (base: Lang.code_span list) : Lang.code_span list =
  let append item base =
    if List.exists ((=) item) base then base
    else item :: base
  in List.fold_right append extra base

(** Scoped definition helpers **)
(**
   Dev Notes:

   All definition information should be attached from the closest source node first.

   For example, if x has definition y which has definition z, and z's definition is useful to x,
   then x should have y attached first before z.
  *)

(** attaches definition information to the target (1st arg) from the source (2nd arg) *)
let attach_def_span_expr (target : 'a Lang.ast_node) (source : 'b Lang.ast_node) =
  if not !enabled then () else

  let base_def_span = source.def_span in
  let source_def_span = source.span in
  let target_def_span = target.def_span in

  let merged_def_span = merge_def_spans [source_def_span] base_def_span |> merge_def_spans target_def_span in
  target.def_span <- merged_def_span

(** attaches definition information to the target (1st arg) from the source code span (2nd arg) *)
let attach_def_from_code_span (target : 'a Lang.ast_node) (source_span : Lang.code_span) =
  if not !enabled then () else

  target.def_span <- merge_def_spans [source_span] target.def_span

(** attaches definition information to the target (1st arg) from the source def span (2nd arg) *)
let attach_def_span (target : 'a Lang.ast_node) (source_span : Lang.code_span) =
  target.def_span <- merge_def_spans [source_span] target.def_span



(** Top-level helpers **)

(** attaches definition information to the target (1st arg) from the source top-level channel_class_def (2nd arg) *)
let attach_def_from_top_level_channel_class (target : 'a Lang.ast_node) (source : Lang.channel_class_def) =
  if not !enabled then () else

  target.def_span <- merge_def_spans [source.span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type (target : 'a Lang.ast_node) (source : Lang.type_def) =
  if not !enabled then () else

  target.def_span <- merge_def_spans [source.span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level macro_def (2nd arg) *)
let attach_def_from_top_level_macro (target : 'a Lang.ast_node) (source : Lang.macro_def) =
  if not !enabled then () else

  target.def_span <- merge_def_spans [source.span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level func_def (2nd arg) *)
let attach_def_from_top_level_func (target : 'a Lang.ast_node) (source : Lang.func_def) =
  if not !enabled then () else

  target.def_span <- merge_def_spans [source.span] target.def_span

(** attaches definition information to the target (1st arg) from the source top-level proc_def (2nd arg) *)
let attach_def_from_top_level_proc (target : 'a Lang.ast_node) (source : Lang.proc_def) =
  if not !enabled then () else

  target.def_span <- merge_def_spans [source.span] target.def_span

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
      attach_def_from_code_span target ep.span;
    | _ -> ()
  );

  target.def_span <- merge_def_spans [source.span] target.def_span


(** attaches definition information to the fields (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type_fields (target_fields: (Lang.identifier * 'a Lang.ast_node) list) (source : Lang.type_def) =
  if not !enabled then () else

  let record_fields = match source.body with
    | `Record fields -> List.map (fun ({d = (id, _); _} as n : 'c Lang.ast_node) -> (id, n)) fields
    | _ -> []
  in

  let variant_fields = match source.body with
    | `Variant (_, variants) -> List.map (fun ({d = (id, _, _); _} as n : 'd Lang.ast_node) -> (id, n)) variants
    | _ -> []
  in

  let annotator def_fields = (fun (field_ident, field_expr) ->
    match (List.assoc_opt field_ident def_fields) with
    | Some field_type_data ->
        attach_def_from_top_level_type field_expr source;
        attach_def_span_expr field_expr field_type_data
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

  let _ = sustained_until in
  target.action_event <- Some (
    source.graph.thread_id,
    source.id,
    match delay_to_exec with | Some d -> d | None -> []
  );
  source.expr_nodes <- target :: source.expr_nodes
