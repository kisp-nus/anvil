(** This module defines methods to assist in annotating nodes within the AST *)

let to_def_span (code_span : Lang.code_span) (cunit_fname : string option) : Lang.def_span =
  { st = code_span.st; ed = code_span.ed; cunit = cunit_fname }

let to_code_span (def_span : Lang.def_span) : Lang.code_span =
  { st = def_span.st; ed = def_span.ed }

let eq (a: Lang.code_span) (b: Lang.def_span) : bool = a.st = b.st && a.ed = b.ed
let eq_strict (a: Lang.code_span) (af: string option) (b: Lang.def_span) : bool = a.st = b.st && a.ed = b.ed && af = b.cunit


(** attaches the compilation unit filename to all top-level scoped definitions within the given target compilation unit *)
let attach_def_cunit_fname (target : Lang.compilation_unit) =
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

(** attaches definition information to the target (1st arg) from the source (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_span_expr (target : 'a Lang.ast_node) (source : 'b Lang.ast_node) (source_cunit_fname: string option) =
  if not (List.exists (eq_strict source.span source_cunit_fname) target.def_span) then
    let source_def_span = to_def_span source.span source_cunit_fname in
    target.def_span <- List.append (source_def_span :: source.def_span) target.def_span

(** attaches definition information to the target (1st arg) from the source code span (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_from_code_span (target : 'a Lang.ast_node) (source_span : Lang.code_span) (source_cunit_fname: string option) =
  if not (List.exists (eq_strict source_span source_cunit_fname) target.def_span) then
    let source_def_span = to_def_span source_span source_cunit_fname in
    target.def_span <- source_def_span :: target.def_span

(** attaches definition information to the target (1st arg) from the source def span (2nd arg) *)
let attach_def_span (target : 'a Lang.ast_node) (source_span : Lang.def_span) =
  if not (List.exists ((=) source_span) target.def_span) then
    target.def_span <- source_span :: target.def_span



(** Top-level helpers **)

(** attaches definition information to the target (1st arg) from the source top-level channel_class_def (2nd arg) *)
let attach_def_from_top_level_channel_class (target : 'a Lang.ast_node) (source : Lang.channel_class_def) =
  if not (List.exists (eq_strict source.span source.cunit_file_name) target.def_span) then
    let source_def_span = to_def_span source.span source.cunit_file_name in
    target.def_span <- List.append (source_def_span :: target.def_span) target.def_span

(** attaches definition information to the target (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type (target : 'a Lang.ast_node) (source : Lang.type_def) =
  if not (List.exists (eq_strict source.span source.cunit_file_name) target.def_span) then
    let source_def_span = to_def_span source.span source.cunit_file_name in
    target.def_span <- List.append (source_def_span :: target.def_span) target.def_span

(** attaches definition information to the target (1st arg) from the source top-level macro_def (2nd arg) *)
let attach_def_from_top_level_macro (target : 'a Lang.ast_node) (source : Lang.macro_def) =
  if not (List.exists (eq_strict source.span source.cunit_file_name) target.def_span) then
    let source_def_span = to_def_span source.span source.cunit_file_name in
    target.def_span <- List.append (source_def_span :: target.def_span) target.def_span

(** attaches definition information to the target (1st arg) from the source top-level func_def (2nd arg) *)
let attach_def_from_top_level_func (target : 'a Lang.ast_node) (source : Lang.func_def) =
  if not (List.exists (eq_strict source.span source.cunit_file_name) target.def_span) then
    let source_def_span = to_def_span source.span source.cunit_file_name in
    target.def_span <- List.append (source_def_span :: target.def_span) target.def_span

(** attaches definition information to the target (1st arg) from the source top-level proc_def (2nd arg) *)
let attach_def_from_top_level_proc (target : 'a Lang.ast_node) (source : Lang.proc_def) =
  if not (List.exists (eq_strict source.span source.cunit_file_name) target.def_span) then
    let source_def_span = to_def_span source.span source.cunit_file_name in
    target.def_span <- List.append (source_def_span :: target.def_span) target.def_span

(** attaches definition information to the target (1st arg) from the source top-level message_def (2nd arg) *)
let attach_def_from_top_level_message (target : 'a Lang.ast_node) (source : Lang.message_def) (spec: Lang.message_specifier) (graph: EventGraph.event_graph) =
  if not (List.exists (eq_strict source.span source.cunit_file_name) target.def_span) then
    let source_def_span = to_def_span source.span source.cunit_file_name in
    target.def_span <- List.append (source_def_span :: target.def_span) target.def_span;

  let ep = spec.endpoint in
  let located_defs =
    let is_match (e : Lang.endpoint_def) = e.name = ep in
    List.find_opt is_match (graph.messages.endpoints @ graph.messages.args)
    in match located_defs with
    | Some ep ->
      attach_def_from_code_span target ep.span (source.cunit_file_name);
    | _ -> ()


(** attaches definition information to the target (1st arg) from the source top-level type_def (2nd arg) *)
let attach_def_from_top_level_type_with_fields (target : 'a Lang.ast_node) (source : Lang.type_def) (fields: (Lang.identifier * 'b Lang.ast_node) list) =

  attach_def_from_top_level_type target source;

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

  List.iter (annotator record_fields) fields;
  List.iter (annotator variant_fields) fields;
  ()


(** Event helpers **)

(** attaches event information to the target (1st arg) from the source (2nd arg), optionally sustained until the given event (3rd arg) *)
let attach_event (target : 'a Lang.ast_node) (source : EventGraph.event) (sustained_until : EventGraph.event option) =
  target.action_event <- Some (
    source.graph.thread_id,
    source.id,
    Option.map (fun (e: EventGraph.event) -> e.id) sustained_until
  )
