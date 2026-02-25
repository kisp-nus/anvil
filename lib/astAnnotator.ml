(** This module defines methods to assist in annotating nodes within the AST *)

let to_def_span (code_span : Lang.code_span) (cunit_fname : string option) : Lang.def_span =
  { st = code_span.st; ed = code_span.ed; cunit = cunit_fname }

let to_code_span (def_span : Lang.def_span) : Lang.code_span =
  { st = def_span.st; ed = def_span.ed }

let eq (a: Lang.code_span) (b: Lang.def_span) : bool = a.st = b.st && a.ed = b.ed


(** attaches definition information to the target (1st arg) from the source (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_span_expr (target : 'a Lang.ast_node) (source : 'b Lang.ast_node) (source_cunit_fname: string option) =
  if not (List.exists (eq source.span) target.def_span) then
    let source_def_span = to_def_span source.span source_cunit_fname in
    target.def_span <- List.append (source_def_span :: source.def_span) target.def_span

(** attaches definition information to the target (1st arg) from the source code span (2nd arg) from the given source compilation unit filename (3rd arg) *)
let attach_def_from_code_span (target : 'a Lang.ast_node) (source_span : Lang.code_span) (source_cunit_fname: string option) =
  if not (List.exists (eq source_span) target.def_span) then
    let source_def_span = to_def_span source_span source_cunit_fname in
    target.def_span <- source_def_span :: target.def_span

(** attaches definition information to the target (1st arg) from the source def span (2nd arg) *)
let attach_def_span (target : 'a Lang.ast_node) (source_span : Lang.def_span) =
  if not (List.exists ((=) source_span) target.def_span) then
    target.def_span <- source_span :: target.def_span


(** attaches event information to the target (1st arg) from the source (2nd arg), optionally sustained until the given event (3rd arg) *)
let attach_event (target : 'a Lang.ast_node) (source : EventGraph.event) (sustained_until : EventGraph.event option) =
  target.action_event <- Some (
    source.graph.thread_id,
    source.id,
    Option.map (fun (e: EventGraph.event) -> e.id) sustained_until
  )
