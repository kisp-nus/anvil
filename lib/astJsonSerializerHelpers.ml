(** Shared helpers for AST JSON serializers. *)

open Lang

let assoc fields = `Assoc fields
let str value = `String value
let int value = `Int value
let bool value = `Bool value
let list f xs = `List (List.map f xs)
let list_rev f xs = `List (List.rev_map f xs)

let opt f = function
  | None -> `Null
  | Some value -> f value

let kind (name : string) (fields : (string * Yojson.Safe.t) list) = assoc (("kind", str name) :: fields)

let code_span_to_yojson (s : code_span) =
  if s = code_span_dummy then `Null
  else
    let open Lexing in
    assoc
      [
        ("start", assoc [("line", int s.st.pos_lnum); ("col", int (s.st.pos_cnum - s.st.pos_bol))]);
        ("end", assoc [("line", int s.ed.pos_lnum); ("col", int (s.ed.pos_cnum - s.ed.pos_bol))]);
      ]
