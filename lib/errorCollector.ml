(** This module provides a mechanism for collecting errors instead of raising them immediately. *)

(* Internal tracking *)
let _global_errors : exn list ref = ref []
let _system_raise = raise

(* Global toggles *)
let collect_errors : bool ref = ref false

(* Method injection *)
let raise (e: exn) =
  if !collect_errors then
    _global_errors := e :: !_global_errors
  else
    _system_raise e

let raise_fatal (e: exn) =
  if !collect_errors then
    _global_errors := e :: !_global_errors;
  _system_raise e

(* API *)
let has_collected_errors () : bool =
  !_global_errors <> []

let dedup_collected_errors () =
  let rec dedup seen = function
    | [] -> List.rev seen
    | x :: xs ->
      if List.exists (fun e -> e = x) seen then
        dedup seen xs
      else
        dedup (x :: seen) xs
  in
  _global_errors := dedup [] !_global_errors

let push_new_error (e: exn) : unit =
  _global_errors := e :: !_global_errors

let map_collected_errors (f: exn -> exn) : unit =
  _global_errors := List.map (fun e -> f e) !_global_errors

let get_collected_errors () : exn list =
  !_global_errors

let clear_collected_errors () : unit =
  _global_errors := []
