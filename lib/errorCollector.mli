(** This module provides a mechanism for collecting errors instead of raising them immediately. *)



(* Global toggles *)

(** Collects errors instead of raising them immediately.
    When set to true, errors will be collected in an internal list instead of being raised.
    When set to false, errors will be raised immediately as usual. *)
val collect_errors : bool ref



(* Method injection *)

(** Injected error raise function with error collection.

    This replaces the standard raise function in the file where this module is used, and may optionally
    collect errors instead of raising them immediately based on the value of [collect_errors].

    If you wish to guarantee that an error is raised immediately regardless of the state of [collect_errors],
    use [raise_fatal] instead. *)
val raise : exn -> unit

(** Injected error raise function that always raises immediately.

    This function will raise the given exception immediately, regardless of the state of [collect_errors].
    However, if [collect_errors] is true, it will also add the exception to the internal list of collected errors. *)
val raise_fatal : exn -> 'a



(* API *)

(** Checks if there are any collected errors. *)
val has_collected_errors : unit -> bool

(** Deduplicates the collected errors, removing any duplicates from the internal list. *)
val dedup_collected_errors : unit -> unit

(** Pushes a new error into the list of collected errors. *)
val push_new_error : exn -> unit

(** Retrieves the list of collected errors. *)
val get_collected_errors : unit -> exn list

(** Clears the list of collected errors, resetting it to an empty state. *)
val clear_collected_errors : unit -> unit
