(** Driver that controls the overall compilation process, handling things include file importing. *)

val compile : ?verification:bool -> out_channel -> Config.compile_config -> unit

val verification_run : Config.compile_config -> unit