(** Driver that controls the overall compilation process, handling things include file importing. *)

val compile : out_channel -> Config.compile_config -> unit
