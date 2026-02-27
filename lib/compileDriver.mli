(** Driver that controls the overall compilation process, handling things include file importing. *)

(** Containing file name, code span, and error message. *)
exception CompileError of Except.error_message

(** Parses the input files and returns the resulting list of compilation units,
    and optionally the graph collections if parsing and checking succeed *)
val parse : Config.compile_config -> ((string * Lang.compilation_unit) list) * (string * EventGraph.event_graph_collection) list * (exn list)

(** Performs the end-to-end compilation process, including parsing, checking, and code generation,
    based on the config, and outputs the generated code to the given output channel *)
val compile : out_channel -> Config.compile_config -> unit
