(** Driver that controls the overall compilation process, handling things include file importing. *)

type parse_result = {
  compilation_units : (string * Lang.compilation_unit) list;
  graph_collections : (string * EventGraph.event_graph_collection) list;
  errors : exn list;
}

(** Parses the input files and returns the resulting list of compilation units,
    and optionally the graph collections if parsing and checking succeed *)
val parse : Config.compile_config -> parse_result

(** Performs the end-to-end compilation process, including parsing, checking, and code generation,
    based on the config, and outputs the generated code to the given output channel *)
val compile : out_channel -> Config.compile_config -> unit
