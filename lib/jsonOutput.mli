(** JSON output module for anvil compiler results *)

type json_position = {
  line: int;
  col: int;
}

type json_trace = {
  path: string option;
  start_pos: json_position;
  end_pos: json_position;
}

type json_fragment = {
  kind: string;  (* "text" | "codespan" *)
  text: string option;
  trace: json_trace option;
}

type json_error = {
  error_type: string;  (* "warning" | "error" *)
  path: string option;
  description: json_fragment list;
}

type json_output = {
  success: bool;
  errors: json_error list;
  output: string option;
  ast: Yojson.Safe.t option;
}

(** Converts an error message to JSON errors *)
val error_message_to_json_error : string -> Except.error_message -> json_error

(** Converts a JSON output to string *)
val json_output_to_string : json_output -> string

(** Creates a successful JSON output with the transpiled output string *)
val transpiled_output : string -> json_output

(** Creates a successful JSON output with the resulting AST in Yojson format *)
val ast_output : (string * Lang.compilation_unit) list -> (string * EventGraph.event_graph_collection) list -> json_error list -> json_output

(** Creates a failed JSON output using the list of JSON errors *)
val failure_output : json_error list -> json_output
