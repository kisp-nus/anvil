val verification_generate_extern_import : out_channel -> string -> unit

val verification_generate : out_channel -> Config.compile_config -> EventGraph.event_graph_collection -> unit

val verification_generate_preamble : out_channel -> unit