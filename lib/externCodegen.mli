type event_graph = EventGraph.event_graph
type proc_graph = EventGraph.proc_graph
type event_graph_collection = EventGraph.event_graph_collection
module Format = CodegenFormat
type port_def = CodegenPort.t
val codegen_ports :
  CodegenPrinter.t ->
  event_graph_collection ->
  Lang.endpoint_def list -> bool -> CodegenPort.t list
val codegen_dut_and_ultimate_wrapper :
  CodegenPrinter.t -> event_graph_collection -> unit
val generate_extern_import : out_channel -> string -> unit
val generate :
  out_channel ->
  Config.compile_config -> EventGraph.event_graph_collection -> unit
