(** AST-to-IR traversal: converts a single {!Lang.expr_node} into a
    {!EventGraph.lowering_data}, mutating [graph] with metadata for type checking and code generation *)
val construct_graphIR :
  EventGraph.event_graph ->
  EventGraph.cunit_info ->
  GraphBuildContext.Typing.build_context ->
  Lang.expr_node ->
  EventGraph.node_data
