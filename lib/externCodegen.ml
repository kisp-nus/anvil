open Lang

type event_graph = EventGraph.event_graph
type proc_graph = EventGraph.proc_graph
type event_graph_collection = EventGraph.event_graph_collection

module Format = CodegenFormat

type port_def = CodegenPort.t

type spawn_binding = {
  child_ep : endpoint_def;
  parent_ep : identifier;
}

type spawned_proc = {
  spawn_idx : int;
  module_name : identifier;
  proc_graph : proc_graph;
  bindings : spawn_binding list;
}

type channel_owner = {
  child_ep : endpoint_def;
}

type channel_pair = {
  channel_idx : int;
  channel_def : channel_def;
  left_owner : channel_owner;
}

type data_binding = {
  data_name : string;
  data_width : int;
  data_port_index : int option;
  test_inst : string;
}

type tb_entry = {
  pair : channel_pair;
  msg : message_def;
  kind : string;
  ctrl_name : string option;
  data_bindings : data_binding list;
  tb_mod : string;
  lifetime : int option;
  static_interval : int option;
}

let codegen_ports printer (graphs : event_graph_collection)
    (endpoints : endpoint_def list) (is_mod_comb : bool) =

  let port_list =
    CodegenPort.gather_ports graphs.channel_classes endpoints
  in

  let rec print_port_list = function
    | [] -> ()
    | [port] ->
        CodegenPort.format graphs.typedefs graphs.macro_defs port
        |> CodegenPrinter.print_line printer
    | port :: rest ->
        CodegenPort.format graphs.typedefs graphs.macro_defs port
        |> Printf.sprintf "%s,"
        |> CodegenPrinter.print_line printer;
        print_port_list rest
  in

  if is_mod_comb then
    print_port_list port_list
  else
    print_port_list ([CodegenPort.clk; CodegenPort.rst] @ port_list);

  port_list


let codegen_dut_and_ultimate_wrapper printer
    (graphs : event_graph_collection) =

  (* ============================================================
     Small helpers
     ============================================================ *)

  let trailing_index (name : string) : int option =
    match String.rindex_opt name '_' with
    | None -> None
    | Some i ->
        let s = String.sub name (i + 1) (String.length name - i - 1) in
        if s = "" then None
        else
          try Some (int_of_string s)
          with Failure _ -> None
  in

  let infer_width_from_dtype (dtype : data_type) : int =
    match dtype with
    | `Logic -> 1
    | `Array (`Logic, ParamEnv.Concrete w) -> w
    | _ -> 32
  in

  let print_csv_lines lines =
    let rec go = function
      | [] -> ()
      | [x] -> CodegenPrinter.print_line printer x
      | x :: xs ->
          CodegenPrinter.print_line printer (x ^ ",");
          go xs
    in
    go lines
  in

  let find_proc_graph (name : string) : proc_graph =
    match CodegenHelpers.lookup_proc graphs.external_event_graphs name with
    | Some g -> g
    | None ->
        begin match CodegenHelpers.lookup_proc graphs.event_graphs name with
        | Some g -> g
        | None ->
            failwith
              (Printf.sprintf "Cannot find spawned process '%s'" name)
        end
  in

  let proc_is_comb (g : proc_graph) : bool =
    let is_not_extern =
      match g.extern_module, g.proc_body with
      | None, _ -> true
      | Some _, Lang.Extern (_, body) -> List.is_empty body.named_ports
      | _ -> true
    in
    List.for_all (fun (thread, _) -> (thread : event_graph).comb) g.threads
    && is_not_extern
  in

  (* ============================================================
     Message timing
     ============================================================ *)

  let lifetime_of_msg (m : message_def) : int option =
    match m.sig_types with
    | stype :: _ ->
        begin match stype.lifetime.e with
        | `Cycles n -> Some n
        | _ -> None
        end
    | [] -> None
  in

  let static_interval_of_msg (m : message_def) : int option =
    match m.send_sync, m.recv_sync with
    | Static (_, interval), Dynamic -> Some interval
    | Dynamic, Static (_, interval) -> Some interval
    | Static (_, interval), Static _ -> Some interval
    | _ -> None
  in

  let classify_tb_by_sync (m : message_def) : string =
    match m.send_sync, m.recv_sync with
    | Dynamic, Dynamic -> "tb1"
    | _, Dynamic -> "tb2"
    | Dynamic, _ -> "tb3"
    | _ -> "tb4"
  in

  let ctrl_if_type = function
    | "tb1" -> "tb1_control_if"
    | "tb2" -> "tb2_control_if"
    | "tb3" -> "tb3_control_if"
    | kind -> failwith (Printf.sprintf "No control interface for %s" kind)
  in

  let ctrl_tb_port = function
    | "tb1" -> "tb1_control_if"
    | "tb2" -> "tb2_control_if"
    | "tb3" -> "tb3_control_if"
    | _ -> ""
  in

  let data_tb_port = function
    | "tb1" -> "tb1_data_if"
    | "tb2" -> "tb2_data_if"
    | "tb3" -> "tb3_data_if"
    | _ -> "tb4_data_if"
  in

  (* ============================================================
     Top process = .anvil filename.

     IMPORTANT:
       The top process is used as the integration description.
       Its local `chan ... -- ...` declarations and `spawn ...`
       statements decide which processes are actually instantiated.

       We DO NOT instantiate the generated top module itself inside
       dut_wrapper, otherwise all spawned processes would be duplicated.
     ============================================================ *)

  let top_name =
    match
      Array.to_list Sys.argv
      |> List.find_opt (fun arg -> Filename.check_suffix arg ".anvil")
    with
    | Some file ->
        let base = Filename.basename file in
        String.sub base 0 (String.length base - String.length ".anvil")
    | None -> "top"
  in

  let top_graph =
    match CodegenHelpers.lookup_proc graphs.event_graphs top_name with
    | Some g -> g
    | None ->
        failwith
          (Printf.sprintf
             "Cannot find top process '%s'. The top process must match the .anvil filename."
             top_name)
  in

  let top_channels =
    match top_graph.proc_body with
    | Lang.Native body -> List.map (fun n -> n.d) body.channels
    | Lang.Extern _ ->
        failwith
          (Printf.sprintf
             "Top process '%s' cannot be extern for -sv-extern integration"
             top_name)
  in

  (* ============================================================
     Resolve ONLY the processes actually spawned by the top.

     A process definition that exists in the file but is never spawned
     here is intentionally ignored.

     For each spawn, pair:
       child process argument endpoint <-> top-local endpoint

     This is the same positional relationship used by normal Anvil
     spawn code generation.
     ============================================================ *)

  let spawned_procs =
    top_graph.spawns
    |> List.mapi
        (fun spawn_idx
              (module_name, (spawn_node : spawn_def ast_node)) ->

          let pg =
            find_proc_graph module_name
          in

          let parent_eps =
            Lang.preprocess_ep_spawn_args
              spawn_node.d.params
          in

          let child_eps =
            pg.messages.args
          in

          if List.length child_eps <> List.length parent_eps then
            failwith
              (Printf.sprintf
                  "Invalid number of endpoint arguments for spawn of %s | expected %d, got %d"
                  module_name
                  (List.length child_eps)
                  (List.length parent_eps));

          let bindings =
            List.map2
              (fun child_ep parent_ep ->
                {child_ep; parent_ep})
              child_eps
              parent_eps
          in

          {
            spawn_idx;
            module_name;
            proc_graph = pg;
            bindings;
          })
  in

  let owners_of_endpoint (parent_ep : string) : channel_owner list =
  spawned_procs
  |> List.filter_map (fun spawned ->
       match List.find_opt (fun b -> b.parent_ep = parent_ep) spawned.bindings with
       | None -> None
       | Some b -> Some {child_ep = b.child_ep})
  in

  let one_owner endpoint_name =
    match owners_of_endpoint endpoint_name with
    | [] -> None
    | [owner] -> Some owner
    | _ ->
        failwith
          (Printf.sprintf
             "Endpoint '%s' is passed to more than one spawned process"
             endpoint_name)
  in

  (* ============================================================
     Active channel instances.

     Multiple channels are supported independently:

       chan a_le -- a_ri : ch1;
       chan b_le -- b_ri : ch2;

       spawn A(a_le); spawn B(a_ri);
       spawn C(b_le); spawn D(b_ri);

     gives:
       channel 0 -> A <-> B
       channel 1 -> C <-> D

     If neither side is spawned, the channel is unused and ignored.
     If exactly one side is spawned, this wrapper cannot reproduce the
     top-level connection safely, so fail instead of generating a wrong
     circuit.
     ============================================================ *)

  let channel_pairs =
    top_channels
    |> List.mapi (fun channel_idx channel_def ->
         let left = one_owner channel_def.endpoint_left in
         let right = one_owner channel_def.endpoint_right in

         match left, right with
         | None, None -> None
         | Some left_owner, Some _right_owner ->
              if channel_def.n_instances <> None then
                failwith
                  (Printf.sprintf
                    "Arrayed channel '%s -- %s' is not yet supported by the verification wrapper"
                    channel_def.endpoint_left
                    channel_def.endpoint_right);

              Some {channel_idx; channel_def; left_owner}
         | Some _, None ->
             failwith
               (Printf.sprintf
                  "Channel '%s -- %s' has a spawned process on '%s' but not on '%s'"
                  channel_def.endpoint_left
                  channel_def.endpoint_right
                  channel_def.endpoint_left
                  channel_def.endpoint_right)
         | None, Some _ ->
             failwith
               (Printf.sprintf
                  "Channel '%s -- %s' has a spawned process on '%s' but not on '%s'"
                  channel_def.endpoint_left
                  channel_def.endpoint_right
                  channel_def.endpoint_right
                  channel_def.endpoint_left))
    |> List.filter_map (fun x -> x)
  in

  if channel_pairs = [] then
    ()
  else (

    (* ============================================================
       Counters
       ============================================================ *)

    let tb1_n = ref 0
    and tb2_n = ref 0
    and tb3_n = ref 0
    and data_n = ref 0
    and test_n = ref 1
    in

    let next_ctrl_name kind =
      match kind with
      | "tb1" ->
          let i = !tb1_n in
          tb1_n := i + 1;
          Some (Printf.sprintf "tb1_control_if_%d" i)
      | "tb2" ->
          let i = !tb2_n in
          tb2_n := i + 1;
          Some (Printf.sprintf "tb2_control_if_%d" i)
      | "tb3" ->
          let i = !tb3_n in
          tb3_n := i + 1;
          Some (Printf.sprintf "tb3_control_if_%d" i)
      | "tb4" -> None
      | _ -> failwith (Printf.sprintf "Unknown TB kind: %s" kind)
    in

    let next_data_binding (port : port_def) =
      let di = !data_n in
      data_n := di + 1;
      let ti = !test_n in
      test_n := ti + 1;
      {
        data_name = Printf.sprintf "data%d" di;
        data_width = infer_width_from_dtype port.dtype;
        data_port_index = trailing_index port.name;
        test_inst = Printf.sprintf "test%d" ti;
      }
    in

    let dummy_data_binding () =
      let di = !data_n in
      data_n := di + 1;
      let ti = !test_n in
      test_n := ti + 1;
      {
        data_name = Printf.sprintf "data%d" di;
        data_width = 1;
        data_port_index = None;
        test_inst = Printf.sprintf "test%d" ti;
      }
    in

    (* ============================================================
       Build one TB entry for every message of every ACTIVE channel.

       One message -> one shared control interface.

       If the message carries multiple data values, each data value gets
       its own data_if and its own TB instance, while all those TBs share
       the SAME control interface for that message.
     ============================================================ *)

    let tb_entries =
      channel_pairs
      |> List.concat_map (fun pair ->
           let cc =
             match
               MessageCollection.lookup_channel_class
                 graphs.channel_classes
                 pair.channel_def.channel_class
             with
             | Some cc -> cc
             | None ->
                 failwith
                   (Printf.sprintf
                      "Cannot find channel class '%s'"
                      pair.channel_def.channel_class)
           in

           cc.messages
           |> List.map (fun raw_msg ->
                let msg =
                  ParamConcretise.concretise_message
                    cc.params
                    pair.channel_def.channel_params
                    raw_msg
                in

                let kind = classify_tb_by_sync msg in
                let ctrl_name = next_ctrl_name kind in

                (* Use one endpoint owner only to discover the generated
                   child-module data ports. Both endpoint owners have the
                   same channel payload shape. Match exact generated data
                   port names so similarly named messages cannot collide. *)
                let all_child_ports =
                  CodegenPort.gather_ports
                    graphs.channel_classes
                    [pair.left_owner.child_ep]
                in

                let data_ports =
                  msg.sig_types
                  |> List.mapi (fun data_idx stype -> (data_idx, stype))
                  |> List.filter_map (fun (data_idx, stype) ->
                       if stype.dtype = Lang.unit_dtype then
                         None
                       else
                         let port_name =
                           Format.format_msg_data_signal_name
                             pair.left_owner.child_ep.name
                             msg.name
                             data_idx
                         in
                         match
                           List.find_opt
                             (fun (p : port_def) -> p.name = port_name)
                             all_child_ports
                         with
                         | Some p -> Some p
                         | None ->
                             failwith
                               (Printf.sprintf
                                  "Cannot find generated data port '%s' for channel %s message %s"
                                  port_name
                                  pair.channel_def.channel_class
                                  msg.name))
                in

                let data_bindings =
                  match data_ports with
                  | [] -> [dummy_data_binding ()]
                  | ports -> List.map next_data_binding ports
                in

                {
                  pair;
                  msg;
                  kind;
                  ctrl_name;
                  data_bindings;
                  tb_mod = kind;
                  lifetime = lifetime_of_msg msg;
                  static_interval = static_interval_of_msg msg;
                }))
    in

    let entries_for_pair pair =
      List.filter (fun e -> e.pair.channel_idx = pair.channel_idx) tb_entries
    in

    let pair_of_parent_endpoint parent_ep =
      List.find_opt
        (fun pair ->
          pair.channel_def.endpoint_left = parent_ep
          || pair.channel_def.endpoint_right = parent_ep)
        channel_pairs
    in

    (* ============================================================
       dut_wrapper interface ports.

       Use the full interface (no .dut modport) because dut_wrapper now
       contains BOTH sides of the channel. One spawned process may drive
       valid/data while the other drives ack.
     ============================================================ *)

    CodegenPrinter.print_line printer "";
    CodegenPrinter.print_line printer (Printf.sprintf "module dut_wrapper_%s ("top_name)
  ~lvl_delta_post:1;
    let dut_ports =
      ["input logic clk_i"; "input logic rst_ni"]
      @ List.filter_map
          (fun e ->
            match e.ctrl_name with
            | None -> None
            | Some ctrl ->
                Some (Printf.sprintf "%s %s" (ctrl_if_type e.kind) ctrl))
          tb_entries
      @ List.concat_map
          (fun e ->
            List.map
              (fun d -> Printf.sprintf "data_if %s" d.data_name)
              e.data_bindings)
          tb_entries
    in

    print_csv_lines dut_ports;
    CodegenPrinter.print_line
      printer
      ");"
      ~lvl_delta_pre:(-1)
      ~lvl_delta_post:1;

    (* ============================================================
       Instantiate every process actually spawned by the TOP.

       Unspawned proc definitions are never instantiated.

       A spawned proc may own endpoints from more than one channel; all
       of its endpoint bindings are connected in this single instance.
     ============================================================ *)

    let connections_for_binding (binding : spawn_binding) =
      match pair_of_parent_endpoint binding.parent_ep with
      | None ->
          failwith
            (Printf.sprintf
               "Spawn endpoint '%s' is not part of an active top-level channel"
               binding.parent_ep)
      | Some pair ->
          entries_for_pair pair
          |> List.concat_map (fun e ->
               let ctrl_conns =
                 match e.ctrl_name with
                 | None -> []
                 | Some ctrl ->
                     (if CodegenPort.message_has_valid_port e.msg then
                        [Printf.sprintf
                           ".%s(%s.valid)"
                           (Format.format_msg_valid_signal_name
                              binding.child_ep.name e.msg.name)
                           ctrl]
                      else [])
                     @
                     (if CodegenPort.message_has_ack_port e.msg then
                        [Printf.sprintf
                           ".%s(%s.ack)"
                           (Format.format_msg_ack_signal_name
                              binding.child_ep.name e.msg.name)
                           ctrl]
                      else [])
               in

               let data_conns =
                 e.data_bindings
                 |> List.filter_map (fun d ->
                      match d.data_port_index with
                      | None -> None
                      | Some data_idx ->
                          Some
                            (Printf.sprintf
                               ".%s(%s.data)"
                               (Format.format_msg_data_signal_name
                                  binding.child_ep.name e.msg.name data_idx)
                               d.data_name))
               in

               ctrl_conns @ data_conns)
    in

    List.iter
      (fun spawned ->
        let conns =
          (if proc_is_comb spawned.proc_graph then
             []
           else
             [".clk_i(clk_i)"; ".rst_ni(rst_ni)"])
          @ List.concat_map connections_for_binding spawned.bindings
        in

        CodegenPrinter.print_line
          printer
          (Printf.sprintf
             "%s _spawn_%d ("
             spawned.module_name
             spawned.spawn_idx)
          ~lvl_delta_post:1;

        print_csv_lines conns;

        CodegenPrinter.print_line
          printer
          ");"
          ~lvl_delta_pre:(-1))
      spawned_procs;

    CodegenPrinter.print_line
      printer
      "endmodule"
      ~lvl_delta_pre:(-1);

    (* ============================================================
       <top_module>_ultimate_wrapper
       ============================================================ *)

    CodegenPrinter.print_line printer "";
    CodegenPrinter.print_line
      printer
      (Printf.sprintf "module %s_ultimate_wrapper;" top_name)
      ~lvl_delta_post:1;

    CodegenPrinter.print_line printer "logic clk_i;";
    CodegenPrinter.print_line printer "logic rst_ni;";
    CodegenPrinter.print_line printer "initial clk_i = 1'b0;";
    CodegenPrinter.print_line printer "initial forever #5 clk_i = ~clk_i;";

    CodegenPrinter.print_line printer "initial begin";
    CodegenPrinter.print_line printer "  rst_ni = 1'b0;";
    CodegenPrinter.print_line printer "  #20 rst_ni = 1'b1;";
    CodegenPrinter.print_line printer "  #500;";
    CodegenPrinter.print_line printer "  $finish;";
    CodegenPrinter.print_line printer "end";

    (* ============================================================
       Interface instances
       ============================================================ *)

    List.iter
      (fun e ->
        match e.ctrl_name with
        | None -> ()
        | Some ctrl ->
            CodegenPrinter.print_line
              printer
              (Printf.sprintf "%s %s();" (ctrl_if_type e.kind) ctrl))
      tb_entries;

    List.iter
      (fun e ->
        List.iter
          (fun d ->
            CodegenPrinter.print_line
              printer
              (Printf.sprintf
                 "data_if #(%d) %s();"
                 d.data_width
                 d.data_name))
          e.data_bindings)
      tb_entries;

    (* ============================================================
       Instantiate dut_wrapper
       ============================================================ *)

    CodegenPrinter.print_line printer (Printf.sprintf "dut_wrapper_%s dut1 (" top_name) ~lvl_delta_post:1;

    let ext_dut_conns =
      [".clk_i(clk_i)"; ".rst_ni(rst_ni)"]
      @ List.filter_map
          (fun e ->
            match e.ctrl_name with
            | None -> None
            | Some ctrl -> Some (Printf.sprintf ".%s(%s)" ctrl ctrl))
          tb_entries
      @ List.concat_map
          (fun e ->
            List.map
              (fun d -> Printf.sprintf ".%s(%s)" d.data_name d.data_name)
              e.data_bindings)
          tb_entries
    in

    print_csv_lines ext_dut_conns;
    CodegenPrinter.print_line printer ");" ~lvl_delta_pre:(-1);

    (* ============================================================
       Instantiate verification TBs.

       One message has one control interface.
       If it carries N data values, instantiate N TB monitors that all
       share that same control interface, one monitor per data_if.
     ============================================================ *)

    List.iter
      (fun e ->
        let tb_params =
          match e.kind with
          | "tb1" ->
              begin match e.lifetime with
              | Some n -> Printf.sprintf "#(.lifetime(%d)) " n
              | None -> ""
              end
          | _ ->
              let lifetime = Option.value e.lifetime ~default:3 in
              let static_interval = Option.value e.static_interval ~default:2 in
              Printf.sprintf
                "#(.lifetime(%d), .static_interval(%d)) "
                lifetime
                static_interval
        in

        List.iter
          (fun d ->
            CodegenPrinter.print_line
              printer
              (Printf.sprintf "%s %s%s (" e.tb_mod tb_params d.test_inst)
              ~lvl_delta_post:1;

            let tb_conns =
              [".clk_i(clk_i)";
               ".rst_ni(rst_ni)";
               Printf.sprintf ".%s(%s)" (data_tb_port e.kind) d.data_name]
              @
              (match e.ctrl_name with
               | None -> []
               | Some ctrl ->
                   [Printf.sprintf ".%s(%s)" (ctrl_tb_port e.kind) ctrl])
            in

            print_csv_lines tb_conns;
            CodegenPrinter.print_line printer ");" ~lvl_delta_pre:(-1))
          e.data_bindings)
      tb_entries;

    CodegenPrinter.print_line
      printer
      "endmodule"
      ~lvl_delta_pre:(-1)
  )


let generate_extern_import out file_name =
  In_channel.with_open_text
    file_name
    (fun in_channel ->
      let eof = ref false in
      while not !eof do
        match In_channel.input_line in_channel with
        | Some line ->
            Out_channel.output_string out line;
            Out_channel.output_char out '\n'
        | None -> eof := true
      done)


let generate
    (out : out_channel)
    (config : Config.compile_config)
    (graphs : EventGraph.event_graph_collection)
    : unit =

  if config.verbose then (
    Printf.eprintf "==== CodeGen Details ====\n";

    List.iter
      (fun (pg : proc_graph) ->
        List.iter
          (fun (g, _) ->
            EventGraphOps.print_dot_graph g Out_channel.stderr)
          pg.threads)
      graphs.event_graphs
  );

  let printer = CodegenPrinter.create out 2 in
  codegen_dut_and_ultimate_wrapper printer graphs
