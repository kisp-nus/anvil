open Lang

type event_graph = EventGraph.event_graph
type proc_graph = EventGraph.proc_graph  (* Ensure proc_graph is defined in EventGraph *)
type event_graph_collection = EventGraph.event_graph_collection

module Format = CodegenFormat

type port_def = CodegenPort.t

let codegen_endpoints printer (graphs : event_graph_collection) (g : event_graph) =
  let print_port_signal_decl = fun (port : port_def) ->
    if port.dtype <> Lang.unit_dtype then
    Printf.sprintf "%s %s;" (Format.format_dtype graphs.typedefs graphs.macro_defs port.dtype) (port.name) |>
      CodegenPrinter.print_line printer
  in
  List.filter (fun (p : endpoint_def) -> p.dir = Left) g.messages.endpoints |>
  CodegenPort.gather_ports graphs.channel_classes |>
  List.iter print_port_signal_decl

let verification_codegen_ports printer (graphs : event_graph_collection)
                      (endpoints : endpoint_def list) =
  let port_list = CodegenPort.gather_ports graphs.channel_classes endpoints in
  let rec print_port_list port_list =
  match port_list with
  | [] -> ()
  | port :: [] ->
      CodegenPort.assertformat graphs.typedefs graphs.macro_defs port |> Printf.sprintf "%s;" |> CodegenPrinter.print_line printer
  | port :: port_list' ->
      CodegenPort.assertformat graphs.typedefs graphs.macro_defs port |> Printf.sprintf "%s;" |> CodegenPrinter.print_line printer;
      print_port_list port_list'
  in
  print_port_list port_list;
  port_list

let verification_codegen_instantiations printer (graphs : event_graph_collection)
                      (endpoints : endpoint_def list) =
  let port_list = CodegenPort.gather_ports graphs.channel_classes endpoints in
  CodegenPrinter.print_line printer ".clk_i(clk_i),";
  CodegenPrinter.print_line printer ".rst_ni(rst_ni),";
  let rec print_port_list port_list =
  match port_list with
  | [] -> ()
  | port :: [] ->
    let s = CodegenPort.instanformat port in 
    Printf.sprintf ".%s (%s)" s s |> CodegenPrinter.print_line printer
  | port :: port_list' ->
    let s = CodegenPort.instanformat port in 
    Printf.sprintf ".%s (%s)," s s |> CodegenPrinter.print_line printer;
    print_port_list port_list'
  in
  print_port_list port_list;
  port_list

let verification_codegen_proc printer (graphs : EventGraph.event_graph_collection) (g : proc_graph) =
  let rec find_endpoint_message count endpoints =
    match endpoints with
    | [] -> None
    | (ep, num_msgs) :: rest ->
        if count < num_msgs then
          Some (ep, count)
        else
          find_endpoint_message (count - num_msgs) rest
  in

  let endpoint_counts =
    List.map (fun (ep : Lang.endpoint_def) ->
      let cc =
        MessageCollection.lookup_channel_class graphs.channel_classes ep.channel_class
        |> Option.get
      in
      (ep, List.length cc.messages)
    ) g.messages.args
  in

  (* To collect the total number of messages in each endpoint *)
  let num_messages (ep : Lang.endpoint_def) =
    let cc =
      Option.get (MessageCollection.lookup_channel_class graphs.channel_classes ep.channel_class)
    in
    List.length cc.messages
  in
  let counts = List.map num_messages g.messages.args in
  let total_count = List.fold_left ( + ) 0 counts in

  (* Print Top Module *)
  (* Print the input, output ports *)
  let name_assert = ExternName.user_sv() ^ "_assert" in
  Printf.sprintf "module %s (\n);" name_assert |> CodegenPrinter.print_line printer ~lvl_delta_post:1;

  (* Print the clock and reset declaration *)
  Printf.sprintf "logic clk_i = 0;" |> CodegenPrinter.print_line printer;
  Printf.sprintf "logic rst_ni;" |> CodegenPrinter.print_line printer;

  (* Loop to print the instantiation for each channel modules *)
  for count = 0 to total_count-1 do
    let s = ExternName.user_sv() ^ "_assert_" ^ string_of_int count in
    let c = "ch_" ^ string_of_int count in
    Printf.sprintf "%s %s (" s c |> CodegenPrinter.print_line printer;
    Printf.sprintf ".clk_i(clk_i)," |> CodegenPrinter.print_line printer;
    Printf.sprintf ".rst_ni(rst_ni)" |> CodegenPrinter.print_line printer;
    CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) ~lvl_delta_post:1 ");";
  done;

  (* Print the block to drive the clock *)
  Printf.sprintf "initial forever #5 clk_i = ~clk_i;" |> CodegenPrinter.print_line printer;

  let clock_block : string = 
  String.concat "\n" [
    Printf.sprintf "initial begin";
    Printf.sprintf "#0; rst_ni = 0;";
    Printf.sprintf "#10 rst_ni=1;";
    Printf.sprintf "#500;";
    Printf.sprintf "$finish;";
    Printf.sprintf "end";
  ]
  in
  let block = clock_block in
  block
  |> String.split_on_char '\n'
  |> List.iter (fun line ->
      CodegenPrinter.print_line printer line ~lvl_delta_post:0
  );

  CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) "endmodule \n";
  
  (* Start Looping to Print the Wrapper *)
  for count = 0 to total_count-1 do
  match find_endpoint_message count endpoint_counts with
  | Some (ep, local_idx) ->

    (* Print the input, output ports *)
    let name_assert = ExternName.user_sv() ^ "_assert_" ^ string_of_int count in
    Printf.sprintf "module %s \n(" name_assert |> CodegenPrinter.print_line printer ~lvl_delta_post:1;

    (* Print the clock and reset declaration *)
    Printf.sprintf "input logic clk_i," |> CodegenPrinter.print_line printer;
    Printf.sprintf "input logic rst_ni" |> CodegenPrinter.print_line printer;
    (* Printf.sprintf "Number of endpoints: %d\n" (List.length g.messages.args) |> CodegenPrinter.print_line printer; *)

    Printf.sprintf ");\n" |> CodegenPrinter.print_line printer;
    (* Check the lifetime contract *)
    let number_of_lifetime_cycles (d : Lang.delay_pat_chan_local) : int option =
      match d with
      | `Cycles n -> Some n
      | _ -> None
    in

    (* Print the lifetime contract *)
    let print_declared_cycle_bound count (ep : Lang.endpoint_def) =
      let cc = MessageCollection.lookup_channel_class graphs.channel_classes ep.channel_class |> Option.get in
      if count < List.length cc.messages then
        let msg_def = List.nth cc.messages count in
        let msg_def = ParamConcretise.concretise_message cc.params ep.channel_params msg_def in
          match msg_def.sig_types with
          | [] ->
            failwith (Printf.sprintf "Cannot emit lifetime bound parameter for endpoint %s: message has no signal types." ep.name)
          | stype0 :: _ ->
            match number_of_lifetime_cycles stype0.lifetime.e with
            | Some n -> Printf.sprintf "parameter int N = %d;" n |> CodegenPrinter.print_line printer
            | None -> failwith (Printf.sprintf "Cannot emit lifetime bound parameter for endpoint %s: lifetime bound is not statically determinable." ep.name)
      else ()
    in 
    print_declared_cycle_bound local_idx ep;

    (* Match synchronisation and module direction *)  
    let assertion_declare_states (msg : message_def) (d : Lang.endpoint_direction) : string =
      match (msg.send_sync, msg.recv_sync) with
      | (Dynamic, Dynamic) ->
        (match d with
        | Left  -> "typedef enum logic [1:0] {WAIT_REQ, WAIT_ACK, DROP_VALID} state_t;"
        | Right -> "typedef enum logic [1:0] {WAIT_ACK, DROP_VALID} state_t;")
      | (_, Dynamic) -> "typedef enum logic [1:0] {WAIT_ACK, DROP_ACK} state_t;"
      | (Dynamic, _) -> "typedef enum logic [1:0] {WAIT_REQ, DROP_VALID} state_t;"
      | _ -> failwith (Printf.sprintf "Cannot emit timing contracts for endpoint %s: timing contracts are unsupported for this combination" ep.name)
    in

    (* Print declaration of the FSM states *)
    let print_declaration_FSM count (ep : Lang.endpoint_def) =
      let cc = MessageCollection.lookup_channel_class graphs.channel_classes ep.channel_class |> Option.get in
      if count < List.length cc.messages then
        let msg_def = List.nth cc.messages count in
        let msg_def = ParamConcretise.concretise_message cc.params ep.channel_params msg_def in
          CodegenPrinter.print_line printer (assertion_declare_states msg_def ep.dir)
      else ()
    in 
    print_declaration_FSM local_idx ep;

    (* Print the shadow logics *)
    Printf.sprintf "state_t state_prev, state_curr, state_next;" |> CodegenPrinter.print_line printer;
    Printf.sprintf "int counter = 0;" |> CodegenPrinter.print_line printer;

    (* Print the declarations of ports, ack, valid, data ... *)
    let _ = verification_codegen_ports printer graphs g.messages.args in
      let initEvents = fst @@ List.hd g.threads in
      codegen_endpoints printer graphs initEvents;

    (* Retreive the user module name *)
    let s = ExternName.user_sv () in
    (* Print user module instantiation *)
    Printf.sprintf "%s user_sv (" s |> CodegenPrinter.print_line printer;
    let _ = verification_codegen_instantiations printer graphs g.messages.args in
    CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) ~lvl_delta_post:1 ");";

    (* Print anvil module instantiation *)
    Printf.sprintf "%s anvil_sv (" g.name |> CodegenPrinter.print_line printer;
    let _ = verification_codegen_instantiations printer graphs g.messages.args in
    CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) ~lvl_delta_post:1 ");";

    let valid_name =
      match CodegenPort.valid_port_names graphs.channel_classes ep local_idx with
      | Some s -> s
      | None -> failwith "Expected valid signal for this message, but none exists"
    in
    let ack_name =
      match CodegenPort.ack_port_names graphs.channel_classes ep local_idx with
      | Some s -> s
      | None -> failwith "Expected ack signal for this message, but none exists"
    in
    let data_names = CodegenPort.data_port_names graphs.channel_classes ep local_idx in
    let datas =
      match data_names with
      | [] -> ""
      | [d] -> d
      | _ -> failwith "Verification assertions currently support only one non-unit data signal per message"
    in

    (* Fixed FSM always_ff block *)
    let ff_block (state1 : string) (state2 : string): string =
    String.concat "\n" [
      Printf.sprintf "always_ff @(posedge clk_i or negedge rst_ni) begin";
      Printf.sprintf "  if (!rst_ni) begin";
      Printf.sprintf "    state_curr <= %s;" state1;
      Printf.sprintf "    counter <= 0;";
      Printf.sprintf "  end else begin";
      Printf.sprintf "    state_prev <= state_curr;";
      Printf.sprintf "    state_curr <= state_next;";
      Printf.sprintf "";
      Printf.sprintf "    if (state_curr == %s || counter != 0) begin" state2;
      Printf.sprintf "      counter <= counter + 1;";
      Printf.sprintf "    end";
      Printf.sprintf "";
      Printf.sprintf "    if (counter == N-1) begin";
      Printf.sprintf "      counter <= 0;";
      Printf.sprintf "    end";
      Printf.sprintf "  end";
      Printf.sprintf "end";
    ]
    in

    (* FSM always_comb block for sender *)
    let comb_block_nosyn_sender (valid_name : string) (ack_name : string) : string =
    String.concat "\n" [
      Printf.sprintf "always_comb begin";
      Printf.sprintf "  case (state_curr)";
      Printf.sprintf "";
      Printf.sprintf "    WAIT_REQ: begin";
      Printf.sprintf "      if (%s) begin" valid_name;
      Printf.sprintf "        state_next = WAIT_ACK;";
      Printf.sprintf "      end else begin";
      Printf.sprintf "        state_next = WAIT_REQ;";
      Printf.sprintf "      end";
      Printf.sprintf "    end";
      Printf.sprintf "";
      Printf.sprintf "    WAIT_ACK: begin";
      Printf.sprintf "      if (%s) begin" ack_name;
      Printf.sprintf "        state_next = DROP_VALID;";
      Printf.sprintf "      end else begin";
      Printf.sprintf "        state_next = WAIT_ACK;";
      Printf.sprintf "      end";
      Printf.sprintf "    end";
      Printf.sprintf "";
      Printf.sprintf "    DROP_VALID: begin";
      Printf.sprintf "      state_next = WAIT_REQ;";
      Printf.sprintf "    end";
      Printf.sprintf "";
      Printf.sprintf "    default: state_next = WAIT_REQ;";
      Printf.sprintf "  endcase";
      Printf.sprintf "end";
    ]
    in

    (* FSM always_comb block for receiver *)
    let comb_block_nosyn_receiver (ack_name : string) : string =
    String.concat "\n" [
      Printf.sprintf "always_comb begin";
      Printf.sprintf "  case (state_curr)";
      Printf.sprintf "";

      Printf.sprintf "    WAIT_ACK: begin";
      Printf.sprintf "      if (%s) begin" ack_name;
      Printf.sprintf "        state_next = DROP_VALID;";
      Printf.sprintf "      end else begin";
      Printf.sprintf "        state_next = WAIT_ACK;";
      Printf.sprintf "      end";
      Printf.sprintf "    end";
      Printf.sprintf "";

      Printf.sprintf "    DROP_VALID: begin";
      Printf.sprintf "      state_next = WAIT_ACK;";
      Printf.sprintf "    end";
      Printf.sprintf "";

      Printf.sprintf "    default: state_next = WAIT_ACK;";

      Printf.sprintf "  endcase";
      Printf.sprintf "end";
    ]
    in

    let comb_block_syn (state1 : string) (state2 : string) (signal : string) : string =
    String.concat "\n" [
      Printf.sprintf "always_comb begin";
      Printf.sprintf "  case (state_curr)";
      Printf.sprintf "";

      Printf.sprintf "    %s: begin" state1;
      Printf.sprintf "      if (%s) begin" signal;
      Printf.sprintf "        state_next = %s;" state2;
      Printf.sprintf "      end else begin";
      Printf.sprintf "        state_next = %s;" state1;
      Printf.sprintf "      end";
      Printf.sprintf "    end";
      Printf.sprintf "";

      Printf.sprintf "    %s: begin" state2;
      Printf.sprintf "      state_next = %s;" state1;
      Printf.sprintf "    end";
      Printf.sprintf "";

      Printf.sprintf "    default: state_next = %s;" state1;

      Printf.sprintf "  endcase";
      Printf.sprintf "end";
    ]
    in
  
    (* match the synchronisation and direction to print the correct fsm and assertion properties *)
    let match_fsm_assertion (msg : message_def) (d : Lang.endpoint_direction) (valid_name : string) (ack_name : string) (data : string): string =
      match (msg.send_sync, msg.recv_sync) with
      | (Dynamic, Dynamic) -> 
        (match d with
        | Left  -> 
          let block = ff_block "WAIT_REQ" "DROP_VALID" in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          
          let block = comb_block_nosyn_sender valid_name ack_name in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          String.concat "\n" [
            Printf.sprintf "`include \"nosyn_user_sender.svh\"\n";
            Printf.sprintf "assert property (data_stable_valid_high(%s, state_curr, %s))" valid_name data;
            Printf.sprintf "else $error(\"Assertion Failed: data_stable_valid_high\");\n";
            Printf.sprintf "assert property (valid_high_until_ack_high(%s, state_curr, %s))" valid_name ack_name;
            Printf.sprintf "else $error(\"Assertion Failed: valid_high_until_ack_high\");\n";
            Printf.sprintf "assert property (ack_high_valid_low(%s, state_curr, %s))" valid_name ack_name;
            Printf.sprintf "else $error(\"Assertion Failed: ack_high_valid_low\");\n";
            Printf.sprintf "assert property (data_stable_N_cycles_after_ack_high(state_curr, counter, %s))" data;
            Printf.sprintf "else $error(\"Assertion Failed: data_stable_N_cycles_after_ack_high\");\n";
            Printf.sprintf "assert property (wait_req_ack_low(%s, %s))" valid_name ack_name;
            Printf.sprintf "else $error(\"Assertion Failed: wait_req_ack_low\");\n";
          ]
        | Right -> 
          let block = ff_block "WAIT_ACK" "DROP_VALID" in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          
          let block = comb_block_nosyn_receiver ack_name in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          String.concat "\n" [
            Printf.sprintf "`include \"nosyn_user_receiver.svh\"\n";
            Printf.sprintf "assert property (ack_low_when_valid_high(state_curr, %s, %s, state_prev))" valid_name ack_name;
            Printf.sprintf "else $error(\"Assertion Failed: ack_low_when_valid_high\");\n";
            Printf.sprintf "assert property (ack_low_after_handshake(state_prev, %s))" ack_name;
            Printf.sprintf "else $error(\"Assertion Failed: ack_low_after_handshake\");\n";
          ] )
      | (_, Dynamic) -> 
        (match d with 
        | Left ->
          let block = ff_block "WAIT_ACK" "DROP_ACK" in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );

          let block = comb_block_syn "WAIT_ACK" "DROP_ACK" ack_name in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          String.concat "\n" [
            Printf.sprintf "`include \"syn_recv_dynamic_user_sender.svh\"\n";
            Printf.sprintf "assert property (data_stable_N_cycles (state_curr, counter, %s))" data;
            Printf.sprintf "else $error(\"Assertion Failed: data_stable_N_cycles\");\n";
          ]
        | Right -> 
          let block = ff_block "WAIT_ACK" "DROP_ACK" in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );

          let block = comb_block_syn "WAIT_ACK" "DROP_ACK" ack_name in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          String.concat "\n" [
            Printf.sprintf "`include \"syn_recv_dynamic_user_receiver.svh\"\n";
            Printf.sprintf "assert property (ack_low_during_DROP_ACK (state_curr, %s))" ack_name;
            Printf.sprintf "else $error(\"Assertion Failed: ack_low_during_DROP_ACK\");\n";
            Printf.sprintf "assert property (data_stable_N_cycles (state_curr, counter, %s))" data;
            Printf.sprintf "else $error(\"Assertion Failed: data_stable_N_cycles\");\n";
          ])
      | (Dynamic, _) -> 
        (match d with
        | Left ->
          let block = ff_block "WAIT_REQ" "DROP_VALID" in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );

          let block = comb_block_syn "WAIT_REQ" "DROP_VALID" valid_name in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          String.concat "\n" [
            Printf.sprintf "`include \"syn_send_dynamic_user_sender.svh\"\n";
            Printf.sprintf "assert property (data_stable_N_cycles (state_curr, counter, %s))" data;
            Printf.sprintf "else $error(\"Assertion Failed: data_stable_N_cycles\");\n";
            Printf.sprintf "assert property (valid_low_during_DROP_VALID (state_curr, %s))" valid_name;
            Printf.sprintf "else $error(\"Assertion Failed: valid_low_during_DROP_VALID\");\n";
          ]
        | Right -> 
          let block = ff_block "WAIT_REQ" "DROP_VALID" in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );

          let block = comb_block_syn "WAIT_REQ" "DROP_VALID" valid_name in
          block
          |> String.split_on_char '\n'
          |> List.iter (fun line ->
              CodegenPrinter.print_line printer line ~lvl_delta_post:0
          );
          String.concat "\n" [
            Printf.sprintf "`include \"syn_send_dynamic_user_receiver.svh\"\n";
            Printf.sprintf "assert property (data_stable_N_cycles (state_curr, counter, %s))" data;
            Printf.sprintf "else $error(\"Assertion Failed: data_stable_N_cycles\");\n";
          ])
      | _ -> failwith (Printf.sprintf "Cannot emit assertions for endpoint %s: unsupported timing-contract combination" ep.name)
    in
    
    (* Print the Assertions *)
    let print_fsm_assertion count (ep : Lang.endpoint_def) =
      let cc = MessageCollection.lookup_channel_class graphs.channel_classes ep.channel_class |> Option.get in
      if count < List.length cc.messages then
        let msg_def = List.nth cc.messages count in
        let msg_def = ParamConcretise.concretise_message cc.params ep.channel_params msg_def in
          CodegenPrinter.print_line printer (match_fsm_assertion msg_def ep.dir valid_name ack_name datas)
      else ()
    in 
    print_fsm_assertion local_idx ep;

  CodegenPrinter.print_line printer ~lvl_delta_pre:(-1) "endmodule"

  | None ->
      ()
  done

let verification_generate_preamble out =
  [
    "/*verilator lint_off DECLFILENAME*/"
  ] |> List.iter (Printf.fprintf out "%s\n")

let verification_generate_extern_import out file_name =
  In_channel.with_open_text file_name
    (fun in_channel ->
      let eof = ref false in
      while not !eof do
        match In_channel.input_line in_channel with
        | Some line ->
          Out_channel.output_string out line;
          Out_channel.output_char out '\n'
        | None -> eof := true
      done
    )

let verification_generate (out : out_channel)
             (config : Config.compile_config)
             (graphs : EventGraph.event_graph_collection) : unit =
  if config.verbose then (
    Printf.eprintf "==== CodeGen Details ====\n";
    List.iter (fun (pg : proc_graph) ->
      List.iter (fun (g, _)  ->
        EventGraphOps.print_dot_graph g Out_channel.stderr
      ) pg.threads
    ) graphs.event_graphs;
  );
  let printer = CodegenPrinter.create out 2 in
  List.iter (verification_codegen_proc printer graphs) graphs.event_graphs

