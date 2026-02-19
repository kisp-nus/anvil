open Lang

type t = {
  dir: message_direction;
  dtype: data_type;
  name: identifier;
}

let message_has_valid_port (msg : message_def) : bool = msg.send_sync = Dynamic
let message_has_ack_port (msg : message_def) : bool = msg.recv_sync = Dynamic

let gather_ports_from_endpoint (channel_classes : channel_class_def list) (endpoint : endpoint_def) : t list =
  let cc = Option.get (MessageCollection.lookup_channel_class channel_classes endpoint.channel_class) in
  let gen_endpoint_ports = fun (msg : message_def) ->
    let msg = ParamConcretise.concretise_message cc.params endpoint.channel_params msg in
    let folder_inner = fun fmt msg_dir (n, port_list) (stype : sig_type_chan_local) ->
      let new_port : t = {
        name = fmt endpoint.name msg.name n;
        dir = msg_dir;
        dtype = stype.dtype;
      } in (n + 1, new_port::port_list)
    in
    let msg_data_dir = get_message_direction msg.dir endpoint.dir in
    let (_, res) = List.fold_left (folder_inner CodegenFormat.format_msg_data_signal_name msg_data_dir) (0, []) msg.sig_types in
    let res =
      if message_has_valid_port msg then
        let valid_port = { name = CodegenFormat.format_msg_valid_signal_name endpoint.name msg.name; dir = msg_data_dir; dtype = `Logic} in
        valid_port::res
      else res
    in
    if message_has_ack_port msg then
      let ack_port = {name = CodegenFormat.format_msg_ack_signal_name endpoint.name msg.name; dir = reverse msg_data_dir; dtype = `Logic} in
      ack_port::res
    else res
  in List.concat_map gen_endpoint_ports cc.messages

let gather_ports (channel_classes : channel_class_def list) (endpoints : endpoint_def list) : t list =
  List.concat_map (gather_ports_from_endpoint channel_classes) endpoints

let clk = {dir = Inp; dtype = `Logic; name = "clk_i"}
let rst = {dir = Inp; dtype = `Logic; name = "rst_ni"}

let format (typedefs : TypedefMap.t) (macro_defs: macro_def list) port =
  let ep_name_formatted = CodegenFormat.sanitize_identifier port.name in
  let inout = match port.dir with
    | Inp -> "input"
    | Out -> "output"
  in Printf.sprintf "%s %s %s" inout (CodegenFormat.format_dtype typedefs macro_defs port.dtype) ep_name_formatted


let assertformat (typedefs : TypedefMap.t) (macro_defs: macro_def list) port =
  let ep_name_formatted = CodegenFormat.sanitize_identifier port.name in
  Printf.sprintf "%s %s" (CodegenFormat.format_dtype typedefs macro_defs port.dtype) ep_name_formatted

let instanformat port =
  let ep_name_formatted = CodegenFormat.sanitize_identifier port.name in
  Printf.sprintf "%s" ep_name_formatted

let valid_port_names_from_endpoint
    (channel_classes : channel_class_def list)
    (endpoint : endpoint_def)
  : identifier list =
  let cc =
    Option.get (MessageCollection.lookup_channel_class channel_classes endpoint.channel_class)
  in
  cc.messages
  |> List.filter (fun msg ->
       let msg = ParamConcretise.concretise_message cc.params endpoint.channel_params msg in
       message_has_valid_port msg
     )
  |> List.map (fun msg ->
       let msg = ParamConcretise.concretise_message cc.params endpoint.channel_params msg in
       CodegenFormat.format_msg_valid_signal_name endpoint.name msg.name
     )

let valid_port_names
    (channel_classes : channel_class_def list)
    (endpoints : endpoint_def list)
  : identifier list =
  List.concat_map (valid_port_names_from_endpoint channel_classes) endpoints

let ack_port_names_from_endpoint
    (channel_classes : channel_class_def list)
    (endpoint : endpoint_def)
  : identifier list =
  let cc =
    Option.get (MessageCollection.lookup_channel_class channel_classes endpoint.channel_class)
  in
  cc.messages
  |> List.filter (fun msg ->
       let msg = ParamConcretise.concretise_message cc.params endpoint.channel_params msg in
       message_has_ack_port msg
     )
  |> List.map (fun msg ->
       let msg = ParamConcretise.concretise_message cc.params endpoint.channel_params msg in
       CodegenFormat.format_msg_ack_signal_name endpoint.name msg.name
     )

let ack_port_names
    (channel_classes : channel_class_def list)
    (endpoints : endpoint_def list)
  : identifier list =
  List.concat_map (ack_port_names_from_endpoint channel_classes) endpoints

let data_port_names (channel_classes : channel_class_def list)
                    (endpoints : endpoint_def list)
  : identifier list =
  let data_names_from_endpoint (endpoint : endpoint_def) =
    let cc =
      Option.get (MessageCollection.lookup_channel_class channel_classes endpoint.channel_class)
    in
    List.concat_map (fun (msg : message_def) ->
      let msg = ParamConcretise.concretise_message cc.params endpoint.channel_params msg in
      List.mapi (fun i (_stype : sig_type_chan_local) ->
        CodegenFormat.format_msg_data_signal_name endpoint.name msg.name i
      ) msg.sig_types
    ) cc.messages
  in
  List.concat_map data_names_from_endpoint endpoints

