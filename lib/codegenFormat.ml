open Lang

let sanitize_identifier (name : identifier) : identifier =
  let replace_char c =
    match c with
    | '[' | ']' -> "___"
    | c -> String.make 1 c
  in
  String.to_seq name |> Seq.map replace_char |> List.of_seq |> String.concat ""

let format_msg_prefix (endpoint_name : identifier) (message_name : identifier) : identifier =
  Printf.sprintf "_%s_%s" (sanitize_identifier endpoint_name) message_name

let format_msg_data_signal_name (endpoint_name : identifier) (message_name : identifier) (data_idx : int) : string =
  Printf.sprintf "_%s_%s_%d" (sanitize_identifier endpoint_name) message_name data_idx

let format_msg_valid_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_valid" (sanitize_identifier endpoint_name) message_name

let format_msg_ack_signal_name (endpoint_name : identifier) (message_name : identifier) : string =
  Printf.sprintf "_%s_%s_ack" (sanitize_identifier endpoint_name) message_name

let format_wirename (thread_id : int) (id : int) : string = Printf.sprintf "thread_%d_wire$%d" thread_id id

let format_dtype (typedefs : TypedefMap.t) (macro_defs : Lang.macro_def list) (dtype : data_type) =
  match dtype with
  | `Logic -> "logic[0:0]"
  | `Opaque typename -> typename
  | _ ->
    let size = (TypedefMap.data_type_size typedefs macro_defs dtype) in
    if size = 0 then
      raise (Failure "[Internal Error] Please report : Cannot format datatype with size 0")
    else
      Printf.sprintf "logic[%d:0]" @@ size - 1

let format_literal = function
  | Binary (len, b) -> Printf.sprintf "%d'b%s" len (List.map string_of_digit b |> List.rev |> String.concat "")
  | Decimal (len, d) -> Printf.sprintf "%d'd%s" len (List.map string_of_digit d |> List.rev |> String.concat "")
  | Hexadecimal (len, h) -> Printf.sprintf "%d'h%s" len (List.map string_of_digit h |> List.rev |> String.concat "")
  | WithLength (len, v) -> Printf.sprintf "%d'd%d" len v
  | NoLength n -> string_of_int n

let format_binop = string_of_binop
let format_unop = string_of_unop

let format_regname_current (regname : identifier) =
  Printf.sprintf "%s_q" regname
let format_regname_next (regname : identifier) =
  Printf.sprintf "%s_n" regname

let format_wire_maybe_const (v : WireCollection.wire MaybeConst.maybe_int_const) =
  let open MaybeConst in
  match v with
  | Const n -> Printf.sprintf "%d" n
  | NonConst w -> format_wirename w.thread_id w.id

module Endpoint = struct
  open EventGraph
  let extract_index (name : identifier) : string option =
    match String.index_opt name '[' with
    | Some idx_start -> Some (String.sub name idx_start (String.length name - idx_start))
    | None -> None

  let base_name (name : identifier) : identifier =
    match String.index_opt name '[' with
    | Some idx -> String.sub name 0 idx
    | None -> name

  let canonicalize (endpoint : endpoint_def) : identifier =
    match endpoint.dir with
    | Left -> endpoint.name
    | Right -> 
      let index_suffix = extract_index endpoint.name in
      let opp_base = Option.value ~default:(base_name endpoint.name) endpoint.opp in
      match index_suffix with
      | Some idx -> opp_base ^ idx
      | None -> opp_base

  let canonicalize_endpoint_name (endpoint_name : identifier) (g : event_graph) : identifier =
    let base = base_name endpoint_name in
    match MessageCollection.lookup_endpoint g.messages base with
    | Some endpoint_local -> 
        let index_suffix = extract_index endpoint_name in
        let canonicalized_base = canonicalize endpoint_local in
        (match index_suffix with
         | Some idx -> canonicalized_base ^ idx
         | None -> canonicalized_base)
    | None -> endpoint_name
end

let endpoint_base_name = Endpoint.base_name
let canonicalize_endpoint_name = Endpoint.canonicalize_endpoint_name
