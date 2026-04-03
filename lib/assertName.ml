let anvil_sv_ref : string option ref = ref None
let user_sv_ref : string option ref = ref None

let generate (anvil_sv:string) (user_sv:string) : unit =
  anvil_sv_ref := Some anvil_sv;
  user_sv_ref := Some user_sv

let strip_sv_name (path : string) : string =
  path |> Filename.basename |> Filename.remove_extension

let user_sv () : string =
  match !user_sv_ref with
  | Some s -> strip_sv_name s
  | None -> failwith "AssertName.sender: not initialised (call AssertName.generate first)"

let anvil_sv () : string =
  match !anvil_sv_ref with
  | Some s -> s
  | None -> failwith "AssertName.receiver: not initialised (call AssertName.generate first)"
