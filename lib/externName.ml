let user_sv_ref : string option ref = ref None

let generate (user_sv:string) : unit =
  user_sv_ref := Some user_sv

let strip_sv_name (path : string) : string =
  path |> Filename.basename |> Filename.remove_extension

let user_sv () : string =
  match !user_sv_ref with
  | Some s -> strip_sv_name s
  | None -> failwith "ExternName.user_sv: not initialised"

