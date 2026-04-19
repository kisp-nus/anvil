open Lang

module DTypeCheck = struct
  let warn msg span file_name =
    Printf.eprintf "[Warning] %s\n" msg;
    SpanPrinter.print_code_span ~indent:2 ~trunc:(-5) stderr file_name span;
    flush stderr

  let type_error msg span =
    raise (Except.TypeError [Text msg; Except.codespan_local span])


  let get_size typedefs macro_defs dtype =
    try Some (TypedefMap.data_type_size typedefs macro_defs dtype)
    with _ -> None

  let fmt_mismatch ~context ~expected ~got =
    Printf.sprintf "%s: expected %s but got %s"
      context (string_of_data_type expected) (string_of_data_type got)

  let fmt_mismatch_opt ~context ~expected ~got =
    Printf.sprintf "%s: expected %s but got %s"
      context (string_of_data_type_opt expected) (string_of_data_type got)

  let fmt_size_mismatch ~context ~expected ~got ~expected_size ~got_size =
    Printf.sprintf "%s: expected %s (size %d) but got %s (size %d)"
      context (string_of_data_type expected) expected_size
      (string_of_data_type got) got_size

  (** Format assignment type error *)
  let fmt_assign lval expected got =
    fmt_mismatch
      ~context:(Printf.sprintf "In assignment: Invalid data type for %s" (string_of_lvalue lval))
      ~expected ~got

  let fmt_func_arg func_name arg_name expected got =
    fmt_mismatch
      ~context:(Printf.sprintf "In function call %s: Invalid argument type for %s" func_name arg_name)
      ~expected ~got

  let fmt_binop op dtype1 dtype2 =
    Printf.sprintf "In binary operation %s: Invalid argument types: %s and %s"
      (string_of_binop op) (string_of_data_type dtype1) (string_of_data_type dtype2)

  let fmt_send endpoint expected got =
    fmt_mismatch
      ~context:(Printf.sprintf "In send: Invalid data type for message %s" endpoint)
      ~expected ~got

  let fmt_record_field field_name expected got =
    fmt_mismatch
      ~context:(Printf.sprintf "In record construction: Invalid data type for field %s" field_name)
      ~expected ~got

  let fmt_let_binding ident expected got =
    fmt_mismatch_opt
      ~context:(Printf.sprintf "Invalid data type for %s" ident)
      ~expected ~got

  let fmt_simple got =
    Printf.sprintf "Invalid data type: %s" (string_of_data_type got)

  let check ~typedefs ~macro_defs ~err_string ~expected ~got ~span ~file_name ~weak_mode =
    if expected = got then
      ()
    else
      let sz_expected = get_size typedefs macro_defs expected in
      let sz_got = get_size typedefs macro_defs got in
      match sz_expected, sz_got with
      | Some se, Some sg when se <> sg ->
        (* sizes don't match - always raise error *)
        let size_err = fmt_size_mismatch
          ~context:"Type size mismatch"
          ~expected ~got ~expected_size:se ~got_size:sg in
        type_error size_err span
      | Some _, Some _ ->
        if weak_mode then
          warn err_string span file_name
        else
          type_error err_string span
      | _ ->
        raise (Except.TypeError [
          Text ("Cannot determine type size for " ^ (string_of_data_type expected) ^ " or " ^ (string_of_data_type got));
          Except.codespan_local span
        ])
end

let check_dtype err_string dtype1 dtype2 span file_name allow typedefs macro_defs =
  match dtype1 with
  | Some dt1 ->
    DTypeCheck.check
      ~typedefs ~macro_defs
      ~err_string ~expected:dt1 ~got:dtype2
      ~span ~file_name ~weak_mode:allow
  | None -> ()