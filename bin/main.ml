let compile_with_json_output config =
  let open Anvil.Config in
  if config.input_filenames = [] then begin
  let json_errors = [Anvil.JsonOutput.{
    error_type = "error";
    path = None;
    description = [ { kind = "text"; text = Some "Error: a file name must be supplied!"; trace = None } ]
  }] in
    let json_result = Anvil.JsonOutput.failure_output json_errors in
    print_endline (Anvil.JsonOutput.json_output_to_string json_result);
    exit 1
  end else begin
    let temp_file = Filename.temp_file "anvil_output" ".sv" in
    try
      Anvil.AstAnnotator.enabled := config.ast_output;
      if config.ast_output then
        let cunits, gcols, errors = Anvil.CompileDriver.parse config in
        let json_errors =
          let convert error_msg = Anvil.JsonOutput.error_message_to_json_error "error" error_msg in
          let compile_error_to_json_error (e: exn) =
            match e with
            | Anvil.CompileHelpers.CompileError msg -> convert msg
            | e -> convert [Anvil.Except.Text ("Unhandled error detected: " ^ (Printexc.to_string e))]
           in
          List.map compile_error_to_json_error errors
        in
        let json_result = Anvil.JsonOutput.ast_output cunits gcols json_errors in
        print_endline (Anvil.JsonOutput.json_output_to_string json_result);
        exit 0
      else ();

      let temp_out = open_out temp_file in
      (try
        Anvil.CompileDriver.compile temp_out config;
        close_out temp_out;
        let output_content = In_channel.with_open_text temp_file In_channel.input_all in
        let json_result = Anvil.JsonOutput.transpiled_output output_content in
        print_endline (Anvil.JsonOutput.json_output_to_string json_result)
      with
        | exn ->
          close_out_noerr temp_out;
          raise exn
      );
      Sys.remove temp_file
    with
    | Anvil.CompileHelpers.CompileError msg ->
      (try Sys.remove temp_file with _ -> ());
      let json_error = Anvil.JsonOutput.error_message_to_json_error "error" msg in
      let json_result = Anvil.JsonOutput.failure_output [json_error] in
      print_endline (Anvil.JsonOutput.json_output_to_string json_result);
      exit 1
  end

let compile_with_normal_output config =
  let open Anvil.Config in
  if config.input_filenames = [] && not config.stdin then begin
    Printf.eprintf "Error: a file name must be supplied!\n";
    exit 1
  end else begin
    let out_channel = match config.output_filename with
      | Some filename -> open_out (Printf.sprintf "%s.anvil.sv" filename)
      | None -> stdout
    in
    try
      Anvil.CompileDriver.compile out_channel config;
      if Option.is_some config.output_filename then
        close_out out_channel
    with
    |  Anvil.CompileHelpers.CompileError msg ->
      if Option.is_some config.output_filename then
        close_out_noerr out_channel;
      let open Anvil.Lang in
      Printf.eprintf "Compilation failed!\n";
      let open Anvil.Except in
      List.iter (
        function
        | Text msg_text -> Printf.eprintf "%s\n" msg_text
        | Codespan (file_name, span) -> (
          let file_name = Option.get file_name in
          Printf.eprintf "%s:%d:%d:\n" file_name span.st.pos_lnum (span.st.pos_cnum - span.st.pos_bol);
          Anvil.SpanPrinter.print_code_span ~indent:2 ~trunc:(-5) stderr file_name span
        )
      ) msg;
      exit 1
  end

let () =
  let config = Anvil.Config.parse_args() in
  if config.ast_output && not config.json_output then begin
    Printf.eprintf "Error: -ast requires -json to be set!\n";
    exit 1
  end else if config.json_output then
    compile_with_json_output config
  else
    compile_with_normal_output config
