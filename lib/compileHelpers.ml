exception CompileError of Except.error_message

let raise_compile_error file_name msg =
  let open Except in
  let msg = match file_name with
  | Some file_name -> List.map (fun frag ->
      match frag with
      | Codespan (None, codespan) -> Codespan (Some file_name, codespan)
      | _ -> frag
    ) msg
  | None -> msg
  in
  raise (CompileError msg)

let canonicalise_file_name file_origin file_name =
  if Filename.is_relative file_name then
    Filename.concat (Filename.dirname file_origin) file_name
  else
    file_name

let rec parse_recursive cunits parsed_files (config : Config.compile_config) filename =
  if Utils.StringSet.mem filename !parsed_files |> not then (
    parsed_files := Utils.StringSet.add filename !parsed_files;
    let cunit =
      let source_filename = match config.input_filenames with
        | top_level::_ when (config.stdin && top_level = filename) -> "-"
        | _ -> filename
      in
      try
        InChannelCacheableAliasable.with_open_aliased
          source_filename
          filename
          (fun in_data ->
            let lexbuf = Lexing.from_string (Bytes.to_string (in_data.buffer)) in
            try Parser.cunit Lexer.read lexbuf
            with
              | Lexer.SyntaxError msg ->
                let open Except in
                [
                  (Text "Syntax error:");
                  (Text msg);
                  (Codespan (Some filename, {st = Lexing.lexeme_start_p lexbuf; ed = Lexing.lexeme_end_p lexbuf}))
                ] |> raise_compile_error (Some filename)
              | _ ->
                let open Except in
                [
                  (Text "Syntax error");
                  (Codespan (Some filename, {st = Lexing.lexeme_start_p lexbuf; ed = Lexing.lexeme_end_p lexbuf}))
                ] |> raise_compile_error (Some filename)
          )
      with
        | Sys_error msg ->
          raise_compile_error (Some filename) [Except.Text msg]
    in
    let cunit = {cunit with cunit_file_name = Some filename} in
    (
      try
        GraphBuilder.syntax_tree_precheck config cunit
      with
        | Except.TypeError msg -> raise_compile_error (Some filename) msg
    );
    cunits := (filename, cunit)::!cunits;
    List.iter (fun imp ->
      let open Lang in
      if not imp.is_extern then
        canonicalise_file_name filename imp.file_name
          |> parse_recursive cunits parsed_files config
    ) cunit.imports
  )

