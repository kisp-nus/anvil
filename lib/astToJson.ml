(** This module defines methods to assist in converting the AST to JSON using Yojson. *)

open Lang

let ast_major_version = 0 (* incr w/ breaking changes *)
let ast_minor_version = 1 (* incr w/ non-breaking additions *)
let ast_patch_version = 0 (* incr w/ non-breaking bug fixes *)
let ast_wip_build = 1 (* incr w/ each build while in wip state; set to 0 when finalized *)

let ast_json_schema_version_string =
  let wip_str = if ast_wip_build > 0 then Printf.sprintf "-wip.%d" ast_wip_build else "" in
  Printf.sprintf "v%d.%d.%d%s" ast_major_version ast_minor_version ast_patch_version wip_str


(* Symbolic sum type for representing cycle delays with unknowns (e.g., N_0 + 1) *)
type cycle_time_sum_term =
  | CycleConstTime of int
  | CycleUnknownTime of string
  | CycleOrTime of cycle_time_sum list
  | CycleMaxTime of string * cycle_time_sum list

and cycle_time_sum = cycle_time_sum_term list

type optionable_cycle_time_sum = cycle_time_sum_term option list

(* Helper functions for cycle time symbolic sums *)
let rec simplify_cycle_time_sum (terms: cycle_time_sum) : cycle_time_sum =
  match terms with
  | [] -> []
  | CycleConstTime n :: rest when n = 0 -> rest
  | t :: rest -> t :: simplify_cycle_time_sum rest

let rec sort_cycle_time_sum (terms: cycle_time_sum) : cycle_time_sum =
  terms
  |> List.sort (fun a b -> match a, b with
    | CycleUnknownTime s1, CycleUnknownTime s2 -> String.compare s1 s2
    | CycleUnknownTime _, CycleConstTime _ -> -1
    | CycleConstTime _, CycleUnknownTime _ -> 1
    | CycleConstTime n1, CycleConstTime n2 -> compare n1 n2
    | CycleMaxTime _, _ -> 1
    | _, CycleMaxTime _ -> -1
    | CycleOrTime _, _ -> 1
    | _, CycleOrTime _ -> -1
  )
  |> List.map (function
    | CycleMaxTime (s, max_terms) -> CycleMaxTime (s, List.map sort_cycle_time_sum max_terms)
    | t -> t
  )

let equal_cycle_time_sums (terms1: cycle_time_sum) (terms2: cycle_time_sum) : bool =
  let symbol_count_tbl = Hashtbl.create ((List.length terms1 + List.length terms2) * 5) in

  let count_const terms =
    List.fold_left (fun acc term ->
      match term with
      | CycleConstTime n -> acc + n
      | _ -> acc
    ) 0 terms
  in

  let count_terms terms dir =
    List.iter (function
      | CycleUnknownTime s ->
          let count = Hashtbl.find_opt symbol_count_tbl s |> Option.value ~default:0 in
          Hashtbl.replace symbol_count_tbl s (count + dir)
      | CycleMaxTime (s, _) ->
          let count = Hashtbl.find_opt symbol_count_tbl s |> Option.value ~default:0 in
          Hashtbl.replace symbol_count_tbl s (count + dir)
      | _ -> ()
    ) terms
  in

  count_terms terms1 (+1);
  count_terms terms2 (-1);

  let result =
    (* if all counts are 0, then the symbolic parts are equivalent *)
    Hashtbl.fold (fun _ count acc -> if count <> 0 then false else acc) symbol_count_tbl true &&
    (* if the constants are equal, then the constant parts are equivalent *)
    count_const terms1 = count_const terms2
  in
  result

let rec add_const_to_cycle_time_sum (n: int) (terms: cycle_time_sum) : cycle_time_sum =
  match terms with
  | CycleConstTime m :: rest ->
      (* merge with the previous constants *)
      add_const_to_cycle_time_sum (n + m) rest
  | CycleMaxTime (max_s, _) :: rest ->
      (* replace the max term with its shorthand as it'll no longer be the most recent addition *)
      add_const_to_cycle_time_sum n (CycleUnknownTime max_s :: rest)
  | _ ->
      (* base case (the constant added at the end to maintain fast access) (only if non-0) *)
      if n = 0 then terms else CycleConstTime n :: terms

let rec add_unknown_to_cycle_time_sum (s: string) (terms: cycle_time_sum) : cycle_time_sum =
  match terms with
  | CycleConstTime n :: rest ->
      (* constants must remain at the end, so we skip over them to add the unknown before it *)
      CycleConstTime n :: (add_unknown_to_cycle_time_sum s rest)
  | CycleMaxTime (max_s, _) :: rest ->
      (* replace the max term with its shorthand as it'll no longer be the most recent addition *)
      add_unknown_to_cycle_time_sum s (CycleUnknownTime max_s :: rest)
  | _ ->
      (* base case, after constants *)
      CycleUnknownTime s :: terms

let or_cycle_time_sum (sums: cycle_time_sum list) : cycle_time_sum =
  (* flatten nested ors *)
  let rec flatten_or (terms: cycle_time_sum list) : cycle_time_sum list =
    match terms with
    | [] -> []
    | [CycleOrTime inner_sums] :: rest ->
        flatten_or inner_sums @ flatten_or rest
    | t :: rest ->
        t :: flatten_or rest
  in

  let merged_sums = flatten_or sums in
  match merged_sums with
  | [] -> []
  | [single] -> single
  | _ -> (CycleOrTime merged_sums) :: []


let max_cycle_time_sum (s: string) (max_terms: cycle_time_sum list) : cycle_time_sum =
  (* highlight the new max-expr created by having it at the end *)

  let merge_max_of sum1 sum2 =
    let const1 = List.fold_left (fun acc term ->
      match term with
      | CycleConstTime n -> acc + n
      | _ -> acc
    ) 0 sum1 in
    let symbolic1 = List.filter (function
      | CycleConstTime _ -> false
      | _ -> true
    ) sum1 |> sort_cycle_time_sum in
    let const2 = List.fold_left (fun acc term ->
      match term with
      | CycleConstTime n -> acc + n
      | _ -> acc
    ) 0 sum2 in
    let symbolic2 = List.filter (function
      | CycleConstTime _ -> false
      | _ -> true
    ) sum2 |> sort_cycle_time_sum in
    let max_of_consts = max const1 const2 in

    if symbolic1 = symbolic2 then
      if max_of_consts = 0 then
        Some symbolic1
      else
        Some ((CycleConstTime max_of_consts) :: symbolic1)

    else
      (* determine if one side is strictly greater than the other by checking if they have strictly more unknown terms *)
      let sym1_counts = Hashtbl.create 10 in
      let sym2_counts = Hashtbl.create 10 in
      List.iter (function
        | CycleUnknownTime s ->
            let count = Hashtbl.find_opt sym1_counts s |> Option.value ~default:0 in
            Hashtbl.replace sym1_counts s (count + 1)
        | _ -> ()
      ) symbolic1;
      List.iter (function
        | CycleUnknownTime s ->
            let count = Hashtbl.find_opt sym2_counts s |> Option.value ~default:0 in
            Hashtbl.replace sym2_counts s (count + 1)
        | _ -> ()
      ) symbolic2;

      let sym1_greater = Hashtbl.fold (fun s count acc ->
        let sym2_count = Hashtbl.find_opt sym2_counts s |> Option.value ~default:0 in
        if count > sym2_count then true else acc
      ) sym1_counts false in
      let sym2_greater = Hashtbl.fold (fun s count acc ->
        let sym1_count = Hashtbl.find_opt sym1_counts s |> Option.value ~default:0 in
        if count > sym1_count then true else acc
      ) sym2_counts false in

      if sym1_greater && not sym2_greater then
        Some ((CycleConstTime max_of_consts) :: symbolic1)
      else if sym2_greater && not sym1_greater then
        Some ((CycleConstTime max_of_consts) :: symbolic2)
      else
        (* non trivial case *)
        None
  in

  let rec flatten_max (sums: cycle_time_sum list) : cycle_time_sum list =
    match sums with
    | [] -> []
    | [CycleMaxTime (_, inner_max_terms)] :: rest ->
        flatten_max inner_max_terms @ flatten_max rest
    | t :: rest ->
        t :: flatten_max rest
  in

  let rec merge_maxes (terms : cycle_time_sum list) : cycle_time_sum list =
    match terms with
    | t1 :: t2 :: rest -> (
      match merge_max_of t1 t2 with
      | Some merged -> merge_maxes (merged :: rest)
      | None -> t1 :: (merge_maxes (t2 :: rest))
    )
    | terms -> terms
  in

  (CycleMaxTime (s, max_terms |> flatten_max |> merge_maxes)) :: []


let rec extend_cycle_time_sums (terms1: cycle_time_sum) (terms2: cycle_time_sum) : cycle_time_sum =
  match terms1 with
  | [] -> terms2
  | CycleConstTime n :: rest -> extend_cycle_time_sums rest terms2 |> add_const_to_cycle_time_sum n
  | CycleUnknownTime s :: rest -> extend_cycle_time_sums rest terms2 |> add_unknown_to_cycle_time_sum s
  | CycleOrTime sums :: rest -> (or_cycle_time_sum sums) @ (extend_cycle_time_sums rest terms2)
  | CycleMaxTime (s, ts) :: rest -> (max_cycle_time_sum s ts) @ (extend_cycle_time_sums rest terms2)



(* Global mapping from (tid, eid, to_eid option) to symbolic sum consumed by that action *)
(* This is populated during event graph processing and used when converting AST nodes *)
let global_event_id_to_unknown_cycles_map : ((int * int * int option), cycle_time_sum) Hashtbl.t = Hashtbl.create 100


(* Helper functions for constructing Yojson values *)
let assoc l = `Assoc l
let str s = `String s
let int n = `Int n
let bool b = `Bool b
let list f xs = `List (List.map f xs)
let list_rev f xs = `List (List.rev_map f xs)

let opt f = function
  | None -> `Null
  | Some v -> f v

let kind (k: string) (fields: (string * Yojson.Safe.t) list) =
  assoc (("kind", str k) :: fields)


(* Function for converting a cycle time symbolic sum to Yojson. *)
let rec symbolic_sum_to_yojson (terms: cycle_time_sum) : Yojson.Safe.t =
  list_rev (function
    | CycleConstTime n -> assoc [("const", int n)]
    | CycleUnknownTime s -> assoc [("sym", str s)]
    | CycleOrTime sums -> assoc [
        ("or", list symbolic_sum_to_yojson sums)
      ]
    | CycleMaxTime (s, max_terms) -> assoc [
        ("sym", str s);
        ("max", list symbolic_sum_to_yojson max_terms)
      ]
  ) (simplify_cycle_time_sum terms)


(* Functions for conversion of Lang constructs to Yojson.
   Each function takes a language construct and returns a Yojson.Safe.t representing that node. *)

let code_span_to_yojson (s: code_span) =
  if s = code_span_dummy then `Null else
  let open Lexing in
  assoc [
    ("start", assoc [
        ("line", int s.st.pos_lnum);
        ("col", int (s.st.pos_cnum - s.st.pos_bol))
      ]);
    ("end", assoc [
        ("line", int s.ed.pos_lnum);
        ("col", int (s.ed.pos_cnum - s.ed.pos_bol))
      ])
  ]

let def_span_to_yojson (s: def_span) =
  if s.st = code_span_dummy.st && s.ed = code_span_dummy.ed then `Null else
  let open Lexing in
  assoc [
    ("file_name", opt str s.cunit);
    ("start", assoc [
        ("line", int s.st.pos_lnum);
        ("col", int (s.st.pos_cnum - s.st.pos_bol))
      ]);
    ("end", assoc [
        ("line", int s.ed.pos_lnum);
        ("col", int (s.ed.pos_cnum - s.ed.pos_bol))
      ])
  ]

let identifier_to_yojson (id: identifier) = str id

let ast_node_to_yojson f (n: 'a ast_node) = kind "ast_node" (
    let sp = ("span", code_span_to_yojson n.span) in
    let dt = ("data", f n.d) in
    let eid = ("event", match n.action_event with
      | Some (tid, eid, sus_to_eid, blocking_cycles) ->
        let live_to_eid_field = (match sus_to_eid with
          | Some sustained_to_eid -> [ ("live_to_eid", int sustained_to_eid) ]
          | None -> []
        ) in
        let exec_delay_field : (string * Yojson.Safe.t) list =
          let lookup = Hashtbl.find_opt global_event_id_to_unknown_cycles_map (tid, eid, sus_to_eid) in
          match blocking_cycles, lookup with
          | _, Some c -> [ ("delay_to_exec", symbolic_sum_to_yojson c) ]
          | n, _ -> [ ("delay_to_exec", symbolic_sum_to_yojson (add_const_to_cycle_time_sum n [])) ]
        in
        assoc (
          [
            ("tid", int tid);
            ("eid", int eid);
          ] @ live_to_eid_field @ exec_delay_field
        )
      | None -> `Null
    ) in
    let def_spans = ("def_span", list_rev def_span_to_yojson n.def_span) in

    match n.action_event, n.def_span with
    | Some _, [] -> [dt; sp; eid]
    | Some _, _ -> [dt; sp; eid; def_spans]
    | None, [] -> [dt; sp]
    | None, _ -> [dt; sp; def_spans]
  )

let arbitrary_type_maybe_param_to_yojson typ_f param =
    match param with
    | ParamEnv.Param s -> assoc [("param", str s)]
    | Concrete v -> assoc [("value", typ_f v)]

let param_type_to_yojson (pt: param_type) = match pt with
  | IntParam -> str "int"
  | TypeParam -> str "type"

let param_to_yojson (p: param) =
  kind "param" [
    ("name", identifier_to_yojson p.param_name);
    ("type", param_type_to_yojson p.param_ty)
  ]


let message_specifier_to_yojson (ms: message_specifier) =
  kind "message_specifier" [
    ("endpoint", identifier_to_yojson ms.endpoint);
    ("msg", identifier_to_yojson ms.msg)
  ]


let delay_pat_to_yojson (x: delay_pat) = kind "delay_pat" (
  match x with
  | `Cycles n ->
      [
        ("type", str "cycles");
        ("value", int n)
      ]

  | `Message_with_offset (ms, o, pos) ->
      [
        ("type", str "message");
        ("value", message_specifier_to_yojson ms);
        ("offset", if pos then int o else int (-o));
      ]

  | `Eternal ->
      [
        ("type", str "eternal")
      ])



let delay_pat_chan_local_to_yojson x = kind "delay_pat_chan_local" (
  match x with
  | `Cycles n ->
      [
        ("type", str "cycles");
        ("value", int n)
      ]

  | `Message_with_offset (id, o, pos) ->
      [
        ("type", str "message");
        ("value", identifier_to_yojson id);
        ("offset", if pos then int o else int (-o));
      ]

  | `Eternal ->
      [
        ("type", str "eternal")
      ])


let sig_lifetime_to_yojson (s: sig_lifetime) =
  kind "sig_lifetime" [
    ("ending", delay_pat_to_yojson s.e)
  ]

let sig_lifetime_chan_local_to_yojson (s: sig_lifetime_chan_local) =
  kind "sig_lifetime_chan_local" [
    ("ending", delay_pat_chan_local_to_yojson s.e)
  ]


let endpoint_direction_to_yojson (d: endpoint_direction) = match d with
  | Left -> str "left"
  | Right -> str "right"



let literal_to_yojson (l: literal) = match l with
  | Binary (len, bits) ->
      kind "literal" [
        ("type", str "binary");
        ("length", int len);
        ("digits", list (fun b -> int (value_of_digit b)) bits)
      ]

  | Decimal (len, digits) ->
      kind "literal" [
        ("type", str "decimal");
        ("length", int len);
        ("digits", list (fun d -> int (value_of_digit d)) digits)
      ]

  | Hexadecimal (len, digits) ->
      kind "literal" [
        ("type", str "hex");
        ("length", int len);
        ("digits", list (fun d -> int (value_of_digit d)) digits)
      ]

  | WithLength (len, v) ->
      kind "literal" [
        ("type", str "with_length");
        ("length", int len);
        ("value", int v)
      ]

  | NoLength v ->
      kind "literal" [
        ("type", str "no_length");
        ("value", int v)
      ]


let rec data_type_to_yojson (x: data_type) = kind "data_type" (
  match x with
  | `Logic ->
      [("type", str "logic")]

  | `Array (dtype, size) ->
      [
        ("type", str "array");
        ("element", data_type_to_yojson dtype);
        ("size", arbitrary_type_maybe_param_to_yojson int size)
      ]

  | `Tuple elems ->
      [
        ("type", str "tuple");
        ("elements", list data_type_to_yojson elems)
      ]

  | `Record fields ->
      [
        ("type", str "record");
        ("elements",
          list_rev (ast_node_to_yojson (fun (id, dt) ->
            assoc [
              ("kind", str "type_element_def");
              ("name", identifier_to_yojson id);
              ("data_type", data_type_to_yojson dt)
            ])
          ) fields)
      ]

  | `Variant (opt_dt, elems) ->
      [
        ("type", str "variant");
        ("data_type", opt data_type_to_yojson opt_dt);
        ("elements",
          list (ast_node_to_yojson (fun (id, opt_dt, opt_literal) ->
            assoc [
              ("kind", str "type_element_def");
              ("name", identifier_to_yojson id);
              ("data_type", opt data_type_to_yojson opt_dt);
              ("literal", opt literal_to_yojson opt_literal);
            ])
          ) elems)
      ]

  | `Opaque id ->
      [
        ("type", str "opaque");
        ("name", identifier_to_yojson id)
      ]

  | `Named (id, params) ->
      [
        ("type", str "named");
        ("name", identifier_to_yojson id);
        ("params", list param_value_to_yojson params)
      ])

and param_value_to_yojson (x: param_value) = kind "param_value" (
  match x with
  | IntParamValue n ->
      [
        ("type", str "int");
        ("value", int n)
      ]
  | TypeParamValue dt ->
      [
        ("type", str "type");
        ("data_type", data_type_to_yojson dt)
      ])


let rec array_dimm_concrete_to_yojson x =
  kind "array_dimensions_concrete" (
    match x with
    | OneDimmension n ->
        [("type", str "one"); ("size", int n)]
    | MultiDimmension (n, sub) ->
        [("type", str "multi"); ("size", int n); ("sub", array_dimm_concrete_to_yojson sub)]
  )

let rec array_index_concrete_to_yojson x =
  kind "array_index_concrete" (
    match x with
    | IndexSingle n ->
        [
          ("type", str "single");
          ("index", int n)
        ]
    | IndexRange (start, size) ->
        [
          ("type", str "range");
          ("start", int start);
          ("size", int size)
        ]
    | IndexMultiDimRange (start, size, sub) ->
        [
          ("type", str "multi_range");
          ("start", int start);
          ("size", int size);
          ("sub", array_index_concrete_to_yojson sub)
        ]
    | IndexMultiDimSingle (index, sub) ->
        [
          ("type", str "multi_single");
          ("index", int index);
          ("sub", array_index_concrete_to_yojson sub)
        ]
  )

let rec array_dimensions_to_yojson x =
  kind "array_dimensions" (
    match x with
    | OneDim dim ->
        [("type", str "one");
         ("size", arbitrary_type_maybe_param_to_yojson int dim)]
    | MultiDim (dim, sub) ->
        [("type", str "multi");
         ("size", arbitrary_type_maybe_param_to_yojson int dim);
         ("sub", array_dimensions_to_yojson sub)]
  )


let endpoint_def_to_yojson (e: endpoint_def) =
  kind "endpoint_def" [
    ("name", identifier_to_yojson e.name);
    ("channel_class", identifier_to_yojson e.channel_class);
    ("channel_params", list param_value_to_yojson e.channel_params);
    ("dir", endpoint_direction_to_yojson e.dir);
    ("foreign", bool e.foreign);
    ("opp", opt identifier_to_yojson e.opp)
  ]

let macro_def_to_yojson (m: macro_def) =
  kind "macro_def" [
    ("id", identifier_to_yojson m.id);
    ("value", int m.value);
    ("span", code_span_to_yojson m.span);
    ("file_name", opt str m.cunit_file_name);
  ]

let type_def_to_yojson (t: type_def) =
  kind "type_def" [
    ("name", identifier_to_yojson t.name);
    ("data_type", data_type_to_yojson t.body);
    ("params", list param_to_yojson t.params);
    ("span", code_span_to_yojson t.span);
    ("file_name", opt str t.cunit_file_name);
  ]


let sig_type_to_yojson (s: sig_type) =
  kind "sig_type" [
    ("data_type", data_type_to_yojson s.dtype);
    ("lifetime", sig_lifetime_to_yojson s.lifetime)
  ]

let sig_type_chan_local_to_yojson (s: sig_type_chan_local) =
  kind "sig_type_chan_local" [
    ("data_type", data_type_to_yojson s.dtype);
    ("lifetime", sig_lifetime_chan_local_to_yojson s.lifetime)
  ]


let reg_def_to_yojson (r: reg_def) =
  kind "reg_def" [
    ("name", str r.name);
    ("data_type", data_type_to_yojson r.d_type);
    ("init", opt str r.init)
  ]

let message_direction_to_yojson (d: message_direction) = match d with
  | Inp -> str "in"
  | Out -> str "out"


let message_sync_mode_to_yojson (m: message_sync_mode) = match m with
  | Dynamic -> kind "message_sync_mode" [("type", str "dynamic")]
  | Static (init, interval) -> kind "message_sync_mode" [
      ("type", str "static");
      ("init", int init);
      ("interval", int interval)
    ]
  | Dependent (msg, delay) -> kind "message_sync_mode" [
      ("type", str "dependent");
      ("msg", str msg);
      ("delay", int delay)
    ]


let message_def_to_yojson (m: message_def) =
  kind "message_def" [
    ("name", identifier_to_yojson m.name);
    ("dir", message_direction_to_yojson m.dir);
    ("send_sync", message_sync_mode_to_yojson m.send_sync);
    ("recv_sync", message_sync_mode_to_yojson m.recv_sync);
    ("sig_types", list sig_type_chan_local_to_yojson m.sig_types);
    ("span", code_span_to_yojson m.span)
  ]

let channel_class_def_to_yojson (c: channel_class_def) =
  kind "channel_class_def" [
    ("name", identifier_to_yojson c.name);
    ("messages", list message_def_to_yojson c.messages);
    ("params", list param_to_yojson c.params);
    ("span", code_span_to_yojson c.span);
    ("file_name", opt str c.cunit_file_name);
  ]

let channel_visibility_to_yojson (v: channel_visibility) = match v with
  | BothForeign -> str "both_foreign"
  | LeftForeign -> str "left_foreign"
  | RightForeign -> str "right_foreign"


let channel_def_to_yojson (c: channel_def) =
  kind "channel_def" [
    ("channel_class", identifier_to_yojson c.channel_class);
    ("channel_params", list param_value_to_yojson c.channel_params);
    ("endpoint_left", identifier_to_yojson c.endpoint_left);
    ("endpoint_right", identifier_to_yojson c.endpoint_right);
    ("visibility", channel_visibility_to_yojson c.visibility)
  ]


let binop_to_yojson_str (x: binop) = match x with
  | Add -> "add"
  | Sub -> "sub"
  | Xor -> "xor"
  | And -> "and"
  | Or -> "or"
  | Lt -> "lt"
  | Gt -> "gt"
  | Lte -> "lte"
  | Gte -> "gte"
  | Shl -> "shl"
  | Shr -> "shr"
  | Eq -> "eq"
  | Neq -> "neq"
  | Mul -> "mul"
  | In -> "in"
  | LAnd -> "land"
  | LOr -> "lor"

let unop_to_yojson_str (x: unop) = match x with
  | Neg -> "neg"
  | Not -> "not"
  | AndAll -> "and_all"
  | OrAll -> "or_all"



let singleton_or_list_to_yojson (f: 'a -> Yojson.Safe.t) (x: 'a singleton_or_list) = kind "singleton_or_list" (
  match x with
  | `Single v -> [ ("type", str "single"); ("value", f v) ]
  | `List vs -> [ ("type", str "list"); ("values", list f vs) ]
)


let rec send_pack_to_yojson (sp: send_pack) =
  kind "send_pack" [
    ("msg_spec", message_specifier_to_yojson sp.send_msg_spec);
    ("data", expr_node_to_yojson sp.send_data)
  ]
and recv_pack_to_yojson (rp: recv_pack) =
  kind "recv_pack" [
    ("msg_spec", message_specifier_to_yojson rp.recv_msg_spec)
  ]
and constructor_spec_to_yojson (cs: constructor_spec) =
  kind "constructor_spec" [
    ("type_name", identifier_to_yojson cs.variant_ty_name);
    ("name", ast_node_to_yojson identifier_to_yojson cs.variant)
  ]

and expr_to_yojson (x: expr) = let result = match x with
  | Literal v -> [ ("type", str "literal"); ("value", literal_to_yojson v) ]
  | Identifier id -> [ ("type", str "identifier"); ("id", identifier_to_yojson id) ]
  | Call (f, args) -> [
      ("type", str "Call");
      ("function", identifier_to_yojson f);
      ("args", list expr_node_to_yojson args)
    ]
  | Assign (lv, e) -> [
      ("type", str "assign");
      ("lvalue", lvalue_to_yojson lv);
      ("expr", expr_node_to_yojson e)
    ]
  | Binop (op, e1, e2) -> [
      ("type", str "binop");
      ("op", str (binop_to_yojson_str op));
      ("lhs", expr_node_to_yojson e1);
      ("rhs", singleton_or_list_to_yojson expr_node_to_yojson e2)
    ]
  | Unop (op, e) -> [
      ("type", str "unop");
      ("op", str (unop_to_yojson_str op));
      ("expr", expr_node_to_yojson e)
    ]
  | Tuple elist -> [
      ("type", str "tuple");
      ("elements", list expr_node_to_yojson elist)
    ]
  | Let (ids, ty, e) -> [
      ("type", str "let");
      ("data_type", opt data_type_to_yojson ty);
      ("ids", list identifier_to_yojson ids);
      ("expr", expr_node_to_yojson e)
    ]
  | Join (e1, e2) -> [
      ("type", str "join");
      ("lhs", expr_node_to_yojson e1);
      ("rhs", expr_node_to_yojson e2)
    ]
  | Wait (e1, e2) -> [
      ("type", str "wait");
      ("lhs", expr_node_to_yojson e1);
      ("rhs", expr_node_to_yojson e2)
    ]
  | Cycle n -> [
      ("type", str "cycle");
      ("cycles", int n)
    ]
  | Sync id -> [
      ("type", str "sync");
      ("id", identifier_to_yojson id)
    ]
  | IfExpr (cond, then_br, else_br) -> [
      ("type", str "if_expr");
      ("cond", expr_node_to_yojson cond);
      ("then", expr_node_to_yojson then_br);
      ("else", expr_node_to_yojson else_br)
    ]
  | TryRecv (id, rp, then_br, else_br) -> [
      ("type", str "try_recv");
      ("id", identifier_to_yojson id);
      ("recv_pack", recv_pack_to_yojson rp);
      ("then", expr_node_to_yojson then_br);
      ("else", expr_node_to_yojson else_br)
    ]
  | TrySend (sp, then_br, else_br) -> [
      ("type", str "try_send");
      ("send_pack", send_pack_to_yojson sp);
      ("then", expr_node_to_yojson then_br);
      ("else", expr_node_to_yojson else_br)
    ]
  | Construct (cs, eo) -> [
      ("type", str "construct");
      ("spec", constructor_spec_to_yojson cs);
      ("arg", (match eo with Some e -> expr_node_to_yojson e | None -> `Null))
    ]
  | Record (ty_name, fields, eo) -> [
      ("type", str "record");
      ("type_name", identifier_to_yojson ty_name);
      ("elements", list (ast_node_to_yojson (fun (id, e) ->
          assoc [
            ("id", identifier_to_yojson id);
            ("value", expr_node_to_yojson e)
          ]
      )) fields);
      ("base", (match eo with Some e -> expr_node_to_yojson e | None -> `Null))
    ]
  | Index (e, idx) -> [
      ("type", str "index");
      ("expr", expr_node_to_yojson e);
      ("index", index_to_yojson idx)
    ]
  | Indirect (e, field) -> [
      ("type", str "indirect");
      ("expr", expr_node_to_yojson e);
      ("field", ast_node_to_yojson identifier_to_yojson field)
    ]
  | Concat (elist, flat) -> [
      ("type", str "concat");
      ("elements", list expr_node_to_yojson elist);
      ("flat", bool flat)
    ]
  | Cast (e, ty) -> [
      ("type", str "cast");
      ("expr", expr_node_to_yojson e);
      ("data_type", data_type_to_yojson ty)
    ]
  | Ready msg_spec -> [
      ("type", str "ready");
      ("msg_spec", message_specifier_to_yojson msg_spec)
    ]
  | Probe msg_spec -> [
      ("type", str "probe");
      ("msg_spec", message_specifier_to_yojson msg_spec)
    ]
  | Match (e, branches) -> [
      ("type", str "match");
      ("expr", expr_node_to_yojson e);
      ("branches", list (fun (pat, br) ->
          kind "branch" [
            ("pattern", expr_node_to_yojson pat);
            ("branch", (match br with Some b -> expr_node_to_yojson b | None -> `Null))
          ]) branches)
    ]
  | Read lvalue -> [
      ("type", str "read");
      ("target", lvalue_to_yojson lvalue)
    ]
  | Debug op -> [
      ("type", str "debug");
      ("op", debug_op_to_yojson op)
    ]
  | Send sp -> [
      ("type", str "send");
      ("send_pack", send_pack_to_yojson sp)
    ]
  | Recv rp -> [
      ("type", str "recv");
      ("recv_pack", recv_pack_to_yojson rp)
    ]
  | SharedAssign (id, e) -> [
      ("type", str "shared_assign");
      ("id", identifier_to_yojson id);
      ("expr", expr_node_to_yojson e)
    ]
  | List elist -> [
      ("type", str "list");
      ("elements", list expr_node_to_yojson elist)
    ]
  | Recurse -> [ ("type", str "recurse") ]
  in kind "expr" result

and expr_node_to_yojson (x: expr_node) = ast_node_to_yojson expr_to_yojson x

and lvalue_to_yojson (x: lvalue) = let result = match x with
  | Reg id -> [ ("type", str "reg"); ("id", identifier_to_yojson id) ]
  | Indexed (lv, idx) -> [
      ("type", str "indexed");
      ("lvalue", lvalue_to_yojson lv);
      ("index", index_to_yojson idx)
    ]
  | Indirected (lv, field) -> [
      ("type", str "indirected");
      ("lvalue", lvalue_to_yojson lv);
      ("field", ast_node_to_yojson identifier_to_yojson field)
    ]
  in kind "lvalue" result

and index_to_yojson (x: index) = let result = match x with
  | Single e -> [
      ("type", str "single");
      ("expr", expr_node_to_yojson e)
    ]
  | Range (e1, e2) -> [
      ("type", str "range");
      ("start", expr_node_to_yojson e1);
      ("size", expr_node_to_yojson e2)
    ]
  in kind "index" result

and debug_op_to_yojson (x: debug_op) = let result = match x with
  | DebugPrint (s, elist) -> [
      ("type", str "debug_print");
      ("msg", str s);
      ("args", list expr_node_to_yojson elist)
    ]
  | DebugFinish -> [ ("type", str "debug_finish") ]
  in kind "debug_op" result



let thread_to_yojson (x: expr_node * message_specifier option) = kind "thread" (
  match x with
  | (e, Some rst) -> [
    ("expr", expr_node_to_yojson e);
    ("span", code_span_to_yojson e.span);
    ("rst", message_specifier_to_yojson rst)
  ]
  | (e, _) -> [
    ("expr", expr_node_to_yojson e);
    ("span", code_span_to_yojson e.span);
    ("rst", `Null)
  ])


let sig_def_to_yojson (s: sig_def) =
  kind "sig_def" [
    ("name", identifier_to_yojson s.name);
    ("stype", sig_type_to_yojson s.stype)
  ]

let cycle_proc_to_yojson (c: cycle_proc) =
  kind "cycle_proc" [
    ("trans_func", expr_node_to_yojson c.trans_func);
    ("sigs", list sig_def_to_yojson c.sigs)
  ]

let rec spawn_def_to_yojson (s: spawn_def) =
  kind "spawn_def" [
    ("proc", identifier_to_yojson s.proc);
    ("params", list args_spawn_to_yojson s.params);
    ("compile_params", list param_value_to_yojson s.compile_params)
  ]
and args_spawn_to_yojson (a: args_spawn) = kind "args_spawn" (
  match a with
  | SingleEp id -> [
      ("type", str "single");
      ("endpoint", identifier_to_yojson id)
  ]
  | IndexedEp (id, dims) -> [
      ("type", str "indexed");
      ("endpoint", identifier_to_yojson id);
      ("dimensions", array_index_concrete_to_yojson dims)
  ])

let shared_var_def_to_yojson (s: shared_var_def) =
  kind "shared_var_def" [
    ("ident", identifier_to_yojson s.ident);
    ("assigning_thread", int s.assigning_thread);
    ("shared_lifetime", sig_lifetime_to_yojson s.shared_lifetime)
  ]





let proc_def_body_to_yojson (b: proc_def_body) =
  kind "proc_def_body" [
    ("type", str "native");
    ("channels", list (ast_node_to_yojson channel_def_to_yojson) b.channels);
    ("spawns", list (ast_node_to_yojson spawn_def_to_yojson) b.spawns);
    ("regs", list (ast_node_to_yojson reg_def_to_yojson) b.regs);
    ("shared_vars", list (ast_node_to_yojson shared_var_def_to_yojson) b.shared_vars);
    ("threads", list thread_to_yojson b.threads)
  ]

let proc_def_body_extern_to_yojson (mod_name: string) (b: proc_def_body_extern) =
  kind "proc_def_body" [
    ("type", str "extern");
    ("module_name", str mod_name);
    ("named_ports", list (fun (name, ty) ->
        assoc [ ("name", str name); ("type", str ty) ]) b.named_ports);
    ("msg_ports", list (fun (msg_spec, data_port, valid_port, ack_port) ->
        assoc [
          ("msg_spec", message_specifier_to_yojson msg_spec);
          ("data_port", opt str data_port);
          ("valid_port", opt str valid_port);
          ("ack_port", opt str ack_port)
        ]) b.msg_ports)
  ]

let proc_def_body_maybe_extern_to_yojson (b: proc_def_body_maybe_extern) = match b with
  | Native body -> proc_def_body_to_yojson body
  | Extern (mod_name, extern_body) -> proc_def_body_extern_to_yojson mod_name extern_body

let proc_def_to_yojson (p: proc_def) =
  kind "proc_def" [
    ("name", str p.name);
    ("args", list (ast_node_to_yojson endpoint_def_to_yojson) p.args);
    ("body", proc_def_body_maybe_extern_to_yojson p.body);
    ("params", list param_to_yojson p.params);
    ("span", code_span_to_yojson p.span);
    ("file_name", opt str p.cunit_file_name);
  ]

let import_directive_to_yojson (i: import_directive) =
  kind "import_directive" [
    ("file_name", str i.file_name);
    ("is_extern", bool i.is_extern);
    ("span", code_span_to_yojson i.span)
  ]

let typed_arg_to_yojson (ta: typed_arg) =
  kind "typed_arg" [
    ("name", identifier_to_yojson ta.arg_name);
    ("data_type", opt data_type_to_yojson ta.arg_type);
    ("span", code_span_to_yojson ta.span)
  ]

let func_def_to_yojson (f: func_def) =
  kind "func_def" [
    ("name", identifier_to_yojson f.name);
    ("args", list typed_arg_to_yojson f.args);
    ("body", expr_node_to_yojson f.body);
    ("span", code_span_to_yojson f.span);
    ("file_name", opt str f.cunit_file_name);
  ]



(* Functions for conversion of important EventGraph constructs to Yojson *)

(* Functions for conversion of important EventGraph constructs to Yojson *)

let event_graph_collection_to_yojson (gc: EventGraph.event_graph_collection): Yojson.Safe.t list =

  (* Note: We do NOT clear the map here, as it needs to accumulate entries from all event graph collections
     in this compilation unit. The map is cleared once at the top level in compilation_unit_with_event_graph_to_yojson. *)

  (* Counter for generating unique symbolic variable names *)
  let symbolic_counter = ref 0 in
  let fresh_symbolic_var prefix = 
    let n = !symbolic_counter in
    symbolic_counter := n + 1;
    Printf.sprintf "%s%d" prefix n
  in

  let event_graph_to_order (t: EventGraph.event_graph) =
    let events = t.events in
    let tid = t.thread_id in

    (* First pass: identify all dynamic operations and assign symbolic variables *)
    let event_source_to_unknown_cycles_map = Hashtbl.create (List.length events) in
    let process_event_for_symbolic_vars (e: EventGraph.event) =
      match e.source with
      | `Seq (e0, atomic_delay) ->
          (match atomic_delay with
          | `Send _ | `Recv _ | `Sync _ ->
              let var = fresh_symbolic_var "n" in
              let symbolic_sum = [CycleUnknownTime var] in
              Hashtbl.add event_source_to_unknown_cycles_map e.source symbolic_sum;
              Hashtbl.add global_event_id_to_unknown_cycles_map (tid, e0.id, Some e.id) symbolic_sum
          | _ -> ())
      | _ -> ()
    in
    List.iter process_event_for_symbolic_vars events;

    (* Second pass: compute delays for each event, using the symbolic variables where needed *)
    let _delays_memo = Hashtbl.create (5 * List.length events) in
    let rec compute_delays (src_e: EventGraph.event_source) : cycle_time_sum =
      if Hashtbl.mem _delays_memo src_e then
        Hashtbl.find _delays_memo src_e
      else
        let delays = match src_e with
        | `Root None -> []
        | `Root Some (e0, _) -> compute_delays e0.source
        | `Seq (e0, atomic_delay) ->
            let adder (sum: cycle_time_sum) = (
              match atomic_delay with
                | `Cycles c -> add_const_to_cycle_time_sum c sum
                | `Send _ | `Recv _ | `Sync _ ->
                    (* Look up the symbolic variable we assigned in the first pass *)
                    (match Hashtbl.find_opt event_source_to_unknown_cycles_map src_e with
                    | Some sym_sum -> extend_cycle_time_sums sym_sum sum
                    | None -> sum) (* Should not happen *)
              ) in
            adder (compute_delays e0.source)
        | `Later (e1, e2) ->
            (* Later of the 2 events *)
            let e1_delays = compute_delays e1.source in
            let e2_delays = compute_delays e2.source in

            (* Compute the max of the two set of delays *)
            let new_max_var = fresh_symbolic_var "max" in
            let results = (max_cycle_time_sum new_max_var [e1_delays; e2_delays]) in

            results
        | `Branch (_, branch_info) ->
            (* A Branch event is reached when one of the branches completes (not when the branch point starts). *)
            let other_events = branch_info.branches_val in
            let other_delays = List.map (fun (oe: EventGraph.event) -> compute_delays oe.source) other_events in
            let unique_other_delays = List.fold_left (fun acc d -> if List.exists (equal_cycle_time_sums d) acc then acc else d :: acc) [] other_delays in
            or_cycle_time_sum unique_other_delays
        in
        Hashtbl.add _delays_memo src_e delays;
        delays
    in

    let get_event_json (e: EventGraph.event) =
      let get_thread_event_id_pair (e: EventGraph.event) = assoc [
        ("tid", int e.graph.thread_id);
        ("eid", int e.id)
      ] in

      let delays_json = symbolic_sum_to_yojson (compute_delays e.source) in

      if List.is_empty e.outs
      then assoc [
        ("eid", int e.id);
        ("delay", delays_json)
      ]
      else assoc [
        ("eid", int e.id);
        ("delay", delays_json);
        ("outs", list get_thread_event_id_pair e.outs)
      ]
    in
    let events_json = list_rev get_event_json events in
    let thread_id_json = int t.thread_id in
    assoc [
      ("tid", thread_id_json);
      ("events", events_json); (* assumption: already topologically sorted *)
      ("span", code_span_to_yojson t.thread_codespan)
    ]
  in

  let proc_graph_to_order (p: EventGraph.proc_graph) =
    let thread_orders = List.map (fun (e, _) -> event_graph_to_order e) p.threads in
    assoc [
      ("proc_name", str p.name);
      ("threads", list (fun t -> t) thread_orders)
    ]
  in

  let procs = gc.event_graphs in
  List.map proc_graph_to_order procs



(* Functions for conversion of a compilation unit to Yojson *)

let compilation_unit_with_supplementary_data_to_yojson (c: compilation_unit) (sd: (string * Yojson.Safe.t) list) =
  kind "compilation_unit" ([
    ("schema", str ast_json_schema_version_string);
    ("file_name", opt str c.cunit_file_name);
    ("channel_classes", list channel_class_def_to_yojson c.channel_classes);
    ("type_defs", list type_def_to_yojson c.type_defs);
    ("macro_defs", list macro_def_to_yojson c.macro_defs);
    ("func_defs", list func_def_to_yojson c.func_defs);
    ("procs", list proc_def_to_yojson c.procs);
    ("imports", list import_directive_to_yojson c.imports);
    ("_extern_procs", list proc_def_to_yojson c._extern_procs)
  ] @ sd)


let compilation_unit_with_event_graph_to_yojson (c: compilation_unit) (gcl: EventGraph.event_graph_collection list) =
  (* Clear the global map once at the start, before processing all event graph collections *)
  Hashtbl.clear global_event_id_to_unknown_cycles_map;
  let event_graphs = List.map (fun gc -> event_graph_collection_to_yojson gc) gcl |> List.concat in
  compilation_unit_with_supplementary_data_to_yojson c [("event_graphs", `List event_graphs)]

let compilation_unit_to_yojson (c: compilation_unit) =
  compilation_unit_with_supplementary_data_to_yojson c []


