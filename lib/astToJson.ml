(** This module defines methods to assist in converting the AST to JSON using Yojson. *)

open Lang

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
      | Some (tid, eid, None) -> assoc
        [ ("tid", int tid); ("eid", int eid) ]
      | Some (tid, eid, Some sustained_to_eid) -> assoc
        [ ("tid", int tid); ("eid", int eid); ("to_eid", int sustained_to_eid) ]
      | None -> `Null
    ) in
    let def_spans = ("def_span", list def_span_to_yojson n.def_span) in

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

let event_graph_collection_to_yojson (gc: EventGraph.event_graph_collection) : Yojson.Safe.t =

  let event_graph_to_order (t: EventGraph.event_graph) =
    let events = t.events in
    let get_event_json (e: EventGraph.event) =
      let get_thread_event_id_pair (e: EventGraph.event) = assoc [
        ("tid", int e.graph.thread_id);
        ("eid", int e.id)
      ] in

      if List.is_empty e.outs
      then assoc [ ("eid", int e.id) ]
      else assoc [
        ("eid", int e.id);
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
  list proc_graph_to_order procs



(* Functions for conversion of a compilation unit to Yojson *)

let compilation_unit_with_supplementary_data_to_yojson (c: compilation_unit) (sd: (string * Yojson.Safe.t) list) =
  kind "compilation_unit" ([
    ("file_name", opt str c.cunit_file_name);
    ("channel_classes", list channel_class_def_to_yojson c.channel_classes);
    ("type_defs", list type_def_to_yojson c.type_defs);
    ("macro_defs", list macro_def_to_yojson c.macro_defs);
    ("func_defs", list func_def_to_yojson c.func_defs);
    ("procs", list proc_def_to_yojson c.procs);
    ("imports", list import_directive_to_yojson c.imports);
    ("_extern_procs", list proc_def_to_yojson c._extern_procs)
  ] @ sd)


let compilation_unit_with_event_graph_to_yojson (c: compilation_unit) (gc: EventGraph.event_graph_collection option) =
  let events_json = match gc with
    | Some g -> event_graph_collection_to_yojson g
    | None -> `Null
  in
  compilation_unit_with_supplementary_data_to_yojson c [("event_graphs", events_json)]


let compilation_unit_to_yojson (c: compilation_unit) =
  compilation_unit_with_supplementary_data_to_yojson c []


