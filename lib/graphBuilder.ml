open EventGraph
open EventGraphOps
open Lang
open ErrorCollector
open GraphBuildContext
open DataTypeCheck

type build_context = Typing.build_context
module BuildContext = Typing.BuildContext

let unwrap_or_err err_msg err_span opt =
  match opt with
  | Some d -> d
  | None -> raise_fatal (event_graph_error_default err_msg err_span)

let binop_td_const graph (ci:cunit_info) _ctx span op n td =
  let w = unwrap_or_err "Invalid value" span td.ld.w in
  let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs td.ld.dtype in
  let sz' = match op with
  | Add -> sz+1
  | Mul -> sz + Utils.int_log2 n
  | _ -> sz in
  let (wires', w') = if sz' > sz then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
      ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz, 0)) graph.wires in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w] wires'
  else (graph.wires, w) in
  let (wires'', wconst) = WireCollection.add_literal graph.thread_id
     ci.typedefs ci.macro_defs (WithLength (sz', n)) wires' in
  let (wires''', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w' (`Single wconst) wires'' in
  graph.wires <- wires''';
  {ld ={td.ld with w = Some wres; dtype = `Array (`Logic, ParamEnv.Concrete sz')}}


let binop_td_td graph (ci:cunit_info) ctx span op td1 td2 =
  let w1 = unwrap_or_err "Invalid value" span td1.ld.w in
  let w2 = unwrap_or_err "Invalid value" span td2.ld.w in
  let sz1 = TypedefMap.data_type_size ci.typedefs ci.macro_defs td1.ld.dtype
  and sz2 = TypedefMap.data_type_size ci.typedefs ci.macro_defs td2.ld.dtype in
  let sz' = match op with
  | Add -> (max sz1 sz2) +1
  | Mul -> sz1 + sz2
  | _ -> max sz1 sz2 in
  let (wires', w1') = if sz' > sz1 then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
    ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz1, 0)) graph.wires in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w1] wires'
  else (graph.wires, w1) in
  let (wires', w2') = if sz' > sz2 then
    let (wires', padding) = WireCollection.add_literal graph.thread_id
    ci.typedefs ci.macro_defs (Lang.WithLength (sz' - sz2, 0)) wires' in
    WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [padding; w2] wires'
  else (wires', w2) in
  let (wires', wres) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs op w1' (`Single w2') wires' in
  graph.wires <- wires';
  let open Typing in
  let new_dtype = (`Array (`Logic, ParamEnv.Concrete sz')) in
  Typing.merged_data graph (Some wres) new_dtype ctx.current [td1.ld; td2.ld]


let rec lvalue_info_of graph (ci:cunit_info) ctx (e:expr_node) lval =
  let span = e.span in
  let binop_td_const = binop_td_const graph ci ctx span
  and binop_td_td = binop_td_td graph ci ctx span in
  match lval with
  | Reg ident ->
    let r = Utils.StringMap.find_opt ident graph.regs
      |> unwrap_or_err ("Undefined register " ^ ident) span in
    let sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.d.d_type in
    AstAnnotator.attach_def_span_expr e r (Some ci.file_name);
    {
      lval_range = full_reg_range ident sz;
      lval_dtype = r.d.d_type
    }
  | Indexed (lval', idx) ->
    (* TODO: better code reuse *)
    let lval_info' = lvalue_info_of graph ci ctx e lval' in
    let (le', _len') = lval_info'.lval_range.subreg_range_interval in
    let (le, len, dtype) =
      TypedefMap.data_type_index ci.typedefs ci.macro_defs
        (construct_graphIR graph ci ctx)
        (binop_td_const Mul)
        lval_info'.lval_dtype idx
      |> unwrap_or_err "Invalid lvalue indexing" span in
    let le'_nd = MaybeConst.map (fun ld -> {ld}) le' in
    let le_n = MaybeConst.map (fun nd -> nd.ld)
      (MaybeConst.add (binop_td_const Add) (binop_td_td Add) le'_nd le)
    in
    (
      let type_def = TypedefMap.type_def_name_resolve ci.typedefs dtype in
      match type_def with
      | Some type_def -> AstAnnotator.attach_def_from_top_level_type e type_def
      | None -> ()
    );
    {
      lval_range = {lval_info'.lval_range with subreg_range_interval = (le_n, len)};
      lval_dtype = dtype
    }
  | Indirected (lval', fieldname) ->
    let lval_info' = lvalue_info_of graph ci ctx e lval' in
    let (le', _len') = lval_info'.lval_range.subreg_range_interval in
    let (le, len, dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs lval_info'.lval_dtype fieldname.d
      |> unwrap_or_err ("Invalid lvalue indirection through field " ^ fieldname.d) span in
    let le'_nd = MaybeConst.map (fun ld -> {ld}) le' in
    let le_n = MaybeConst.map (fun nd -> nd.ld)
      (MaybeConst.add_const le (binop_td_const Add) le'_nd)
    in
    (
      let type_def = TypedefMap.type_def_name_resolve ci.typedefs dtype in (
        match type_def with
        | Some type_def -> AstAnnotator.attach_def_from_top_level_type_with_fields e type_def [(fieldname.d, fieldname)]
        | None -> ()
      );
    );
    {
      lval_range = {lval_info'.lval_range with subreg_range_interval = (le_n, len)};
      lval_dtype = dtype
    }

and construct_graphIR (graph : event_graph) (ci : cunit_info)
                    (ctx : build_context) (e : expr_node) : node_data =
  let binop_td_const = binop_td_const graph ci ctx
  and _binop_td_td = binop_td_td graph ci ctx in
  match e.d with
  | Literal lit ->
    let (wires', w) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs lit graph.wires in
    graph.wires <- wires';
    Typing.const_data graph (Some w) (dtype_of_literal lit :> data_type) ctx.current
  | Sync ident ->
    (
      let shared_info = Hashtbl.find_opt ctx.shared_vars_info ident
        |> unwrap_or_err ("Undefined identifier: " ^ ident) e.span in
      Typing.sync_event_data graph ident shared_info.value ctx.current
    )
  | Identifier ident ->
      let ctx_val = Typing.context_lookup ctx.cg_lt_ctx ident in
      let macro_val = List.assoc_opt ident (List.map (fun (macro : macro_def) ->(macro.id, macro)) ci.macro_defs) in
      (match ctx_val, macro_val with
        | Some a, Some b_def ->
          AstAnnotator.attach_def_span e a.binding_def_span;
          AstAnnotator.attach_def_from_top_level_macro e b_def;
          raise (event_graph_error_default ("Conflicting Identifier " ^ ident ^ " declarations found") e.span);
          Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
        | Some binding, None ->
          AstAnnotator.attach_def_span e binding.binding_def_span;
          Typing.use_binding binding |> Typing.sync_data graph ctx.current
        | None, Some macro ->
          AstAnnotator.attach_def_from_top_level_macro e macro;
          let sz = Utils.int_log2 (macro.value + 1) in
          let (wires', w) = WireCollection.add_literal graph.thread_id
              ci.typedefs ci.macro_defs
              (WithLength (sz, macro.value)) graph.wires in
            graph.wires <- wires';
            Typing.const_data graph (Some w) (`Array (`Logic, ParamEnv.Concrete sz)) ctx.current
        | None, None ->
          Typing.context_lookup ctx.cg_lt_ctx ident
          |> unwrap_or_err ("Undefined identifier: " ^ ident) e.span
          |> Typing.use_binding |> Typing.sync_data graph ctx.current
      )

  | Assign (lval, e') ->
    let td = construct_graphIR graph ci ctx e' in
    let lvi = lvalue_info_of graph ci ctx e lval in
    let err_string = DTypeCheck.fmt_assign lval lvi.lval_dtype td.ld.dtype in
    check_dtype err_string (Some lvi.lval_dtype) td.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
    ctx.current.actions <- (RegAssign (lvi, td.ld) |> tag_with_span e.span)::ctx.current.actions;
    AstAnnotator.attach_event e ctx.current None (Some [DelayConst 1]);
    Typing.cycles_data graph 1 ctx.current
  | Call (id, arg_list) ->
      let func = List.find_opt (fun (f: Lang.func_def) -> f.name = id) ci.func_defs
        |> unwrap_or_err ("Undefined function: " ^ id) e.span in
      AstAnnotator.attach_def_from_top_level_func e func;
      let td_args = List.map (construct_graphIR graph ci ctx) arg_list in
      let ctx' = BuildContext.clear_bindings ctx |> ref in
      if List.length td_args <> List.length func.args then
        raise (event_graph_error_default "Arguments missing in function call" e.span);
        List.iter2 (fun td arg ->
        (
          let _ = td.ld.w in (* added for tc*)
          match arg.arg_type with
            | Some gtype ->
              let err_string = DTypeCheck.fmt_func_arg id arg.arg_name gtype td.ld.dtype in
              check_dtype err_string (Some gtype) td.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
            | None -> ()
        );
        ctx' := BuildContext.add_binding !ctx' arg.arg_name td.ld (AstAnnotator.to_def_span e.span (Some ci.file_name))
      ) td_args func.args;
      construct_graphIR graph ci !ctx' func.body
  | Binop (binop, e1, e2) ->
    (
      let td1 = construct_graphIR graph ci ctx e1 in
      let w1 = unwrap_or_err "Invalid value" e1.span td1.ld.w in
      match e2 with
      | `List ws2 ->
        let td2_list = List.map (fun e2 ->
          let td2 = construct_graphIR graph ci ctx e2 in
          td2
        ) ws2 in
        let w2_list = List.map (fun td2 ->
          unwrap_or_err "Invalid value of second operator" e1.span td2.ld.w
        ) td2_list in
        let (wires', w) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs binop w1 (`List w2_list) graph.wires in
        graph.wires <- wires';
        let new_dtype = `Array (`Logic, ParamEnv.Concrete w.size) in
        Typing.merged_data graph (Some w) new_dtype ctx.current (List.concat [[td1.ld]; List.map (fun td -> td.ld) td2_list])
      | `Single e ->
        let td2 = construct_graphIR graph ci ctx e in
        let w2 = unwrap_or_err "Invalid value of second operator" e1.span td2.ld.w in
        let (wires', w) = WireCollection.add_binary graph.thread_id ci.typedefs ci.macro_defs binop w1 (`Single w2) graph.wires in
        graph.wires <- wires';
        let err_string = DTypeCheck.fmt_binop binop td1.ld.dtype td2.ld.dtype in
        check_dtype err_string (Some td1.ld.dtype) td2.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        
        let new_dtype = match binop with
        | LAnd | LOr | Lt | Gt | Lte | Gte | Eq | Neq ->
          `Logic
        | _ ->
          td1.ld.dtype
        in 
        
        Typing.merged_data graph (Some w) new_dtype ctx.current [td1.ld; td2.ld]
    )
  | Unop (unop, e') ->
    let td = construct_graphIR graph ci ctx e' in
    let w' = unwrap_or_err "Invalid value" e'.span td.ld.w in
    let (wires', w) = WireCollection.add_unary graph.thread_id ci.typedefs unop w' graph.wires in
    graph.wires <- wires';
    Typing.derived_data (Some w) td.ld
  | Tuple [] -> Typing.const_data graph None (unit_dtype) ctx.current
  | Let (_idents, _, e) ->
    raise (Except.TypeError [Text "Let expressions cannot be unused!"; Except.codespan_local e.span]);
    Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
  | Join (e1, e2) ->
    (
      match e1.d with
      | Let (["_"], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_simple td1.ld.dtype in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let td = construct_graphIR graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.ld.lt td.ld.lt in
        {ld = {td.ld with lt}} (* forcing the bound value to be awaited *)
      | Let ([], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_mismatch_opt ~context:"Invalid data type" ~expected:dtype ~got:td1.ld.dtype in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let td = construct_graphIR graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.ld.lt td.ld.lt in
        {ld = {td.ld with lt}} (* forcing the bound value to be awaited *)
      | Let ([ident], dtype, inner_e) ->
          let td1 = construct_graphIR graph ci ctx inner_e in
          let err_string = DTypeCheck.fmt_let_binding ident dtype td1.ld.dtype in
          check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
          let ctx' = BuildContext.add_binding ctx ident td1.ld (AstAnnotator.to_def_span e1.span (Some ci.file_name)) in
          let td = construct_graphIR graph ci ctx' e2 in
          (* check if the binding is used *)
          let binding = Typing.context_lookup ctx'.cg_lt_ctx ident |> Option.get in
          (* no need to wait if the value is current *)
          if td1.ld.lt.live.id <> ctx.current.id && not binding.binding_used then (
            (* raise (event_graph_error_default "Value is potentially not awaited!" e1.span) *)
            Printf.eprintf "[Warning] Value bound to %s is potentially not awaited!\n" ident;
            (SpanPrinter.print_code_span ~indent:2 ~trunc:(-5) stderr ci.file_name e1.span)
          ;
          );
          td
      | Let _ ->
        raise (event_graph_error_default "Discarding expression results!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
      | _ ->
        let td1 = construct_graphIR graph ci ctx e1 in
        let td = construct_graphIR graph ci ctx e2 in
        let lt = Typing.lifetime_intersect graph td1.ld.lt td.ld.lt in
        {ld = {td.ld with lt}} (* forcing the bound value to be awaited *)
    )
  | Wait (e1, e2) ->
    (
      match e1.d with
      | Let (["_"], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_simple td1.ld.dtype in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        construct_graphIR graph ci ctx' e2
      | Let ([], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_simple td1.ld.dtype in
        check_dtype err_string dtype td1.ld.dtype e1.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        construct_graphIR graph ci ctx' e2
      | Let ([ident], dtype, inner_e) ->
        let td1 = construct_graphIR graph ci ctx inner_e in
        let err_string = DTypeCheck.fmt_let_binding ident dtype td1.ld.dtype in
        check_dtype err_string dtype td1.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
        (* add the binding to the context *)
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        let ctx' = BuildContext.add_binding ctx' ident td1.ld (AstAnnotator.to_def_span e1.span (Some ci.file_name)) in
        construct_graphIR graph ci ctx' e2
      | Let _ ->
        raise (event_graph_error_default "Discarding expression results!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
      | _ ->
        let td1 = construct_graphIR graph ci ctx e1 in
        let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
        construct_graphIR graph ci ctx' e2
    )
  | Ready msg_spec ->
    let msg = MessageCollection.lookup_message graph.messages msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in ready" e.span in
    AstAnnotator.attach_def_from_top_level_message e msg msg_spec graph;
    AstAnnotator.attach_event e ctx.current None None;
    (* if msg.dir <> In then (
      (* mismatching direction *)
      raise (event_graph_error_default "Mismatching message direction!" e.span)
    ); *)
    let wires, msg_valid_port = WireCollection.add_msg_valid_port graph.thread_id ci.typedefs msg_spec graph.wires in
    graph.wires <- wires;
    Typing.immediate_data graph (Some msg_valid_port) `Logic ctx.current
  | Probe msg_spec ->
    let msg = MessageCollection.lookup_message graph.messages msg_spec ci.channel_classes
    |> unwrap_or_err "Invalid message specifier in probe" e.span in
    AstAnnotator.attach_def_from_top_level_message e msg msg_spec graph;
    AstAnnotator.attach_event e ctx.current None None;
    let wires, msg_ack_port = WireCollection.add_msg_ack_port graph.thread_id ci.typedefs msg_spec graph.wires in
    graph.wires <- wires;
    Typing.immediate_data graph (Some msg_ack_port) `Logic ctx.current
  | Cycle n ->
    let data = Typing.cycles_data graph n ctx.current in
    AstAnnotator.attach_event e ctx.current None (Some [DelayConst n]);
    data
  | IfExpr (e1, e2, e3) ->
    let td1 = construct_graphIR graph ci ctx e1 in
    (* TODO: type checking *)
    let w1 = unwrap_or_err "Invalid condition" e1.span td1.ld.w in
    let ctx' = BuildContext.wait graph ctx td1.ld.lt.live in
    let branch_info = {
      branch_cond_v = td1.ld;
      branch_cond = TrueFalse;
      branch_count = 2; (* for if-else *)
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true) = BuildContext.branch_side graph ctx' branch_info 0 in
    let td2 = construct_graphIR graph ci ctx_true e2 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx' branch_info 1 in
    let td3 = construct_graphIR graph ci ctx_false e3 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td2.ld.lt.live; td3.ld.lt.live];

    BuildContext.branch_merge ctx' [ctx_true; ctx_false];
    AstAnnotator.attach_event e ctx.current None None;

    let ctx_br = BuildContext.branch graph ctx' branch_info in
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.ld.lt.dead td2.ld.lt.dead |> Utils.list_unordered_join td3.ld.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.ld.reg_borrows td2.ld.reg_borrows |> Utils.list_unordered_join td3.ld.reg_borrows in
    (
      match td2.ld.w, td3.ld.w with
      | None, None -> {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
      | Some w2, Some w3 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w1, w2)] w3 graph.wires in
        graph.wires <- wires';
        {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.ld.dtype}}
      | _ ->
        raise (event_graph_error_default "Invalid if expression!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | TrySend (send_pack, e1, e2) ->
    (* data to send *)
    let td_send_data = construct_graphIR graph ci ctx send_pack.send_data in

    let wires, w_cond = WireCollection.add_msg_ack_port graph.thread_id ci.typedefs send_pack.send_msg_spec graph.wires in
    let td_cond = {
      w = Some w_cond;
      lt = {live = ctx.current; dead = [(ctx.current, `Cycles 1)]};
      reg_borrows = [];
      dtype = `Logic;
    } in
    graph.wires <- wires;

    (* TODO: dedup *)
    let branch_info = {
      branch_cond_v = td_cond;
      branch_cond = TrueFalse;
      branch_count = 2;
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true) = BuildContext.branch_side graph ctx branch_info 0 in
    let td1 = construct_graphIR graph ci ctx_true e1 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx branch_info 1 in
    let td2 = construct_graphIR graph ci ctx_false e2 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td1.ld.lt.live; td2.ld.lt.live];

    BuildContext.branch_merge ctx [ctx_true; ctx_false];

    ctx_true.current.actions <- (ImmediateSend (send_pack.send_msg_spec, td_send_data.ld) |> tag_with_span e.span)::ctx_true.current.actions;
    AstAnnotator.attach_event e ctx_true.current None None;

    (
      try let msg = MessageCollection.lookup_message graph.messages send_pack.send_msg_spec ci.channel_classes
        |> unwrap_or_err "Invalid message specifier in try send" e.span
      in AstAnnotator.attach_def_from_top_level_message e msg send_pack.send_msg_spec graph;
      with _ -> ()
    );

    let ctx_br = BuildContext.branch graph ctx branch_info in
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.ld.lt.dead td2.ld.lt.dead |> Utils.list_unordered_join td_cond.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.ld.reg_borrows td2.ld.reg_borrows |> Utils.list_unordered_join td_cond.reg_borrows in
    (
      match td1.ld.w, td2.ld.w with
      | None, None -> {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
      | Some w1, Some w2 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w_cond, w1)] w2 graph.wires in
        graph.wires <- wires';
        {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.ld.dtype}}
      | _ ->
        raise (event_graph_error_default "Invalid try send expression!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | TryRecv (ident, recv_pack, e1, e2) ->
    let wires, w_cond = WireCollection.add_msg_valid_port graph.thread_id ci.typedefs recv_pack.recv_msg_spec graph.wires in
    let td_cond = {
      w = Some w_cond;
      lt = {live = ctx.current; dead = [(ctx.current, `Cycles 1)]};
      reg_borrows = [];
      dtype = `Logic;
    } in
    graph.wires <- wires;

    (* TODO: dedup *)
    let branch_info = {
      branch_cond_v = td_cond;
      branch_cond = TrueFalse;
      branch_count = 2;
      branches_to = [];
      branches_val = [];
    } in

    let (br_side_true, ctx_true_no_binding) = BuildContext.branch_side graph ctx branch_info 0 in

    (* message *)
    let msg = MessageCollection.lookup_message graph.messages recv_pack.recv_msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in try receive" e.span in
    let (wires', w_recv) = WireCollection.add_msg_port graph.thread_id ci.typedefs ci.macro_defs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let stype = List.hd msg.sig_types in
    let d_recv = delay_pat_globalise recv_pack.recv_msg_spec.endpoint stype.lifetime.e in
    let td_recv = {
      w = w_recv;
      lt = {live = ctx_true_no_binding.current; dead = [(ctx_true_no_binding.current, d_recv)]};
      reg_borrows = [];
      dtype = stype.dtype;
    } in
    let ctx_true = BuildContext.add_binding ctx_true_no_binding ident td_recv (AstAnnotator.to_def_span e1.span (Some ci.file_name)) in
    let td1 = construct_graphIR graph ci ctx_true e1 in
    let (br_side_false, ctx_false) = BuildContext.branch_side graph ctx branch_info 1 in
    let td2 = construct_graphIR graph ci ctx_false e2 in

    branch_info.branches_to <- [ctx_true.current; ctx_false.current];
    branch_info.branches_val <- [td1.ld.lt.live; td2.ld.lt.live];

    BuildContext.branch_merge ctx [ctx_true_no_binding; ctx_false];

    let action = ImmediateRecv recv_pack.recv_msg_spec |> tag_with_span e.span in
    ctx_true.current.actions <- action::ctx_true.current.actions;
    AstAnnotator.attach_event e ctx_true.current None None;
    AstAnnotator.attach_def_from_top_level_message e msg recv_pack.recv_msg_spec graph;

    let ctx_br = BuildContext.branch graph ctx branch_info in
    br_side_true.branch_event <- Some ctx_br.current;
    br_side_false.branch_event <- Some ctx_br.current;
    let lt = {live = ctx_br.current; (* branch event is reached when either split event is reached *)
      dead = Utils.list_unordered_join td1.ld.lt.dead td2.ld.lt.dead |> Utils.list_unordered_join td_cond.lt.dead} in
    let reg_borrows' = Utils.list_unordered_join td1.ld.reg_borrows td2.ld.reg_borrows |> Utils.list_unordered_join td_cond.reg_borrows in
    (
      match td1.ld.w, td2.ld.w with
      | None, None -> {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
      | Some w1, Some w2 ->
      (* TODO: check that the data types are the same *)
        let (wires', w) = WireCollection.add_switch graph.thread_id ci.typedefs [(w_cond, w1)] w2 graph.wires in
        graph.wires <- wires';
        {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td2.ld.dtype}}
      | _ ->
        raise (event_graph_error_default "Invalid try recv expression!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )

  | Match (match_v, match_arms) ->
    let td_v = construct_graphIR graph ci ctx match_v in
    let w_v = unwrap_or_err "Invalid match expression" match_v.span td_v.ld.w in
    let ctx' = BuildContext.wait graph ctx td_v.ld.lt.live in
    let branch_info = {
      branch_cond_v = td_v.ld;
      branch_cond = MatchCases [];
      branch_count = List.length match_arms;
      branches_to = [];
      branches_val = [];
    } in

    let (default_arms, match_arms) =
      List.partition (fun ((pat, _) : expr_node * expr_node option) -> pat.d = Identifier "_") match_arms in
    (
      match default_arms with
      | [default_arm] ->
        let td_pats = List.map (fun (pat, _) -> construct_graphIR graph ci ctx' pat) match_arms in
        (* make sure that the patterns are all constants *)
        List.iter2
          (fun ((pat, _) : expr_node * expr_node option) td ->
            let valid =
              match td.ld.w with
              | None -> false
              | Some w -> w.is_const
            in
            if not valid then
              raise (event_graph_error_default "Match patterns must be constant values!" pat.span)
          ) match_arms td_pats;

        (* build bodies *)
        let branches = List.mapi
          (fun idx (_, body) ->
            let (br_side, ctx) = BuildContext.branch_side graph ctx' branch_info idx in
            let td = construct_graphIR graph ci ctx @@ Option.get body in
            ((ctx, br_side, td), (ctx.current, td.ld.lt.live))
          )
          match_arms
        in
        let (br_side_default, ctx_default) = BuildContext.branch_side graph ctx' branch_info (branch_info.branch_count - 1) in
        let td_default = construct_graphIR graph ci ctx_default @@ Option.get @@ snd default_arm in

        let (branches, branches_events) = List.split branches in
        let (branches_to, branches_val) = List.split branches_events in
        branch_info.branches_to <- branches_to @ [ctx_default.current];
        branch_info.branches_val <- branches_val @ [td_default.ld.lt.live];

        BuildContext.branch_merge ctx' @@ ctx_default::(List.map (fun (ctx, _, _) -> ctx) branches);
        let ctx_br = BuildContext.branch graph ctx' branch_info in
        List.iter
          (fun (_, br_side, _) ->
            br_side.branch_event <- Some ctx_br.current
          )
          branches;
        br_side_default.branch_event <- Some ctx_br.current;
        let lt = {
          live = ctx_br.current;
          dead = List.fold_left (fun l (_, _, td) -> Utils.list_unordered_join l td.ld.lt.dead) td_v.ld.lt.dead branches;
        } in
        let reg_borrows' = List.fold_left (fun l (_, _, td) -> Utils.list_unordered_join l td.ld.reg_borrows) td_v.ld.reg_borrows branches in
        branch_info.branch_cond <- MatchCases (List.map (fun td -> td.ld) td_pats);
        if List.for_all (fun (_, _, td_val) -> Option.is_none td_val.ld.w) branches then
          {ld = {w = None; lt; reg_borrows = reg_borrows'; dtype = unit_dtype}}
        else (
          let (wires', w) = WireCollection.add_cases graph.thread_id ci.typedefs w_v
            (List.map2 (fun td_pat (_, _, td_val) -> (Option.get td_pat.ld.w, Option.get td_val.ld.w)) td_pats branches) (match td_default.ld.w with | Some w -> w | None -> raise_fatal (event_graph_error_default "Invalid match expression (exactly one default case expected)!" e.span))
            graph.wires in
          graph.wires <- wires';
          {ld = {w = Some w; lt; reg_borrows = reg_borrows'; dtype = td_default.ld.dtype}}
        )
      | _ ->
          raise @@ event_graph_error_default "Invalid match expression (atleast one default case expected)!" e.span;
          Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | Cast (e', dtype) ->
    let td = construct_graphIR graph ci ctx e' in
    let w = unwrap_or_err "Invalid value in cast" e'.span td.ld.w in
    let target_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs dtype in
    if w.size > target_size then (
      (* Truncate: take slice of the wire to match target size *)
      let base_i = MaybeConst.Const 0 in
      let wires', w' = WireCollection.add_slice graph.thread_id w base_i target_size graph.wires in
      graph.wires <- wires';
      Typing.merged_data graph (Some w') dtype ctx.current [td.ld]
    ) else if w.size < target_size then (
      (* Extend: pad with zeros to match target size *)
      let pad_len = target_size - w.size in
      let wires', pad_w = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs (WithLength (pad_len, 0)) graph.wires in
      let wires', w' = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [pad_w; w] wires' in
      graph.wires <- wires';
      Typing.merged_data graph (Some w') dtype ctx.current [td.ld]
    ) else (
      (* Sizes match, just update type *)
      Typing.merged_data graph (Some w) dtype ctx.current [td.ld]
    )
  | Concat (es, is_flat) ->
    let tds = List.map (fun e' -> (e', construct_graphIR graph ci ctx e')) es in
    let ws = List.map (fun ((e', td) : expr_node * node_data) -> unwrap_or_err "Invalid value in concat" e'.span td.ld.w) tds in
    let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
    graph.wires <- wires';
    let tdtype = List.map (fun ((_, td): expr_node * node_data) -> td.ld.dtype) tds in
    if not (List.for_all (fun d -> d = List.hd tdtype) tdtype) && (not is_flat) then
      raise (Except.TypeError [Text ("In concat: Incompatible types: " ^ (String.concat ", " (List.map string_of_data_type tdtype))); Except.codespan_local e.span]);
    let new_dtype = ( 
      match is_flat with 
      | false -> `Array (List.hd tdtype, ParamEnv.Concrete (List.length es))
      | _ -> `Array (`Logic, ParamEnv.Concrete (w.size))
    ) in
    AstAnnotator.attach_event e ctx.current None None;
    List.map (fun (_,td) -> td.ld) tds |> Typing.merged_data graph (Some w) new_dtype ctx.current
  |  Read rlval ->
    let reg_ident = Lang.get_lvalue_reg_id rlval in
    let r = Utils.StringMap.find_opt reg_ident graph.regs
      |> unwrap_or_err ("Undefined register " ^ reg_ident) e.span in
    AstAnnotator.attach_def_span_expr e r (Some ci.file_name);
    let (wires'', w'') = WireCollection.add_reg_read graph.thread_id ci.typedefs ci.macro_defs r.d graph.wires in
    graph.wires <- wires'';
    let td = {ld = {w = Some w''; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = r.d.d_type}} in
    let get_borrow_info in_off le dt w lval td' =
      match lval with 
        | Reg _ -> (dt,in_off,le,w,td')
        | Indexed (lv, idx) ->
          let inner_info = lvalue_info_of graph ci ctx e lv in
          (
            let type_def = TypedefMap.type_def_name_resolve ci.typedefs inner_info.lval_dtype in
            match type_def with
            | Some type_def ->
                AstAnnotator.attach_def_from_top_level_type e type_def
            | None -> ()
          );
          let inner_le = fst inner_info.lval_range.subreg_range_interval in
          let inner_le_nd = MaybeConst.map (fun ld -> {ld}) inner_le in
          let inner_dtype = inner_info.lval_dtype in
          let (offset_le, len, dt') = TypedefMap.data_type_index ci.typedefs ci.macro_defs
            (construct_graphIR graph ci ctx)
            (binop_td_const e.span Mul)
            inner_dtype idx |> unwrap_or_err (Printf.sprintf "Invalid indexing %s for datatype %s" (string_of_index idx) (Lang.string_of_data_type inner_dtype)) e.span in
          let total_offset_le = MaybeConst.add (binop_td_const e.span Add) (_binop_td_td e.span Add) offset_le inner_le_nd in
          let off_i = MaybeConst.map_off total_offset_le in
          let (off, le') = if off_i < 0 then (
            Printf.eprintf "[Warning] The offset is not a constant value for %s, borrowing full range\n" reg_ident;
            (0, len)
          ) else (off_i, len) in
          let (wire', w') = WireCollection.add_slice graph.thread_id w (MaybeConst.map (fun td -> unwrap_or_err "Invalid indexing in lvalue" e.span td.ld.w) total_offset_le) le' graph.wires in
          graph.wires <- wire';
          let new_td = match total_offset_le with
            | MaybeConst.NonConst td_offset -> Typing.merged_data graph (Some w') dt' ctx.current [td'.ld; td_offset.ld]
            | MaybeConst.Const _ -> {ld = {td'.ld with w = Some w'; dtype = dt'}}
          in
          (dt', off, le', w', new_td)
        | Indirected (lval_inner, field_id) -> 
          let lval_info_inner = lvalue_info_of graph ci ctx e lval_inner in
          (
            let type_def = TypedefMap.type_def_name_resolve ci.typedefs lval_info_inner.lval_dtype in
            match type_def with
            | Some type_def ->
                AstAnnotator.attach_def_from_top_level_type_with_fields e type_def [(field_id.d, field_id)]
            | None -> ()
          );
          let (inner_le, _inner_len) = lval_info_inner.lval_range.subreg_range_interval in
          let inner_le_nd = MaybeConst.map (fun ld -> {ld}) inner_le in
          let (field_offset_le, len, new_dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs lval_info_inner.lval_dtype field_id.d
            |> unwrap_or_err (Printf.sprintf "Invalid indirection %s" field_id.d) e.span in
          (* total offset = inner offset + field offset *)
          let total_offset_le = MaybeConst.add_const field_offset_le (binop_td_const e.span Add) inner_le_nd in
          let off_i = MaybeConst.map_off total_offset_le in
          let (off, le') = if off_i < 0 then (
            Printf.eprintf "[Warning] The offset is not a constant value for indirection %s, borrowing full range\n" field_id.d;
            (0, len)
          ) else (off_i, len) in
          let (wi', new_w) = WireCollection.add_slice graph.thread_id w (MaybeConst.map (fun td -> unwrap_or_err "Invalid indexing in indirection" e.span td.ld.w) total_offset_le) le' graph.wires in
          graph.wires <- wi';
          let new_td = match total_offset_le with
            | MaybeConst.Const _ -> {ld = {td'.ld with w = Some new_w; dtype = new_dtype}}
            | MaybeConst.NonConst td_offset -> Typing.merged_data graph (Some new_w) new_dtype ctx.current [td'.ld; td_offset.ld]
          in
          (new_dtype, off, le', new_w, new_td)
    in
    let full_sz = TypedefMap.data_type_size ci.typedefs ci.macro_defs r.d.d_type in
    let (_dt,off,le,_w,td'') = get_borrow_info 0 full_sz r.d.d_type w'' rlval td in
    let borrow = {borrow_range = sub_reg_range reg_ident off le; borrow_start = ctx.current; borrow_source_span = e.span} in
    {ld = { td''.ld with dtype = _dt; reg_borrows = borrow :: td''.ld.reg_borrows }}
  | Debug op ->
    (
      match op with
      | DebugPrint (s, e_list) ->
        let timed_ws = List.map (construct_graphIR graph ci ctx) e_list in
        let all_w = List.for_all(
          fun td ->
            match td.ld.w with
            | Some _ -> true
            | None -> false
        ) timed_ws in 
        if not all_w then
          raise (Except.TypeError [Text "Invalid value in debug print"; Except.codespan_local e.span]);
        ctx.current.actions <- (let open EventGraph in DebugPrint (s, List.map (fun td -> td.ld) timed_ws) |> tag_with_span e.span)::ctx.current.actions;
        AstAnnotator.attach_event e ctx.current None None;
        {ld = {w = None; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}}
      | DebugFinish ->
        ctx.current.actions <- (let open EventGraph in tag_with_span e.span DebugFinish)::ctx.current.actions;
        AstAnnotator.attach_event e ctx.current None None;
        {ld = {w = None; lt = EventGraphOps.lifetime_const ctx.current; reg_borrows = []; dtype = unit_dtype}}
    )
  | Send send_pack ->
    (* just check that the endpoint and the message type is defined *)
    let ep  = send_pack.send_msg_spec.endpoint in
    if not (MessageCollection.endpoint_owned graph.messages ep) then
      raise (event_graph_error_default (Printf.sprintf "Endpoint %s not owned by the process" ep) e.span);
    let msg = MessageCollection.lookup_message graph.messages send_pack.send_msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in send" e.span in

    AstAnnotator.attach_def_from_top_level_message e msg send_pack.send_msg_spec graph;

    if msg.dir <> Out then (
      (* mismatching direction *)
      raise (event_graph_error_default "Mismatching message direction!" e.span)
    );
    let td = construct_graphIR graph ci ctx send_pack.send_data in
    let msg_dtype = (List.hd msg.sig_types).dtype in
    let err_string = DTypeCheck.fmt_send send_pack.send_msg_spec.endpoint msg_dtype td.ld.dtype in
    check_dtype err_string (Some msg_dtype) td.ld.dtype e.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
    let ntd = Typing.send_msg_data graph send_pack.send_msg_spec ctx.current in
    ctx.current.sustained_actions <-
      ({
        until = ntd.ld.lt.live;
        ty = Send (send_pack.send_msg_spec, td.ld)
      } |> tag_with_span e.span)::ctx.current.sustained_actions;
    AstAnnotator.attach_event e ctx.current (Some ntd.ld.lt.live) None;
    ntd

  | Recv recv_pack ->
    let ep  = recv_pack.recv_msg_spec.endpoint in
    if not (MessageCollection.endpoint_owned graph.messages ep) then
      raise (event_graph_error_default (Printf.sprintf "Endpoint %s not owned by the process" ep) e.span);
    let msg = MessageCollection.lookup_message graph.messages recv_pack.recv_msg_spec ci.channel_classes
      |> unwrap_or_err "Invalid message specifier in receive" e.span in

    AstAnnotator.attach_def_from_top_level_message e msg recv_pack.recv_msg_spec graph;

    if msg.dir <> Inp then (
      (* mismatching direction *)
      raise (event_graph_error_default "Mismatching message direction!" e.span)
    );
    let (wires', w) = WireCollection.add_msg_port graph.thread_id ci.typedefs ci.macro_defs recv_pack.recv_msg_spec 0 msg graph.wires in
    graph.wires <- wires';
    let ntd = Typing.recv_msg_data graph w recv_pack.recv_msg_spec msg ctx.current in
    ctx.current.sustained_actions <-
      ({until = ntd.ld.lt.live; ty = Recv recv_pack.recv_msg_spec} |> tag_with_span e.span)::ctx.current.sustained_actions;
    AstAnnotator.attach_event e ctx.current (Some ntd.ld.lt.live) None;
    ntd

  | Indirect (e', fieldname) ->
    let td = construct_graphIR graph ci ctx e' in
    (
      let type_def = TypedefMap.type_def_name_resolve ci.typedefs td.ld.dtype in
      match type_def with
      | Some type_def ->
          AstAnnotator.attach_def_from_top_level_type_with_fields e' type_def [(fieldname.d, fieldname)]
      | None -> ()
    );
    let w = unwrap_or_err "Invalid value in indirection" e'.span td.ld.w in
    let (offset_le, len, new_dtype) = TypedefMap.data_type_indirect ci.typedefs ci.macro_defs td.ld.dtype fieldname.d
      |> unwrap_or_err (Printf.sprintf "Invalid indirection %s" fieldname.d) e.span in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w (Const offset_le) len graph.wires in
    graph.wires <- wires';
    {ld = {
      td.ld with
      w = Some new_w;
      dtype = new_dtype
    }
    }
  | Index (e', ind) ->
    let td = construct_graphIR graph ci ctx e' in
    let w = unwrap_or_err "Invalid value in indexing" e'.span td.ld.w in
    let (offset_le, len, new_dtype) =
      TypedefMap.data_type_index ci.typedefs ci.macro_defs
        (construct_graphIR graph ci ctx)
        (binop_td_const e.span Mul)
        td.ld.dtype ind
      |> unwrap_or_err (Printf.sprintf "Invalid indexing %s for datatype %s" (string_of_index ind) (Lang.string_of_data_type td.ld.dtype)) e.span in
    let wire_of (td:node_data) = unwrap_or_err (Printf.sprintf "Invalid indexing for %s in data type %s" (string_of_index ind) (Lang.string_of_data_type td.ld.dtype)) e.span td.ld.w in
    let offset_le_w = MaybeConst.map wire_of offset_le in
    let (wires', new_w) = WireCollection.add_slice graph.thread_id w offset_le_w len graph.wires in
    graph.wires <- wires';
    (
      match offset_le with
      | Const _ -> {ld = {td.ld with w = Some new_w; dtype = new_dtype}}
      | NonConst td_offset ->
          Typing.merged_data graph (Some new_w) new_dtype ctx.current [td.ld; td_offset.ld]
    )
  | Record (record_ty_name, field_exprs, None) ->
    (
      match TypedefMap.type_def_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
      | Some type_def ->
          let field_exprs_flattened = List.map (fun n -> let (ident, _) = n.d in (ident, n)) field_exprs in
          AstAnnotator.attach_def_from_top_level_type_with_fields e type_def field_exprs_flattened
      | None -> ()
    );

    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
      | Some (`Record record_fields) ->
        (
          match Utils.list_match_reorder
            (List.map (fun n -> fst n.d) record_fields)
            (List.map (fun n -> n.d) field_exprs) with
          | Some expr_reordered ->
            let tds = List.map2 (fun n e' ->
              let field_name, expected_dtype = n.d in
              let td = construct_graphIR graph ci ctx e' in
              let err_string = DTypeCheck.fmt_record_field field_name expected_dtype td.ld.dtype in
              check_dtype err_string (Some expected_dtype) td.ld.dtype e'.span ci.file_name ci.weak_typecasts ci.typedefs ci.macro_defs;
              (e', td)
            ) record_fields expr_reordered in
            let ws = List.rev_map (fun ((e', td) : expr_node * node_data) ->
              unwrap_or_err "Invalid value in record field" e'.span td.ld.w) tds in
            let (wires', w) = WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs ws graph.wires in
            graph.wires <- wires';
            List.map (fun (_, td) -> td.ld) tds |> Typing.merged_data graph (Some w) (`Named (record_ty_name, [])) ctx.current
          | _ ->
            raise (event_graph_error_default "Invalid record type value!" e.span);
            Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
        )
      | _ ->
          raise (event_graph_error_default "Invalid record type name!" e.span);
          Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | Record (record_ty_name, field_exprs, Some field_base) ->
      (
        match TypedefMap.type_def_name_resolve ci.typedefs @@ `Named (record_ty_name, []) with
        | Some type_def ->
            let field_exprs_flattened = List.map (fun n -> let (ident, _) = n.d in (ident, n)) field_exprs in
            AstAnnotator.attach_def_from_top_level_type_with_fields e type_def field_exprs_flattened
        | None -> ()
      );

      (* record update *)
      let td_base = construct_graphIR graph ci ctx field_base in
      let tds = List.map (fun n -> let (field_ident, e') = n.d in (field_ident, e', construct_graphIR graph ci ctx e')) field_exprs in
      let updates =
        (* bruteforce *)
        List.map (fun (field_ident, _e', td) ->
          match TypedefMap.data_type_indirect ci.typedefs ci.macro_defs (`Named (record_ty_name, [])) field_ident with
          | None -> 
            let err_string = Printf.sprintf "In record update: Invalid field %s for record type %s" field_ident record_ty_name in
             raise_fatal (event_graph_error_default err_string e.span)
          | Some (offset_le, len, _dtype) -> (offset_le, len, Option.get td.ld.w)
        ) tds in
      let (wires', w) = WireCollection.add_update graph.thread_id ci.typedefs (Option.get td_base.ld.w) updates graph.wires in
      graph.wires <- wires';
      Typing.merged_data graph (Some w) (`Named (record_ty_name, [])) ctx.current (td_base.ld::(List.map (fun (_, _, td) -> td.ld) tds))
  | Construct (cstr_spec, cstr_expr_opt) ->
    (
      match TypedefMap.type_def_name_resolve ci.typedefs @@ `Named (cstr_spec.variant_ty_name, []) with
      | Some type_def -> AstAnnotator.attach_def_from_top_level_type_with_fields e type_def [(cstr_spec.variant.d, cstr_spec.variant)]
      | None -> ()
    );
    (
      match TypedefMap.data_type_name_resolve ci.typedefs @@ `Named (cstr_spec.variant_ty_name, []) with
      | Some (`Variant (dtype_opt, variants)) ->
        let variant_val = `Variant (dtype_opt, variants) in
        let e_dtype_opt = variant_lookup_dtype variant_val cstr_spec.variant.d in
        (
          match e_dtype_opt, cstr_expr_opt with
          | Some e_dtype, Some cstr_expr ->
            let td = construct_graphIR graph ci ctx cstr_expr in
            (* if td.dtype <> e_dtype then
              raise (Except.TypeError [Text ("In variant construction: Invalid data type for " ^ cstr_spec.variant ^ ": expected " ^ (string_of_data_type e_dtype) ^ " got " ^ (string_of_data_type td.dtype)); Except.codespan_local e.span]); *)
            let w = unwrap_or_err "Invalid value in variant construction" cstr_expr.span td.ld.w in
            let tag_size = variant_tag_size variant_val
            and data_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs e_dtype
            and tot_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs variant_val
            and var_idx = variant_lookup_index variant_val cstr_spec.variant.d
              |> unwrap_or_err ("Invalid constructor: " ^ cstr_spec.variant.d) e.span in
            let (wires', w_tag) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
              (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size + data_size then
              (* no padding *)
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w; w_tag] wires'
            else begin
              (* padding needed *)
              let (wires', w_pad) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
                (WithLength (tot_size - tag_size - data_size, 0)) wires' in
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w_pad; w; w_tag] wires'
            end in
            graph.wires <- wires';
            {ld = { td.ld with w = Some new_w }}
          | None, None ->
            let tag_size = variant_tag_size variant_val
            and tot_size = TypedefMap.data_type_size ci.typedefs ci.macro_defs variant_val
            and var_idx = variant_lookup_index variant_val cstr_spec.variant.d
              |> unwrap_or_err ("Invalid constructor: " ^ cstr_spec.variant.d) e.span in
            let (wires', w_tag) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
              (WithLength (tag_size, var_idx)) graph.wires in
            let (wires', new_w) = if tot_size = tag_size then
              (wires', w_tag)
            else begin
              let (wires', w_pad) = WireCollection.add_literal graph.thread_id ci.typedefs ci.macro_defs
                (WithLength (tot_size - tag_size, 0)) wires' in
              WireCollection.add_concat graph.thread_id ci.typedefs ci.macro_defs [w_pad; w_tag] wires'
            end in
            graph.wires <- wires';
            Typing.const_data graph (Some new_w)  (`Named (cstr_spec.variant_ty_name, [])) ctx.current
          | _ ->
            raise (event_graph_error_default "Invalid variant construct expression!" e.span);
            Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
        )
      | _ ->
        raise (event_graph_error_default "Invalid variant type name!" e.span);
        Typing.const_data graph None (unit_dtype) ctx.current (* dummy return *)
    )
  | SharedAssign (id, value_expr) ->
    let shared_info = Hashtbl.find_opt ctx.shared_vars_info id
      |> unwrap_or_err ("Undefined identifier " ^ id) e.span in
    if graph.thread_id = shared_info.assigning_thread then
      let value_td = construct_graphIR graph ci ctx value_expr in
      if not ctx.lt_check_phase then (
        if (Option.is_some shared_info.value.w) || (Option.is_some shared_info.assigned_at) then (
          let prev_assign_action =
            (Option.get shared_info.assigned_at).actions
            |> List.find (fun {d; _} ->
              match d with
              | PutShared (id', _, _) -> id' = id
              | _ -> false
            )
          in
          raise (EventGraphError [
            Text "Shared value can only be assigned in one place!";
            Except.codespan_local e.span;
            Text "Previously assigned at:";
            Except.codespan_local prev_assign_action.span
          ]);
        );
        shared_info.value.w <- value_td.ld.w;
        shared_info.assigned_at <- Some ctx.current;
      );
      ctx.current.actions <- (PutShared (id, shared_info, value_td.ld) |> tag_with_span e.span)::ctx.current.actions;
      AstAnnotator.attach_event e ctx.current None None;
    else
      raise (event_graph_error_default "Shared variable assigned in wrong thread" e.span);
    Typing.const_data graph None (unit_dtype) ctx.current
  | List li ->
    let tds = List.map (construct_graphIR graph ci ctx) li in
    let ws = List.map (fun td -> unwrap_or_err "Invalid wires!" e.span td.ld.w) tds in
    let (wires', new_w) = WireCollection.add_list graph.thread_id ci.typedefs ws graph.wires
      |> unwrap_or_err "Invalid list!" e.span in
    graph.wires <- wires';
    let td = List.hd tds in
    Typing.merged_data graph (Some new_w) (`Array (td.ld.dtype, ParamEnv.Concrete (List.length tds))) ctx.current (List.map (fun td -> td.ld) tds)
  | Recurse ->
    ctx.current.is_recurse <- true;
    Typing.const_data graph None unit_dtype ctx.current
  | Tuple _ ->
    raise (event_graph_error_default "Unimplemented expression!" e.span);
    Typing.const_data graph None unit_dtype ctx.current (* dummy return *)

