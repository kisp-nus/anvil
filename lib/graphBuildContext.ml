open EventGraph
open EventGraphOps
open Lang


module Typing = struct
  
  type 'a binding  = {
    binding_val : 'a;
    mutable binding_used : bool; (** if the binding has been used (to enforce relevance) *)
  }

  let use_binding binding =
    binding.binding_used <- true;
    binding.binding_val

  type 'a context = 'a binding Utils.string_map

  type build_context = {
    cg_lt_ctx : lowering_data context;  (* To Do : We need to seperate codegen and lifetime contexts, but would cause aggressive changes as it flows down*)
    current : event;
    shared_vars_info : (identifier, shared_var_info) Hashtbl.t;
    lt_check_phase : bool;
  }

  let event_create g source =
    let event_create_inner () =
      let parents =
        match source with
        | `Root None -> []
        | `Seq (e, _)
        | `Root Some (e, _) -> [e]
        | `Later (e1, e2) -> [e1; e2]
        | `Branch (_, { branches_val; _ }) -> branches_val
      in
      let new_preds = List.fold_left
        (fun preds e -> Utils.IntSet.union preds e.preds)
        Utils.IntSet.empty
        parents
      in
      let new_preds = Utils.IntSet.add (g.last_event_id + 1) new_preds in
      let n = {actions = []; sustained_actions = []; source; id = g.last_event_id + 1;
        is_recurse = false;
        outs = []; graph = g; preds = new_preds; removed = false } in
      g.events <- n::g.events;
      g.last_event_id <- n.id;
      n
    in
    (
      match source with
      | `Later (e1, e2) -> (
        if Utils.IntSet.mem e1.id e2.preds then
          e2
        else if Utils.IntSet.mem e2.id e1.preds then
          e1
        else
          event_create_inner ()
      )
      | _ -> event_create_inner ()
    )


  let lifetime_intersect g (a : lifetime) (b : lifetime) =
    {
      live = event_create g (`Later (a.live, b.live));
      dead = Utils.list_unordered_join a.dead b.dead;
    }

  let cycles_data g (n : int) (current : event)  =
    let live_event = event_create g (`Seq (current, `Cycles n)) in
    {ld = {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}; reg_borrows = []; dtype = unit_dtype}}
  let sync_data g (current : event) (td: lowering_data) =
    let ev = event_create g (`Later (current, td.lt.live)) in
    {ld = {td with lt = {td.lt with live = ev}}}

  let immediate_data _g (w : wire option) dtype (current : event) = {ld = {w; lt = lifetime_immediate current; reg_borrows = []; dtype}}
  let const_data _g (w : wire option) dtype (current : event) = {ld = {w; lt = lifetime_const current; reg_borrows = []; dtype}}
  let merged_data g (w : wire option) dtype (current : event) (tds : lowering_data list) =
    let lts = List.map (fun x -> x.lt) tds in
    match lts with
    | [] -> const_data g w dtype current
    | lt::lts' ->
      let lt' = List.fold_left (lifetime_intersect g) lt lts' in
      let reg_borrows' = List.concat_map (fun x -> x.reg_borrows) tds in
      {ld = {w; lt = lt'; reg_borrows = reg_borrows'; dtype}}
  let derived_data (w : wire option) (td : lowering_data) = {ld = {td with w}}
  let send_msg_data g (msg : message_specifier) (current : event) =
    let live_event = event_create g (`Seq (current, `Send msg)) in
    {ld = {w = None; lt = {live = live_event; dead = [(live_event, `Eternal)]}; reg_borrows = []; dtype = unit_dtype}}

  let sync_event_data g ident gtd current =
    let event_synced = event_create g (`Seq (current, `Sync ident)) in
    let dpat = gtd.glt.e in
    (
      match dpat with
      | `Cycles _ -> ()
      | _ -> raise (Except.UnimplementedError [Text "Non-static lifetime for shared data is unsupported!"])
    );
    {ld = {w = gtd.w; lt = {live = event_synced; dead = [(event_synced, dpat)]}; reg_borrows = []; dtype = gtd.gdtype}}

  let recv_msg_data g (w : wire option) (msg : message_specifier) (msg_def : message_def) (current : event) =
    let event_received = event_create g (`Seq (current, `Recv msg)) in
    let stype = List.hd msg_def.sig_types in
    let e = delay_pat_globalise msg.endpoint stype.lifetime.e in
    {ld = {w; lt = {live = event_received; dead = [(event_received, e)]}; reg_borrows = []; dtype = stype.dtype}}

  let context_add (ctx : lowering_data context) (v : identifier) (d : lowering_data) : lowering_data context =
    Utils.StringMap.add v {binding_val = d; binding_used = false} ctx
  let context_empty = Utils.StringMap.empty
  let context_lookup (ctx : lowering_data context) (v : identifier) = Utils.StringMap.find_opt v ctx
  (* checks if lt lives at least as long as required *)

  let context_clear_used =
    Utils.StringMap.map (fun r -> {r with binding_used = false})

  module BuildContext = struct
    type t = build_context
    let create_empty g si lt_check_phase: t = {
      cg_lt_ctx = context_empty;
      current = event_create g (`Root None);
      shared_vars_info = si;
      lt_check_phase;
    }

    let clear_bindings (ctx : t) : t =
      {ctx with cg_lt_ctx = context_empty}
    let add_binding (ctx : t) (v : identifier) (d : lowering_data) : t =
      {ctx with cg_lt_ctx = context_add ctx.cg_lt_ctx v d}
    let wait g (ctx : t) (other : event) : t =
      {ctx with current = event_create g (`Later (ctx.current, other))}

    (* returns a pair of contexts for *)
    let branch_side g (ctx : t) (bi : branch_info) (sel : int) : branch_side_info * t  =
      let br_side_info = {branch_event = None; owner_branch = bi; branch_side_sel = sel} in
      let event_side_root = event_create g (`Root (Some (ctx.current, br_side_info))) in
      (br_side_info, {ctx with current = event_side_root; cg_lt_ctx = context_clear_used ctx.cg_lt_ctx})

    let branch g (ctx : t) (br_info : branch_info) : t =
      {ctx with current = event_create g (`Branch (ctx.current, br_info))}

    (* merge the used state in context *)
    let branch_merge ctx br_ctxs =
      let ctx1 = List.hd br_ctxs in
      let other_ctxs = List.tl br_ctxs in
      Utils.StringMap.iter (fun ident r ->
        if r.binding_used
            && (List.for_all
                (fun ctx2 -> (Utils.StringMap.find ident ctx2.cg_lt_ctx).binding_used)
               other_ctxs) then (
          (Utils.StringMap.find ident ctx.cg_lt_ctx).binding_used <- true
        )
      ) ctx1.cg_lt_ctx
  end


end