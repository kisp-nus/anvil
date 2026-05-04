open Lang
open AstJsonSerializerHelpers

type event_json_context = {
  graph_by_tid : (int, EventGraph.event_graph list) Hashtbl.t;
  channel_classes : Lang.channel_class_def list;
}

let current_event_json_context : event_json_context option ref = ref None

type cycle_time_sum_term =
  | CycleConstTime of int
  | CycleUnknownTime of string
  | CycleOrTime of cycle_time_sum list
  | CycleMaxTime of string * cycle_time_sum list

and cycle_time_sum = cycle_time_sum_term list

let rec simplify_cycle_time_sum (terms : cycle_time_sum) : cycle_time_sum =
  match terms with
  | [] -> []
  | CycleConstTime n :: rest when n = 0 -> rest
  | term :: rest -> term :: simplify_cycle_time_sum rest

let rec sort_cycle_time_sum (terms : cycle_time_sum) : cycle_time_sum =
  terms
  |> List.sort (fun a b ->
       match a, b with
       | CycleUnknownTime s1, CycleUnknownTime s2 -> String.compare s1 s2
       | CycleUnknownTime _, CycleConstTime _ -> -1
       | CycleConstTime _, CycleUnknownTime _ -> 1
       | CycleConstTime n1, CycleConstTime n2 -> compare n1 n2
       | CycleMaxTime _, _ -> 1
       | _, CycleMaxTime _ -> -1
       | CycleOrTime _, _ -> 1
       | _, CycleOrTime _ -> -1)
  |> List.map (function
       | CycleMaxTime (sym, max_terms) -> CycleMaxTime (sym, List.map sort_cycle_time_sum max_terms)
       | term -> term)

let equal_cycle_time_sums (terms1 : cycle_time_sum) (terms2 : cycle_time_sum) : bool =
  let symbol_count_tbl = Hashtbl.create ((List.length terms1 + List.length terms2) * 5) in

  let count_const terms =
    List.fold_left
      (fun acc term ->
        match term with
        | CycleConstTime n -> acc + n
        | _ -> acc)
      0 terms
  in

  let count_terms terms dir =
    List.iter
      (function
        | CycleUnknownTime sym
        | CycleMaxTime (sym, _) ->
            let count = Hashtbl.find_opt symbol_count_tbl sym |> Option.value ~default:0 in
            Hashtbl.replace symbol_count_tbl sym (count + dir)
        | _ -> ())
      terms
  in

  count_terms terms1 (+1);
  count_terms terms2 (-1);

  Hashtbl.fold (fun _ count acc -> count = 0 && acc) symbol_count_tbl true
  && count_const terms1 = count_const terms2

let rec add_const_to_cycle_time_sum (n : int) (terms : cycle_time_sum) : cycle_time_sum =
  match terms with
  | CycleConstTime m :: rest -> add_const_to_cycle_time_sum (n + m) rest
  | CycleMaxTime (max_sym, _) :: rest -> add_const_to_cycle_time_sum n (CycleUnknownTime max_sym :: rest)
  | _ -> if n = 0 then terms else CycleConstTime n :: terms

let rec add_unknown_to_cycle_time_sum (sym : string) (terms : cycle_time_sum) : cycle_time_sum =
  match terms with
  | CycleConstTime n :: rest -> CycleConstTime n :: add_unknown_to_cycle_time_sum sym rest
  | CycleMaxTime (max_sym, _) :: rest -> add_unknown_to_cycle_time_sum sym (CycleUnknownTime max_sym :: rest)
  | _ -> CycleUnknownTime sym :: terms

let or_cycle_time_sum (sums : cycle_time_sum list) : cycle_time_sum =
  let rec flatten_or terms =
    match terms with
    | [] -> []
    | [CycleOrTime inner_sums] :: rest -> flatten_or inner_sums @ flatten_or rest
    | term :: rest -> term :: flatten_or rest
  in
  match flatten_or sums with
  | [] -> []
  | [single] -> single
  | merged_sums -> [CycleOrTime merged_sums]

let max_cycle_time_sum (sym : string) (max_terms : cycle_time_sum list) : cycle_time_sum =
  let merge_max_of sum1 sum2 =
    let const1 =
      List.fold_left
        (fun acc term ->
          match term with
          | CycleConstTime n -> acc + n
          | _ -> acc)
        0 sum1
    in
    let symbolic1 =
      List.filter (function CycleConstTime _ -> false | _ -> true) sum1 |> sort_cycle_time_sum
    in
    let const2 =
      List.fold_left
        (fun acc term ->
          match term with
          | CycleConstTime n -> acc + n
          | _ -> acc)
        0 sum2
    in
    let symbolic2 =
      List.filter (function CycleConstTime _ -> false | _ -> true) sum2 |> sort_cycle_time_sum
    in
    let max_of_consts = max const1 const2 in

    if symbolic1 = symbolic2 then
      if max_of_consts = 0 then Some symbolic1 else Some (CycleConstTime max_of_consts :: symbolic1)
    else
      let sym1_counts = Hashtbl.create 10 in
      let sym2_counts = Hashtbl.create 10 in
      List.iter
        (function
          | CycleUnknownTime s ->
              let count = Hashtbl.find_opt sym1_counts s |> Option.value ~default:0 in
              Hashtbl.replace sym1_counts s (count + 1)
          | _ -> ())
        symbolic1;
      List.iter
        (function
          | CycleUnknownTime s ->
              let count = Hashtbl.find_opt sym2_counts s |> Option.value ~default:0 in
              Hashtbl.replace sym2_counts s (count + 1)
          | _ -> ())
        symbolic2;

      let sym1_greater =
        Hashtbl.fold
          (fun s count acc ->
            let sym2_count = Hashtbl.find_opt sym2_counts s |> Option.value ~default:0 in
            count > sym2_count || acc)
          sym1_counts false
      in
      let sym2_greater =
        Hashtbl.fold
          (fun s count acc ->
            let sym1_count = Hashtbl.find_opt sym1_counts s |> Option.value ~default:0 in
            count > sym1_count || acc)
          sym2_counts false
      in

      if sym1_greater && not sym2_greater then Some (CycleConstTime max_of_consts :: symbolic1)
      else if sym2_greater && not sym1_greater then Some (CycleConstTime max_of_consts :: symbolic2)
      else None
  in

  let rec flatten_max sums =
    match sums with
    | [] -> []
    | [CycleMaxTime (_, inner_max_terms)] :: rest -> flatten_max inner_max_terms @ flatten_max rest
    | term :: rest -> term :: flatten_max rest
  in

  let rec merge_maxes terms =
    match terms with
    | t1 :: t2 :: rest -> (
        match merge_max_of t1 t2 with
        | Some merged -> merge_maxes (merged :: rest)
        | None -> t1 :: merge_maxes (t2 :: rest))
    | terms -> terms
  in

  [CycleMaxTime (sym, max_terms |> flatten_max |> merge_maxes)]

let rec extend_cycle_time_sums (terms1 : cycle_time_sum) (terms2 : cycle_time_sum) : cycle_time_sum =
  match terms1 with
  | [] -> terms2
  | CycleConstTime n :: rest -> extend_cycle_time_sums rest terms2 |> add_const_to_cycle_time_sum n
  | CycleUnknownTime sym :: rest -> extend_cycle_time_sums rest terms2 |> add_unknown_to_cycle_time_sum sym
  | CycleOrTime sums :: rest -> or_cycle_time_sum sums @ extend_cycle_time_sums rest terms2
  | CycleMaxTime (sym, terms) :: rest -> max_cycle_time_sum sym terms @ extend_cycle_time_sums rest terms2

let cycle_time_sum_of_exec_delay (delay : Lang.exec_delay) : cycle_time_sum =
  List.fold_left
    (fun acc term ->
      match term with
      | DelayConst n -> add_const_to_cycle_time_sum n acc
      | DelaySym sym -> add_unknown_to_cycle_time_sum sym acc)
    [] delay

let atomic_delay_to_cycle_time_sum (tid : int) (from_ev : EventGraph.event) (to_ev : EventGraph.event)
    (ad : EventGraph.atomic_delay) : cycle_time_sum =
  match ad with
  | `Cycles c -> add_const_to_cycle_time_sum c []
  | `Send _ | `Recv _ | `Sync _ ->
      (match AstAnnotator.lookup_seq_delay_symbol tid from_ev.id to_ev.id with
      | Some sym -> [CycleUnknownTime sym]
      | None -> [])

let rel_delays_from_event (graph : EventGraph.event_graph) (origin : EventGraph.event) : (int, cycle_time_sum) Hashtbl.t =
  let max_eid = List.fold_left (fun n (e : EventGraph.event) -> max n e.id) 0 graph.events in
  let dist : cycle_time_sum option array = Array.make (max_eid + 1) None in
  let set_dist (ev : EventGraph.event) d = if ev.id >= 0 && ev.id <= max_eid then dist.(ev.id) <- Some d in
  let get_dist (ev : EventGraph.event) = if ev.id >= 0 && ev.id <= max_eid then dist.(ev.id) else None in

  set_dist origin [];
  List.iter (fun e -> set_dist e []) (GraphAnalysis.event_predecessors origin);

  let events_topo = List.rev graph.events in
  List.iter
    (fun (ev : EventGraph.event) ->
      if Option.is_none (get_dist ev) then
        let d =
          match ev.source with
          | `Root None -> None
          | `Root (Some (e0, _)) -> get_dist e0
          | `Seq (e0, ad) ->
              Option.map (fun d0 -> extend_cycle_time_sums (atomic_delay_to_cycle_time_sum graph.thread_id e0 ev ad) d0) (get_dist e0)
          | `Later (e1, e2) -> (
              match get_dist e1, get_dist e2 with
              | Some d1, Some d2 -> Some (max_cycle_time_sum (Printf.sprintf "max_rel_%d_%d" origin.id ev.id) [d1; d2])
              | _ -> None)
          | `Branch (_, { branches_val; _ }) ->
              let ds = List.filter_map get_dist branches_val in
              (match ds with
              | [] -> None
              | [d] -> Some d
              | _ ->
                  let unique_ds =
                    List.fold_left (fun acc d -> if List.exists (equal_cycle_time_sums d) acc then acc else d :: acc) [] ds
                  in
                  Some (or_cycle_time_sum unique_ds))
        in
        match d with Some d' -> set_dist ev d' | None -> ())
    events_topo;

  let memo = Hashtbl.create (2 * List.length graph.events + 1) in
  List.iter
    (fun (ev : EventGraph.event) ->
      match get_dist ev with
      | Some d -> Hashtbl.replace memo ev.id d
      | None -> ())
    graph.events;
  memo

let sustain_lifetime_for_msg (ctx : event_json_context) (tid : int) (base_eid : int) (msg_spec : message_specifier) : cycle_time_sum option =
  let graphs = Hashtbl.find_opt ctx.graph_by_tid tid |> Option.value ~default:[] in
  let lookup_message_allow_foreign (graph : EventGraph.event_graph) (spec : message_specifier) =
    let ( let* ) = Option.bind in
    let* endpoint = MessageCollection.lookup_endpoint graph.messages spec.endpoint in
    let* cc = MessageCollection.lookup_channel_class ctx.channel_classes endpoint.channel_class in
    let* msg = List.find_opt (fun (m : message_def) -> m.name = spec.msg) cc.messages in
    let* msg = Some { msg with dir = get_message_direction msg.dir endpoint.dir } in
    Some (ParamConcretise.concretise_message cc.params endpoint.channel_params msg)
  in
  let resolve_message (graph : EventGraph.event_graph) (spec : message_specifier) =
    match MessageCollection.lookup_message graph.messages spec ctx.channel_classes with
    | Some m -> Some m
    | None -> lookup_message_allow_foreign graph spec
  in
  let lookup_message_with_endpoint_aliases (graph : EventGraph.event_graph) (spec : message_specifier) =
    match resolve_message graph spec with
    | Some msg_def -> Some (spec, msg_def)
    | None ->
        let endpoint_names = List.map (fun (ep : endpoint_def) -> ep.name) (graph.messages.args @ graph.messages.endpoints) in
        let rec try_endpoints = function
          | [] -> None
          | ep_name :: rest ->
              let spec' = { spec with endpoint = ep_name } in
              (match resolve_message graph spec' with
              | Some msg_def -> Some (spec', msg_def)
              | None -> try_endpoints rest)
        in
        try_endpoints endpoint_names
  in
  let rec try_graphs = function
    | [] -> None
    | (graph : EventGraph.event_graph) :: rest ->
        let base_ev_opt = List.find_opt (fun (ev : EventGraph.event) -> ev.id = base_eid) graph.events in
        (match base_ev_opt, lookup_message_with_endpoint_aliases graph msg_spec with
        | Some base_ev, Some (msg_spec', msg_def) ->
            let stype = List.hd msg_def.sig_types in
            let dpat = delay_pat_globalise msg_spec'.endpoint stype.lifetime.e in
            let rel_delays = rel_delays_from_event graph base_ev in
            let computed =
              match dpat with
              | `Cycles n -> Some (add_const_to_cycle_time_sum n [])
              | `Eternal -> Some []
              | `Message_with_offset (msg, off, true) ->
                  let first_events = GraphAnalysis.events_first_msg graph.events base_ev msg in
                  let sums =
                    List.filter_map
                      (fun (ev : EventGraph.event) ->
                        Hashtbl.find_opt rel_delays ev.id |> Option.map (fun d -> add_const_to_cycle_time_sum off d))
                      first_events
                  in
                  let direct =
                    match sums with
                    | [] -> None
                    | [single] -> Some single
                    | _ -> Some (or_cycle_time_sum sums)
                  in
                  (match direct with
                  | Some _ -> direct
                  | None ->
                      let recurse_event = List.find_opt (fun (e : EventGraph.event) -> e.is_recurse) graph.events in
                      let root_event = List.find_opt (fun (e : EventGraph.event) -> e.source = `Root None) graph.events in
                      (match recurse_event, root_event with
                      | Some recurse_ev, Some root_ev ->
                          let base_to_recurse = Hashtbl.find_opt rel_delays recurse_ev.id in
                          let root_rel_delays = rel_delays_from_event graph root_ev in
                          let msg_events = GraphAnalysis.events_with_msg graph.events msg in
                          let sums_from_loop =
                            List.filter_map
                              (fun (ev : EventGraph.event) ->
                                match base_to_recurse, Hashtbl.find_opt root_rel_delays ev.id with
                                | Some d1, Some d2 -> Some (add_const_to_cycle_time_sum off (extend_cycle_time_sums d2 d1))
                                | _ -> None)
                              msg_events
                          in
                          (match sums_from_loop with
                          | [] -> None
                          | [single] -> Some single
                          | _ -> Some (or_cycle_time_sum sums_from_loop))
                      | _ -> None))
              | `Message_with_offset (_msg, _off, false) -> None
            in
            (match computed with Some _ -> computed | None -> try_graphs rest)
        | _ -> try_graphs rest)
  in
  try_graphs graphs

let sustain_lifetime_of_expr_event (expr : expr) (tid : int) (eid : int) : cycle_time_sum option =
  match !current_event_json_context with
  | Some ctx -> (
      match expr with
      | Send sp -> sustain_lifetime_for_msg ctx tid eid sp.send_msg_spec
      | Recv rp -> sustain_lifetime_for_msg ctx tid eid rp.recv_msg_spec
      | _ -> None)
  | None -> None

let rec symbolic_sum_to_yojson (terms : cycle_time_sum) : Yojson.Safe.t =
  list_rev
    (function
      | CycleConstTime n -> assoc [("const", int n)]
      | CycleUnknownTime sym -> assoc [("sym", str sym)]
      | CycleOrTime sums -> assoc [("or", list symbolic_sum_to_yojson sums)]
      | CycleMaxTime (sym, max_terms) -> assoc [("sym", str sym); ("max", list symbolic_sum_to_yojson max_terms)])
    (simplify_cycle_time_sum terms)

let ast_node_event_to_yojson = function
  | Some (tid, eid, delay_to_exec) ->
      assoc
        [
          ("tid", int tid);
          ("eid", int eid);
          ("delay_to_exec", symbolic_sum_to_yojson (cycle_time_sum_of_exec_delay delay_to_exec));
        ]
  | None -> `Null

let expr_node_event_to_yojson (expr : expr) = function
  | Some (tid, eid, delay_to_exec) ->
      let sustain_lifetime_field =
        match sustain_lifetime_of_expr_event expr tid eid with
        | Some lifetime -> [("sustain_lifetime", symbolic_sum_to_yojson lifetime)]
        | None -> []
      in
      assoc
        ([
           ("tid", int tid);
           ("eid", int eid);
         ]
        @ sustain_lifetime_field
        @ [("delay_to_exec", symbolic_sum_to_yojson (cycle_time_sum_of_exec_delay delay_to_exec))])
  | None -> `Null

let event_graph_collection_to_yojson (gc : EventGraph.event_graph_collection) : Yojson.Safe.t list =
  let symbolic_counter = ref 0 in
  let fresh_symbolic_var prefix =
    let n = !symbolic_counter in
    symbolic_counter := n + 1;
    Printf.sprintf "%s%d" prefix n
  in

  let event_graph_to_order (t : EventGraph.event_graph) =
    let events = t.events in
    let tid = t.thread_id in

    let delays_memo : (int, cycle_time_sum) Hashtbl.t = Hashtbl.create (5 * List.length events) in
    let rec compute_delays (ev : EventGraph.event) : cycle_time_sum =
      if Hashtbl.mem delays_memo ev.id then Hashtbl.find delays_memo ev.id
      else
        let delays =
          match ev.source with
          | `Root None -> []
          | `Root (Some (e0, _)) -> compute_delays e0
          | `Seq (e0, atomic_delay) ->
              let sum = compute_delays e0 in
              (match atomic_delay with
              | `Cycles c -> add_const_to_cycle_time_sum c sum
              | `Send _ | `Recv _ | `Sync _ -> (
                  match AstAnnotator.lookup_seq_delay_symbol tid e0.id ev.id with
                  | Some sym -> extend_cycle_time_sums [CycleUnknownTime sym] sum
                  | None -> sum))
          | `Later (e1, e2) ->
              let e1_delays = compute_delays e1 in
              let e2_delays = compute_delays e2 in
              max_cycle_time_sum (fresh_symbolic_var "max") [e1_delays; e2_delays]
          | `Branch (_, branch_info) ->
              let other_delays = List.map (fun (oe : EventGraph.event) -> compute_delays oe) branch_info.branches_val in
              let unique_other_delays =
                List.fold_left (fun acc d -> if List.exists (equal_cycle_time_sums d) acc then acc else d :: acc) [] other_delays
              in
              or_cycle_time_sum unique_other_delays
        in
        Hashtbl.add delays_memo ev.id delays;
        delays
    in

    let get_event_json (e : EventGraph.event) =
      let get_thread_event_id_pair (event : EventGraph.event) =
        assoc [("tid", int event.graph.thread_id); ("eid", int event.id)]
      in
      let delays_json = symbolic_sum_to_yojson (compute_delays e) in
      if List.is_empty e.outs then assoc [("eid", int e.id); ("delay", delays_json)]
      else assoc [("eid", int e.id); ("delay", delays_json); ("outs", list get_thread_event_id_pair e.outs)]
    in
    assoc
      [
        ("tid", int t.thread_id);
        ("events", list_rev get_event_json events);
        ("span", code_span_to_yojson t.thread_codespan);
      ]
  in

  let proc_graph_to_order (p : EventGraph.proc_graph) =
    let thread_orders = List.map (fun (e, _) -> event_graph_to_order e) p.threads in
    assoc [("proc_name", str p.name); ("threads", list (fun t -> t) thread_orders)]
  in

  List.map proc_graph_to_order gc.event_graphs

let build_event_json_context channel_classes (gcl : EventGraph.event_graph_collection list) =
  let graph_by_tid = Hashtbl.create 16 in
  List.iter
    (fun (gcol : EventGraph.event_graph_collection) ->
      List.iter
        (fun (pg : EventGraph.proc_graph) ->
          List.iter
            (fun ((g : EventGraph.event_graph), _rst) ->
              let current = Hashtbl.find_opt graph_by_tid g.thread_id |> Option.value ~default:[] in
              Hashtbl.replace graph_by_tid g.thread_id (g :: current))
            pg.threads)
        gcol.event_graphs)
    gcl;
  { graph_by_tid; channel_classes }

let with_event_json_context channel_classes gcl f =
  let prev_ctx = !current_event_json_context in
  current_event_json_context := Some (build_event_json_context channel_classes gcl);
  Fun.protect f ~finally:(fun () -> current_event_json_context := prev_ctx)
