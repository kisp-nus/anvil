open Lang
open AstJsonSerializerHelpers

(*

TODO: This submodule is a temporary placeholder for the AST JSON seralizer that simultaneously performs: JSON serialization,
and certain specifics of the event-graph analysis and traversal logic needed to compute the abstract cycle delays and
value sustain lifetimes in concrete/symbolic terms. The logic is inferred from the actual compilation stages.

However, these concrete/symbolic terms should be best done during the actual graph analysis phase _during_ compilation,
rather than as a postprocessing step during JSON serialization. In other words, it should be rewritten in the future
to only contain the JSON serialization logic to prevent code/semantics drift, unnecessary behavior duplication,
and ease of reuse for other semantic analyses or improved error reporting that weren't possible in the existing compiler
as of writing.

The goal for the AST JSON serializer should be that it'll be a pure translation layer from the compiler's internal
representations of event data to the JSON AST, without any logic implemented out of it like here.

 *)

type cycle_time_sum_term =
  | CycleConstTime of int
  | CycleUnknownTime of string
  | CycleOrTime of string * cycle_time_sum list
  | CycleMaxTime of string * cycle_time_sum list

and cycle_time_sum = cycle_time_sum_term list

type event_expr_def =
  | EventExprOr of cycle_time_sum list
  | EventExprMax of cycle_time_sum list

type process_event_json_context = {
  symbolic_counters : (string, int) Hashtbl.t;
  or_symbols_by_key : (string, string) Hashtbl.t;
  max_symbols_by_key : (string, string) Hashtbl.t;
  event_exprs_by_sym : (string, event_expr_def) Hashtbl.t;
}

type event_json_context = {
  graph_by_tid : (int, EventGraph.event_graph list) Hashtbl.t;
  channel_classes : Lang.channel_class_def list;
  process_contexts : (Lang.identifier, process_event_json_context) Hashtbl.t;
}

let current_event_json_context : event_json_context option ref = ref None
let current_process_event_json_context : process_event_json_context option ref = ref None

let current_process_ctx () =
  match !current_process_event_json_context with
  | Some ctx -> ctx
  | None -> failwith "event JSON serialization used without process context"

(** Canonical ordering used for stable comparisons and symbol interning.

    The ordering is structural rather than semantic: terms that are only
    algebraically equivalent still compare differently unless another helper has
    normalized them first. Nested `Or` and `Max` branches are sorted recursively. *)
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
       | CycleOrTime (sym, or_terms) -> CycleOrTime (sym, List.map sort_cycle_time_sum or_terms)
       | CycleMaxTime (sym, max_terms) -> CycleMaxTime (sym, List.map sort_cycle_time_sum max_terms)
       | term -> term)

(** Allocate the next symbol for a given prefix (e.g., `or`, `max`, ...). *)
let fresh_symbolic_var (ctx : process_event_json_context) prefix =
  let n = Hashtbl.find_opt ctx.symbolic_counters prefix |> Option.value ~default:0 in
  Hashtbl.replace ctx.symbolic_counters prefix (n + 1);
  Printf.sprintf "%s%d" prefix n

(** Reuse an existing symbol for `key`, or allocate a new one.

    In other words, we look up `key` to check whether an existing symbol
    exists for it and use it; otherwise, create a new one.

    This is standard structural interning: symbol identity follows the canonical
    expression key rather than the traversal path that discovered the expression. *)
let intern_symbol (ctx : process_event_json_context) prefix key tbl =
  match Hashtbl.find_opt tbl key with
  | Some sym -> sym
  | None ->
      let sym = fresh_symbolic_var ctx prefix in
      Hashtbl.replace tbl key sym;
      sym

(** Add a constant delay into a flat additive sum.

    This helper only merges adjacent/additive constant information. It does not try
    to distribute through `CycleOrTime`; that behavior is intentionally disabled so
    `Or` simplification stays explicit and easier to reason about. It does collapse
    `CycleMaxTime` to its symbolic wrapper when adding around it, matching the
    module's current treatment of `Max` as an opaque additive term in this path. *)
let rec add_const_to_cycle_time_sum (n : int) (terms : cycle_time_sum) : cycle_time_sum =
  match terms with
  | CycleConstTime m :: rest -> add_const_to_cycle_time_sum (n + m) rest
  (* | CycleOrTime (or_sym, _) :: rest -> add_const_to_cycle_time_sum n (CycleUnknownTime or_sym :: rest) collapse terms *) (* TODO: disabled for investigation *)
  (* | CycleMaxTime (max_sym, _) :: rest -> add_const_to_cycle_time_sum n (CycleUnknownTime max_sym :: rest) collapse terms *)
  | _ -> if n = 0 then terms else CycleConstTime n :: terms

(** Add a symbolic delay into a flat additive sum.

    As with `add_const_to_cycle_time_sum`, this keeps `CycleOrTime` wrappers intact
    and only treats `CycleMaxTime` as collapsible to its symbolic wrapper in this
    low-level additive path. *)
let rec add_unknown_to_cycle_time_sum (sym : string) (terms : cycle_time_sum) : cycle_time_sum =
  match terms with
  | CycleConstTime n :: rest -> CycleConstTime n :: add_unknown_to_cycle_time_sum sym rest
  (* | CycleOrTime (or_sym, _) :: rest -> add_unknown_to_cycle_time_sum sym (CycleUnknownTime or_sym :: rest) collapse terms *) (* TODO: disabled for investigation *)
  (* | CycleMaxTime (max_sym, _) :: rest -> add_unknown_to_cycle_time_sum sym (CycleUnknownTime max_sym :: rest) collapse terms *)
  | _ -> CycleUnknownTime sym :: terms

(** Canonical normalization entry point for cycle-time sums.

    This keeps sums in the stable form used by the shared constructor pipelines and
    JSON output. It merges flat constants, sorts additive terms, and recursively
    normalizes nested `Or`/`Max` branches through the existing shared non-interning
    constructors. *)
let rec normalize_cycle_time_sum (terms : cycle_time_sum) : cycle_time_sum =
  let rec add_normalized_terms acc = function
    | [] -> acc
    | CycleConstTime n :: rest -> add_normalized_terms (add_const_to_cycle_time_sum n acc) rest
    | CycleUnknownTime sym :: rest -> add_normalized_terms (add_unknown_to_cycle_time_sum sym acc) rest
    | (CycleOrTime _ as term) :: rest -> add_normalized_terms (term :: acc) rest
    | (CycleMaxTime _ as term) :: rest -> add_normalized_terms (term :: acc) rest
  in
  let normalized_nested_terms =
    List.concat_map
      (function
        | CycleConstTime n -> if n = 0 then [] else [CycleConstTime n]
        | CycleUnknownTime sym -> [CycleUnknownTime sym]
        | CycleOrTime (sym, sums) -> [CycleOrTime (sym, List.map normalize_cycle_time_sum sums)]
        | CycleMaxTime (sym, sums) -> [CycleMaxTime (sym, List.map normalize_cycle_time_sum sums)])
      terms
  in
  add_normalized_terms [] normalized_nested_terms |> sort_cycle_time_sum

(** Structural serialization key for cycle-time sums.

    Equal keys mean the sums are identical after the module's current canonical
    normalization rules. This is what `interned_or_cycle_time_sum` and
    `interned_max_cycle_time_sum` use to reuse symbolic names. *)
let rec cycle_time_sum_key (terms : cycle_time_sum) : string =
  normalize_cycle_time_sum terms
  |> List.map cycle_time_sum_term_key
  |> String.concat ";"

and cycle_time_sum_term_key = function
  | CycleConstTime n -> Printf.sprintf "const:%d" n
  | CycleUnknownTime sym -> Printf.sprintf "sym:%s" sym
  | CycleOrTime (_, sums) ->
      let child_keys = List.map cycle_time_sum_key sums |> List.sort String.compare in
      Printf.sprintf "or:[%s]" (String.concat "," child_keys)
  | CycleMaxTime (_, sums) ->
      let child_keys = List.map cycle_time_sum_key sums |> List.sort String.compare in
      Printf.sprintf "max:[%s]" (String.concat "," child_keys)

(** Structural equality under the canonical key.

    This is stronger than raw syntactic equality because wrapper symbol names on
    nested `Or`/`Max` terms are ignored, but weaker than full algebraic equality:
    near-duplicate expressions only compare equal if they normalize to the same
    structure. *)
let equal_cycle_time_sums (terms1 : cycle_time_sum) (terms2 : cycle_time_sum) : bool =
  cycle_time_sum_key terms1 = cycle_time_sum_key terms2

(** Remove structurally duplicate branch sums while preserving first-seen order. *)
let dedup_cycle_time_sums (sums : cycle_time_sum list) : cycle_time_sum list =
  List.fold_left
    (fun acc d -> if List.exists (equal_cycle_time_sums d) acc then acc else d :: acc)
    [] sums
  |> List.rev

(** Extract the additive constant bucket from one branch. *)
let branch_const_part (terms : cycle_time_sum) : int =
  List.fold_left
    (fun acc term ->
      match term with
      | CycleConstTime n -> acc + n
      | _ -> acc)
    0 terms

(** Extract the non-constant additive terms from one branch.

    These are later counted as a multiset when searching for a common `Or` prefix. *)
let branch_nonconst_terms (terms : cycle_time_sum) : cycle_time_sum_term list =
  List.filter (function CycleConstTime _ -> false | _ -> true) terms

(** Factor the largest shared additive prefix out of a set of choice branches.

    The prefix consists of:
    - the minimum constant shared by every branch
    - the minimum multiplicity of each structurally identical non-constant term

    The result is `(shared_prefix, remainders)` such that each original branch is
    structurally equivalent to `shared_prefix + remainder_i`.

    This is valid for both `Or` and `Max`: if every branch contains the same
    additive prefix, that prefix can be pulled outside the wrapper. *)
let factor_common_choice_prefix (sums : cycle_time_sum list) : cycle_time_sum * cycle_time_sum list =
  match sums with
  | [] -> [], []
  | _ ->
      let min_const = List.fold_left (fun acc branch -> min acc (branch_const_part branch)) max_int sums in
      let term_counts_of_branch branch =
        let counts = Hashtbl.create 8 in
        List.iter
          (fun term ->
            let key = cycle_time_sum_term_key term in
            let count = Hashtbl.find_opt counts key |> Option.value ~default:0 in
            Hashtbl.replace counts key (count + 1))
          (branch_nonconst_terms branch);
        counts
      in
      let shared_counts =
        match sums with
        | first :: rest ->
            let shared = term_counts_of_branch first in
            List.iter
              (fun branch ->
                let branch_counts = term_counts_of_branch branch in
                let keys = Hashtbl.to_seq_keys shared |> List.of_seq in
                List.iter
                  (fun key ->
                    let shared_count = Hashtbl.find shared key in
                    let branch_count = Hashtbl.find_opt branch_counts key |> Option.value ~default:0 in
                    if branch_count = 0 then Hashtbl.remove shared key
                    else Hashtbl.replace shared key (min shared_count branch_count))
                  keys)
              rest;
            shared
        | [] -> Hashtbl.create 0
      in
      let representative_terms = Hashtbl.create 8 in
      List.iter
        (fun branch ->
          List.iter
            (fun term ->
              let key = cycle_time_sum_term_key term in
              if not (Hashtbl.mem representative_terms key) then Hashtbl.add representative_terms key term)
            (branch_nonconst_terms branch))
        sums;
      let shared_prefix_terms =
        let terms = ref [] in
        if min_const > 0 then terms := CycleConstTime min_const :: !terms;
        Hashtbl.iter
          (fun key count ->
            let term = Hashtbl.find representative_terms key in
            for _ = 1 to count do
              terms := term :: !terms
            done)
          shared_counts;
        normalize_cycle_time_sum !terms
      in
      let remainders =
        List.map
          (fun branch ->
            let remaining_const = branch_const_part branch - min_const in
            let remaining_counts = term_counts_of_branch branch in
            Hashtbl.iter
              (fun key count ->
                let current = Hashtbl.find_opt remaining_counts key |> Option.value ~default:0 in
                let updated = current - count in
                if updated <= 0 then Hashtbl.remove remaining_counts key
                else Hashtbl.replace remaining_counts key updated)
              shared_counts;
            let terms = ref [] in
            if remaining_const > 0 then terms := CycleConstTime remaining_const :: !terms;
            Hashtbl.iter
              (fun key count ->
                let term = Hashtbl.find representative_terms key in
                for _ = 1 to count do
                  terms := term :: !terms
                done)
              remaining_counts;
            normalize_cycle_time_sum !terms)
          sums
      in
      shared_prefix_terms, remainders

(** Fold branch sets that already contain all branches of a nested `Or` term.

    This targets patterns like `or(base_branches..., inner_or + extra)` where the
    outer branch set already contains every branch of `inner_or`. In that case the
    outer branch set can keep the interned `inner_or` wrapper and drop the duplicate
    explicit branches, which enables smaller factored forms downstream. *)
let fold_nested_or_branches (sums : cycle_time_sum list) : cycle_time_sum list =
  let remove_first_matching target branches =
    let rec go prefix = function
      | [] -> List.rev prefix
      | branch :: rest when equal_cycle_time_sums branch target -> List.rev_append prefix rest
      | branch :: rest -> go (branch :: prefix) rest
    in
    go [] branches
  in
  let rec step sums =
    let rec try_branches checked = function
      | [] -> List.rev checked
      | branch :: rest ->
          let rec try_terms prefix = function
            | [] -> try_branches (branch :: checked) rest
            | CycleOrTime (_, inner_sums) as or_term :: suffix ->
                let inner_sums = List.map normalize_cycle_time_sum inner_sums |> dedup_cycle_time_sums in
                if List.for_all (fun inner -> List.exists (equal_cycle_time_sums inner) sums) inner_sums then
                  let sums_without_inner = List.fold_left (fun acc inner -> remove_first_matching inner acc) sums inner_sums in
                  let replacement = normalize_cycle_time_sum [or_term] in
                  let updated = dedup_cycle_time_sums (replacement :: sums_without_inner) in
                  if List.length updated < List.length sums then step updated else try_terms (or_term :: prefix) (suffix)
                else try_terms (or_term :: prefix) suffix
            | term :: suffix -> try_terms (term :: prefix) suffix
          in
          try_terms [] branch
    in
    let updated = try_branches [] sums in
    if List.length updated = List.length sums && List.for_all2 equal_cycle_time_sums updated sums then sums else step updated
  in
  step sums

(** Shared internal `Or` constructor pipeline.

    All `Or` construction flows through here so the module applies one consistent
    normalization sequence:
    - canonicalize each branch
    - flatten nested one-term `Or`s
    - deduplicate equal branches
    - fold nested `Or` containment patterns
    - factor a shared additive prefix
    - collapse empty/singleton cases
    - resolve a symbol only for the remaining irreducible `Or` wrapper

    `resolve_sym` is the only policy hook: non-interned callers provide a fixed
    symbol, while interned callers derive/reuse one from the normalized branches. *)
let or_cycle_time_sum_with (resolve_sym : cycle_time_sum list -> string) (sums : cycle_time_sum list) : cycle_time_sum =
  let rec flatten_or terms =
    match terms with
    | [] -> []
    | [CycleOrTime (_, inner_sums)] :: rest -> flatten_or inner_sums @ flatten_or rest
    | term :: rest -> term :: flatten_or rest
  in
  let normalized_sums =
    sums
    |> List.map normalize_cycle_time_sum
    |> flatten_or
    |> dedup_cycle_time_sums
    |> fold_nested_or_branches
    |> dedup_cycle_time_sums
  in
  match normalized_sums with
  | [] -> []
  | [single] -> single
  | _ ->
      let shared_prefix, remainders = factor_common_choice_prefix normalized_sums in
      let remainders = remainders |> dedup_cycle_time_sums in
      let or_remainder =
        match remainders with
        | [] -> []
        | [single] -> single
        | merged_sums -> [CycleOrTime (resolve_sym merged_sums, merged_sums)]
      in
      normalize_cycle_time_sum (shared_prefix @ or_remainder)

(** Construct an `Or` wrapper without interning.
    (i.e., w/o reusing existing symbols/objects when possible). *)
let or_cycle_time_sum (sym : string) (sums : cycle_time_sum list) : cycle_time_sum =
  or_cycle_time_sum_with (fun _ -> sym) sums

(** Construct an `Or` wrapper and intern its symbol by structural key.
    This means if an existing or expression already exists, its symbol is reused. *)
let interned_or_cycle_time_sum (ctx : process_event_json_context) (sums : cycle_time_sum list) : cycle_time_sum =
  or_cycle_time_sum_with
    (fun merged_sums ->
      let key = Printf.sprintf "or:[%s]" (List.map cycle_time_sum_key merged_sums |> List.sort String.compare |> String.concat ",") in
      let sym = intern_symbol ctx "or" key ctx.or_symbols_by_key in
      if not (Hashtbl.mem ctx.event_exprs_by_sym sym) then Hashtbl.add ctx.event_exprs_by_sym sym (EventExprOr merged_sums);
      sym)
    sums

(** Conservative dominance merge for two `Max` branches.

    If one branch is known to be at least as large as the other under the module's
    current symbolic comparison, returns the dominating branch. Otherwise returns
    `None` and keeps both branches. *)
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

(** Flatten nested one-term `Max` wrappers into one branch list. *)
let rec flatten_max_branches sums =
  match sums with
  | [] -> []
  | [CycleMaxTime (_, inner_max_terms)] :: rest -> flatten_max_branches inner_max_terms @ flatten_max_branches rest
  | term :: rest -> term :: flatten_max_branches rest

(** Repeatedly apply conservative dominance merging within one `Max` branch list. *)
let rec merge_max_branches terms =
  match terms with
  | t1 :: t2 :: rest -> (
      match merge_max_of t1 t2 with
      | Some merged -> merge_max_branches (merged :: rest)
      | None -> t1 :: merge_max_branches (t2 :: rest))
  | terms -> terms

(** Shared internal `Max` constructor pipeline.

    This mirrors the `Or` constructor cleanup at a lower algebraic power:
    - canonicalize each branch
    - flatten nested one-term `Max`s
    - deduplicate equal branches
    - factor a shared additive prefix
    - apply conservative dominance merging
    - collapse empty/singleton cases
    - resolve a symbol only for the remaining irreducible `Max` wrapper *)
let max_cycle_time_sum_with (resolve_sym : cycle_time_sum list -> string) (max_terms : cycle_time_sum list) : cycle_time_sum =
  let normalized_terms =
    max_terms
    |> List.map normalize_cycle_time_sum
    |> flatten_max_branches
    |> dedup_cycle_time_sums
  in
  match normalized_terms with
  | [] -> []
  | [single] -> single
  | _ ->
      let shared_prefix, remainders = factor_common_choice_prefix normalized_terms in
      let merged_remainders = remainders |> dedup_cycle_time_sums |> merge_max_branches in
      let max_remainder =
        match merged_remainders with
        | [] -> []
        | [single] -> single
        | merged_terms -> [CycleMaxTime (resolve_sym merged_terms, merged_terms)]
      in
      normalize_cycle_time_sum (shared_prefix @ max_remainder)

(** Construct a `Max` wrapper without interning
    (i.e., w/o reusing existing symbols/objects when possible). *)
let max_cycle_time_sum (sym : string) (max_terms : cycle_time_sum list) : cycle_time_sum =
  max_cycle_time_sum_with (fun _ -> sym) max_terms

(** Construct a `Max` wrapper and intern its symbol by structural key.
    This means if an existing or expression already exists, its symbol is reused. *)
let interned_max_cycle_time_sum (ctx : process_event_json_context) (max_terms : cycle_time_sum list) : cycle_time_sum =
  max_cycle_time_sum_with
    (fun merged_terms ->
      let key = Printf.sprintf "max:[%s]" (List.map cycle_time_sum_key merged_terms |> List.sort String.compare |> String.concat ",") in
      let sym = intern_symbol ctx "max" key ctx.max_symbols_by_key in
      if not (Hashtbl.mem ctx.event_exprs_by_sym sym) then Hashtbl.add ctx.event_exprs_by_sym sym (EventExprMax merged_terms);
      sym)
    max_terms

(** Add one cycle-time sum on top of another.

    Flat constants and unknowns are merged directly. `CycleOrTime` and
    `CycleMaxTime` are reconstructed through their respective constructors so their
    wrapper structure remains explicit in the result. This helper is compositional,
    not fully normalizing: it preserves enough structure for later interning and
    JSON output rather than trying to perform aggressive algebraic simplification. *)
let rec extend_cycle_time_sums (terms1 : cycle_time_sum) (terms2 : cycle_time_sum) : cycle_time_sum =
  match terms1 with
  | [] -> terms2
  | CycleConstTime n :: rest -> extend_cycle_time_sums rest terms2 |> add_const_to_cycle_time_sum n
  | CycleUnknownTime sym :: rest -> extend_cycle_time_sums rest terms2 |> add_unknown_to_cycle_time_sum sym
  | CycleOrTime (sym, sums) :: rest -> or_cycle_time_sum sym sums @ extend_cycle_time_sums rest terms2
  | CycleMaxTime (sym, terms) :: rest -> max_cycle_time_sum sym terms @ extend_cycle_time_sums rest terms2

(** Translate a frontend `exec_delay` annotation into the internal sum form. *)
let cycle_time_sum_of_exec_delay (delay : Lang.exec_delay) : cycle_time_sum =
  List.fold_left
    (fun acc term ->
      match term with
      | DelayConst n -> add_const_to_cycle_time_sum n acc
      | DelaySym sym -> add_unknown_to_cycle_time_sum sym acc)
    [] delay

(** Convert a single event-graph edge delay into a cycle-time sum.

    Message/sync edges reuse the symbolic delay names attached earlier by
    `AstAnnotator`, so later structural interning can see consistent leaf symbols. *)
let atomic_delay_to_cycle_time_sum (tid : int) (from_ev : EventGraph.event) (to_ev : EventGraph.event)
    (ad : EventGraph.atomic_delay) : cycle_time_sum =
  match ad with
  | `Cycles c -> add_const_to_cycle_time_sum c []
  | `Send _ | `Recv _ | `Sync _ ->
      (match AstAnnotator.lookup_seq_delay_symbol tid from_ev.id to_ev.id with
      | Some sym -> [CycleUnknownTime sym]
      | None -> [])

(** Compute symbolic delays from `origin` to every reachable event in one thread.

    Branches are combined with interned `Or` terms and `Later` joins with interned
    `Max` terms so equivalent relative-delay expressions discovered from different
    origins/traversals can still share symbols inside one serialization context. *)
let rel_delays_from_event (_ctx : event_json_context) (graph : EventGraph.event_graph) (origin : EventGraph.event) : (int, cycle_time_sum) Hashtbl.t =
  let proc_ctx = current_process_ctx () in
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
              | Some d1, Some d2 -> Some (interned_max_cycle_time_sum proc_ctx [d1; d2])
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
                    Some (interned_or_cycle_time_sum proc_ctx unique_ds))
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

(** Reconstruct the sustain lifetime attached to a send/recv expression event.

    The serializer only stores the expression's starting `(tid, eid)` annotation, so
    this helper recomputes the lifetime from the event graph plus the message
    signature's declared lifetime.

    Resolution is intentionally tolerant of serializer-only context:
    - it tries every graph recorded for the thread id
    - it can resolve foreign endpoints by rebuilding a concrete message def from the
      endpoint's channel class
    - it probes endpoint aliases if the original endpoint name does not resolve in
      the candidate graph

    Supported lifetime forms:
    - `#n`: a constant `n`-cycle sustain lifetime
    - `eternal`: represented as the empty additive sum
    - `msg + off`: the delay from the effective origin event to the first reachable
      matching message-completion event, plus `off`

    For sends, the effective origin is the send-completion event rather than the
    expression's annotated start event. This matches the internal lifetime-checker
    model: the payload lifetime for `send ch.msg(...)` begins once the send has
    completed, so the send's own wait-to-complete must not be counted twice.

    For receives, the effective origin remains the expression's annotated event.
    `recv ch.msg` is annotated at the receive-completion event already, so dependent
    lifetimes are measured from the point where the value actually becomes available.

    For recursive threads, `msg + off` also has a loop fallback: if no reachable
    matching message is found before the recurse edge, we compose the delay from the
    base event to the recurse event with the delay from the root event to a matching
    message in the next iteration. *)
let sustain_lifetime_for_msg (ctx : event_json_context) (tid : int) (base_eid : int)
    ~(from_send_completion : bool) (msg_spec : message_specifier) : cycle_time_sum option =
  let proc_ctx = current_process_ctx () in
  let graphs = Hashtbl.find_opt ctx.graph_by_tid tid |> Option.value ~default:[] in
  let matching_send_completion_event (base_ev : EventGraph.event) (spec : message_specifier) =
    List.find_map
      (fun (sa_span : EventGraph.sustained_action Lang.ast_node) ->
        match sa_span.d.ty with
        | Send (msg_spec', _) when msg_spec' = spec -> Some sa_span.d.until
        | _ -> None)
      base_ev.sustained_actions
  in
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
            let origin_ev =
              if from_send_completion then
                Option.value ~default:base_ev (matching_send_completion_event base_ev msg_spec')
              else
                base_ev
            in
            let rel_delays = rel_delays_from_event ctx graph origin_ev in
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
                  let direct_sustain =
                    match sums with
                    | [] -> None
                    | [single] -> Some single
                    | _ -> Some (interned_or_cycle_time_sum proc_ctx sums)
                  in
                  (match direct_sustain with
                  | Some _ -> direct_sustain
                  | None ->
                      let recurse_event = List.find_opt (fun (e : EventGraph.event) -> e.is_recurse) graph.events in
                      let root_event = List.find_opt (fun (e : EventGraph.event) -> e.source = `Root None) graph.events in
                      (match recurse_event, root_event with
                      | Some recurse_ev, Some root_ev ->
                          let base_to_recurse = Hashtbl.find_opt rel_delays recurse_ev.id in
                          let root_rel_delays = rel_delays_from_event ctx graph root_ev in
                          let msg_events = GraphAnalysis.events_with_msg graph.events msg in
                          let loop_sums =
                            List.filter_map
                              (fun (ev : EventGraph.event) ->
                                match base_to_recurse, Hashtbl.find_opt root_rel_delays ev.id with
                                | Some d1, Some d2 -> Some (add_const_to_cycle_time_sum off (extend_cycle_time_sums d2 d1))
                                | _ -> None)
                              msg_events
                          in
                          (match loop_sums with
                          | [] -> None
                          | [single] -> Some single
                          | _ -> Some (interned_or_cycle_time_sum proc_ctx loop_sums))
                      | _ -> None))
              | `Message_with_offset (_msg, _off, false) -> None
            in
            (match computed with Some _ -> computed | None -> try_graphs rest)
        | _ -> try_graphs rest)
  in
  try_graphs graphs

(** Compute the optional `sustain_lifetime` JSON field for expression events.

    Only send/recv expressions can introduce sustained actions, so other expression
    forms deliberately omit the field. *)
let sustain_lifetime_of_expr_event (expr : expr) (tid : int) (eid : int) : cycle_time_sum option =
  match !current_event_json_context with
  | Some ctx -> (
      match expr with
      | Send sp -> sustain_lifetime_for_msg ctx tid eid ~from_send_completion:true sp.send_msg_spec
      | Recv rp -> sustain_lifetime_for_msg ctx tid eid ~from_send_completion:false rp.recv_msg_spec
      | _ -> None)
  | None -> None

let symbolic_sum_to_yojson (terms : cycle_time_sum) : Yojson.Safe.t =
  list_rev
    (function
      | CycleConstTime n -> assoc [("const", int n)]
      | CycleUnknownTime sym -> assoc [("sym", str sym)]
      | CycleOrTime (sym, _sums) -> assoc [("sym", str sym)]
      | CycleMaxTime (sym, _max_terms) -> assoc [("sym", str sym)])
    (normalize_cycle_time_sum terms)

let event_exprs_to_yojson (ctx : process_event_json_context) : Yojson.Safe.t =
  let event_expr_def_to_yojson = function
    | EventExprOr sums -> assoc [("type", str "or"); ("value", list symbolic_sum_to_yojson sums)]
    | EventExprMax sums -> assoc [("type", str "max"); ("value", list symbolic_sum_to_yojson sums)]
  in
  Hashtbl.to_seq_keys ctx.event_exprs_by_sym
  |> List.of_seq
  |> List.sort String.compare
  |> List.map (fun sym -> (sym, event_expr_def_to_yojson (Hashtbl.find ctx.event_exprs_by_sym sym)))
  |> assoc

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
  let global_ctx =
    match !current_event_json_context with
    | Some ctx -> ctx
    | None -> failwith "event_graph_collection_to_yojson called without event JSON context"
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
                  let ctx = current_process_ctx () in
                  let e1_delays = compute_delays e1 in
                  let e2_delays = compute_delays e2 in
                  interned_max_cycle_time_sum ctx [e1_delays; e2_delays]
              | `Branch (_, branch_info) ->
                  let ctx = current_process_ctx () in
                  let other_delays = List.map (fun (oe : EventGraph.event) -> compute_delays oe) branch_info.branches_val in
                  let unique_other_delays =
                    List.fold_left (fun acc d -> if List.exists (equal_cycle_time_sums d) acc then acc else d :: acc) [] other_delays
              in
              interned_or_cycle_time_sum ctx unique_other_delays
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
    let proc_ctx = Hashtbl.find global_ctx.process_contexts p.name in
    let prev_proc_ctx = !current_process_event_json_context in
    current_process_event_json_context := Some proc_ctx;
    Fun.protect
      (fun () ->
        let thread_orders = List.map (fun (e, _) -> event_graph_to_order e) p.threads in
        assoc [("proc_name", str p.name); ("event_exprs", event_exprs_to_yojson proc_ctx); ("threads", list (fun t -> t) thread_orders)])
      ~finally:(fun () -> current_process_event_json_context := prev_proc_ctx)
  in

  List.map proc_graph_to_order gc.event_graphs

let build_process_context () =
  {
    symbolic_counters = Hashtbl.create 4;
    or_symbols_by_key = Hashtbl.create 32;
    max_symbols_by_key = Hashtbl.create 32;
    event_exprs_by_sym = Hashtbl.create 32;
  }

let build_event_json_context channel_classes (gcl : EventGraph.event_graph_collection list) =
  let graph_by_tid = Hashtbl.create 16 in
  let process_contexts = Hashtbl.create 16 in
  List.iter
    (fun (gcol : EventGraph.event_graph_collection) ->
      List.iter
        (fun (pg : EventGraph.proc_graph) ->
          if not (Hashtbl.mem process_contexts pg.name) then Hashtbl.add process_contexts pg.name (build_process_context ());
          List.iter
            (fun ((g : EventGraph.event_graph), _rst) ->
              let current = Hashtbl.find_opt graph_by_tid g.thread_id |> Option.value ~default:[] in
              Hashtbl.replace graph_by_tid g.thread_id (g :: current))
            pg.threads)
        gcol.event_graphs)
    gcl;
  {
    graph_by_tid;
    channel_classes;
    process_contexts;
  }

let with_process_event_json_context proc_name f =
  match !current_event_json_context with
  | None -> f ()
  | Some ctx ->
      let proc_ctx =
        match Hashtbl.find_opt ctx.process_contexts proc_name with
        | Some proc_ctx -> proc_ctx
        | None ->
            let proc_ctx = build_process_context () in
            Hashtbl.add ctx.process_contexts proc_name proc_ctx;
            proc_ctx
      in
      let prev_proc_ctx = !current_process_event_json_context in
      current_process_event_json_context := Some proc_ctx;
      Fun.protect f ~finally:(fun () -> current_process_event_json_context := prev_proc_ctx)

let with_event_json_context channel_classes gcl f =
  let prev_ctx = !current_event_json_context in
  let prev_proc_ctx = !current_process_event_json_context in
  current_event_json_context := Some (build_event_json_context channel_classes gcl);
  current_process_event_json_context := None;
  Fun.protect f ~finally:(fun () ->
      current_process_event_json_context := prev_proc_ctx;
      current_event_json_context := prev_ctx)
