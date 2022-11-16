open Parse_ast

let gen = let r = ref 0 in fun s -> incr r; Printf.sprintf "%s__%d" s !r

let mk_merge var over t eqs loc =
  let merge = PE_merge_adt (over, (List.map2 (fun tt eq -> tt, eq) t eqs)) in
  let peq_expr = {pexpr_desc = merge; pexpr_loc = loc} in
  PE_eq ({peq_patt = var; peq_expr})

let trad {pautom_loc; pautom} =
  let constrs = List.map (fun {pn_case={pn_constr;_}; _} -> pn_constr) pautom in
  let eqs = List.map (fun {pn_case={pn_equation;_}; _} -> pn_equation) pautom in
  let t = {pt_name = gen "typ" ; pt_constr = constrs} in

  let (n, _, _) as state_init_local = gen "state", t.pt_name, List.hd constrs in

  let var_state =
    { ppatt_desc = PP_ident n;
      ppatt_loc = pautom_loc } in

  let over =
    { pexpr_desc = PE_ident n;
      pexpr_loc = pautom_loc } in

  let eqs = List.map (function PE_eq eq -> eq.peq_expr | _ -> assert false) eqs in
  let merge_state = mk_merge var_state over t.pt_constr eqs pautom_loc in

  let var = assert false in
  let over = assert false in
  let eqs = assert false in
  let merge_var   = mk_merge var over t.pt_constr eqs pautom_loc in

  t, [merge_state; merge_var]

let map l =
  let acc, l =
    List.fold_left_map (fun ((loc_acc, typ_acc) as acc) e -> match e with
    | PE_automaton a ->
        let t, a = trad a in
        (loc_acc, t :: typ_acc), a
    | _ -> acc, [e]) ([], []) l
  in
  acc, List.flatten l

let apply node =
  let (locals, types), pn_equs = map node.pn_equs in
  types, { node with pn_init_local = locals ; pn_equs }

let compile { p_nodes; p_types } =
  let acc, nodes = List.fold_left_map (fun acc e -> let types, trad = apply e in types @ acc, trad) [] p_nodes in
  { p_nodes = nodes; p_types = p_types @ acc }
