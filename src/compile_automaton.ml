open Parse_ast

let gen = let r = ref 0 in fun s -> incr r; Printf.sprintf "%s__%d" s !r

let mk_merge var over t eqs loc =
  let merge = PE_merge_adt (over, (List.map2 (fun tt eq -> tt, eq) t eqs)) in
  let peq_expr = {pexpr_desc = merge; pexpr_loc = loc} in
  PE_eq ({peq_patt = var; peq_expr})

let mk_constr_expr name loc = { pexpr_desc = PE_constr name; pexpr_loc = loc }

let trad {pautom_loc; pautom} =
  let constrs = List.map (function
    | {pn_weak=false;_} -> failwith {|automaton with "unless" clause (strong transitions) are not implemented yet|}
    | {pn_case={pn_constr;_};_} -> pn_constr
  ) pautom in

  let eqs = List.map (fun {pn_case={pn_equation;_}; _} -> pn_equation) pautom in
  let t = Asttypes.{ name = gen "typ" ; constr = constrs } in

  let (n, _, _) as state_init_local = gen "state", Asttypes.Tadt t.name, List.hd constrs in

  let vv = ref None in
  let eqs_var =
    List.map (function
      | PE_eq eq ->
          vv := Some eq.peq_patt; (* on récupère au passage la variable *)
          eq.peq_expr
      | _ -> assert false)
    eqs in

  let var_var = Option.get !vv in

  let var_state =
    { ppatt_desc = PP_ident n;
      ppatt_loc = pautom_loc } in

  let (cond_locals, vars), eqs_state =
    List.fold_left_map
    (fun (cond_loc_acc, vars_acc) {pn_cond; pn_out; pn_case={pn_constr; pn_loc; _}} ->
      let c1 = mk_constr_expr pn_out pautom_loc in
      let c2 = mk_constr_expr pn_constr pautom_loc in
      let name = gen "cond" in
      let var_cond = { pexpr_desc = PE_ident name; pexpr_loc = pn_loc} in
      let peq_patt =
        { ppatt_desc = PP_ident name;
          ppatt_loc = pn_loc } in
      let acc = (PE_eq { peq_patt; peq_expr = pn_cond} ) :: cond_loc_acc, name::vars_acc in
      acc, { pexpr_desc = PE_op (Asttypes.Op_if, [var_cond; c1; c2]); pexpr_loc = pautom_loc}
    ) ([], []) pautom in

  let over =
    { pexpr_desc = PE_ident n;
      pexpr_loc = pautom_loc } in

  let merge_var   = mk_merge var_var   over t.constr eqs_var   pautom_loc in
  let merge_state = mk_merge var_state over t.constr eqs_state pautom_loc in

  let set_conds = cond_locals in

  t, vars, state_init_local, set_conds @ [merge_state; merge_var]

let map_eq l =
  let acc, l =
    List.fold_left_map (fun ((locals_non_init, loc_acc, typ_acc) as acc) e -> match e with
    | PE_automaton a ->
        let t, v, state, a = trad a in
        (v @ locals_non_init, state :: loc_acc, t :: typ_acc), a
    | _ -> acc, [e]) ([], [], []) l
  in
  acc, List.flatten l

let apply node =
  let (locals, locals_init, types), pn_equs = map_eq node.pn_equs in
  let locals = List.map (fun e -> e, Asttypes.Tbool) locals in
  types, { node with pn_init_local = locals_init ; pn_equs ; pn_local = locals @ node.pn_local}

let compile { p_nodes; p_types } =
  let acc, nodes = List.fold_left_map (fun acc e -> let types, trad = apply e in types @ acc, trad) [] p_nodes in
  { p_nodes = nodes; p_types = p_types @ acc }
