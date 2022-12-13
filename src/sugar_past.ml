open Parse_ast
open Asttypes

type error =
  | ConstrInconnu of string

exception Error of location * error

let error loc e = raise (Error (loc, e))

let report fmt = function
  | ConstrInconnu id -> Format.fprintf fmt "undefined constructor %s in automaton" id

let gen = let r = ref 0 in fun s -> incr r; Printf.sprintf "%s__%d" s !r

let mk_merge var over t eqs loc =
  let merge = PE_merge (over, (List.map2 (fun tt eq -> tt, eq) t eqs)) in
  {pexpr_desc = merge; pexpr_loc = loc}

let mk_merge_eq var over {constr; name} eqs loc =
  let t = List.map (fun id ->
      { pexpr_desc = PE_const (Cadt (name, Some id));
        pexpr_loc = loc })
      constr
  in
  let peq_expr = mk_merge var over t eqs loc in
  PE_eq ({peq_patt = var; peq_expr})

let mk_fby_merge init var over {constr; name} eqs loc =
  let t = List.map (fun id ->
      { pexpr_desc = PE_const (Cadt (name, Some id));
        pexpr_loc = loc })
      constr
  in
  let expr_merge = mk_merge var over t eqs loc in
  let pre = {pexpr_desc = PE_pre expr_merge; pexpr_loc = loc} in
  let fby = PE_arrow (init, pre) in
  let fby = {pexpr_desc = fby; pexpr_loc = loc } in
  PE_eq ({peq_patt = var; peq_expr = fby})

let mk_constr_expr t name loc = { pexpr_desc = PE_const (Asttypes.Cadt (t, Some name)); pexpr_loc = loc }

let trad_autom {pautom_loc; pautom} =
  let constrs = List.map (function
    | {pn_weak=false;_} -> failwith {|automaton with "unless" clause (strong transitions) are not implemented yet|}
    | {pn_case={pn_constr;_};_} -> pn_constr
  ) pautom in

  let () = match List.find_opt (fun e -> not @@ List.mem e.pn_out constrs) pautom with
    | None -> ()
    | Some e -> error pautom_loc (ConstrInconnu e.pn_out) in


  let eqs = List.map (fun {pn_case={pn_equation;_}; _} -> pn_equation) pautom in
  let t = Asttypes.{ name = gen "typ" ; constr = constrs } in

  let (n, _) as state_init_local = gen "state", Asttypes.Tadt t.name in

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
      let c1 = mk_constr_expr t.name pn_out pautom_loc in
      let c2 = mk_constr_expr t.name pn_constr pautom_loc in
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

  let merge_var = mk_merge_eq var_var over t eqs_var pautom_loc in

  let init =
    { pexpr_desc = PE_const (Cadt (t.name, Some (List.hd constrs)));
      pexpr_loc = pautom_loc } in

  let merge_state = mk_fby_merge init var_state over t eqs_state pautom_loc in

  let set_conds = cond_locals in

  t, vars, state_init_local, set_conds @ [merge_state; merge_var]


let map_eq l =
  let acc, l =
    List.fold_left_map (fun ((locals_non_init, typ_acc) as acc) e -> match e with
    | PE_automaton a ->
        let t, v, state, a = trad_autom a in
        let v = List.map (fun e -> e, Asttypes.Tbool) v in
        (state :: v @ locals_non_init, t :: typ_acc), a
    | PE_print (s,e) ->
        let var = gen "print_var", Tint in
        let peq_patt =
          { ppatt_desc = PP_ident (fst var);
            ppatt_loc  = (List.hd e).pexpr_loc } in
        let peq_expr = {pexpr_desc = PE_print (s,e); pexpr_loc= (List.hd e).pexpr_loc} in
        let eq = PE_eq {peq_patt; peq_expr} in
        let acc = var :: locals_non_init, typ_acc in
        acc, [eq]
    | _ -> acc, [e]) ([], []) l
  in
  acc, List.flatten l

let apply node =
  let (locals, types), pn_equs = map_eq node.pn_equs in
  types, { node with pn_equs ; pn_local = locals @ node.pn_local}

let unsugar { p_nodes; p_types } =
  let acc, nodes = List.fold_left_map (fun acc e -> let types, trad = apply e in types @ acc, trad) [] p_nodes in
  { p_nodes = nodes; p_types = p_types @ acc }
