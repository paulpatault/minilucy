open Typed_ast

let new_local =
  let cpt = ref 0 in fun () -> incr cpt;
    Ident.make ("aux'"^(string_of_int !cpt)) Ident.Stream

let new_patt ({texpr_type = ty; texpr_loc = loc; _} as e) =
  match ty with
  | [t] ->
    let x = new_local () in
    let decl = [x, t] in
    let patt = { tpatt_desc = [x]; tpatt_type = [t];tpatt_loc = loc; } in
    let expr = {e with texpr_desc = TE_ident x; } in
    decl, patt, expr
  | tl ->
    let xl = List.map (fun _ -> new_local ()) tl in
    let decl = List.combine xl tl in
    let patt = {tpatt_desc = xl; tpatt_type = tl; tpatt_loc = loc} in
    let le =
      List.map (fun (id,ty) ->
          {texpr_desc = TE_ident id; texpr_type = [ty]; texpr_loc = loc;})
        decl
    in
    decl, patt, {e with texpr_desc = TE_tuple le;}

let rec normalize ctx e =
  match e.texpr_desc with
  | TE_const _ | TE_ident _ ->
      ctx, e
  | TE_op (op, el) ->
      let ctx, el' =
        match op, el with
        | (Op_not | Op_sub | Op_sub_f), [e] ->
          let ctx, e' = normalize ctx e in
          ctx, [e']
        | (Op_and | Op_or | Op_impl
          | Op_add | Op_sub | Op_mul | Op_div | Op_mod
          | Op_div_f | Op_mul_f | Op_sub_f | Op_add_f
          | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge), [e1; e2] ->
          let ctx, e1' = normalize ctx e1 in
          let ctx, e2' = normalize ctx e2 in
          ctx, [e1'; e2']
        | Op_if, [e1; e2; e3] ->
          let ctx, e1' = normalize ctx e1 in
          let ctx, e2' = normalize ctx e2 in
          let ctx, e3' = normalize ctx e3 in
          ctx, [e1'; e2'; e3']
        | _ -> failwith "Unreachable"
      in
      ctx, {e with texpr_desc = TE_op (op, el');}

  | TE_print (s, el) ->
      let (new_equs, new_vars), e' = normalize_list ctx el in
      let x_decl, x_patt, x_expr = new_patt e in
      let x_eq = {teq_patt = x_patt;
                  teq_expr = {e with texpr_desc = TE_print (s, e')}}
      in
      (x_eq::new_equs, x_decl@new_vars), x_expr
  | TE_app (f, el) ->
      let (new_equs, new_vars), el' = normalize_list ctx el in
      let x_decl, x_patt, x_expr = new_patt e in
      let x_eq = {teq_patt = x_patt;
                  teq_expr = {e with texpr_desc = TE_app (f, el')}}
      in
      (x_eq::new_equs, x_decl@new_vars), x_expr
  | TE_prim (f, el) ->
      let (new_equs, new_vars), el' = normalize_list ctx el in
      let x_decl, x_patt, x_expr = new_patt e in
      let x_eq = {teq_patt = x_patt;
                  teq_expr = {e with texpr_desc = TE_prim (f, el')}}
      in
      (x_eq::new_equs, x_decl@new_vars), x_expr
  | TE_tuple l ->
      let ctx, l' = normalize_list ctx l in
      ctx, {e with texpr_desc = TE_tuple l'}
  | TE_fby (e1, e2) ->
      let ctx, e1' = normalize ctx e1 in
      let (new_eqs, new_vars), e2' = normalize ctx e2 in
      let y_decl, y_patt, y_expr = new_patt e2' in
      let y_eq = {teq_patt = y_patt; teq_expr = e2'} in
      let x_decl, x_patt, x_expr = new_patt e in
      let x_eq = {teq_patt = x_patt;
                  teq_expr = {e with texpr_desc = TE_fby (e1', y_expr)}}
      in
      (x_eq::y_eq::new_eqs, x_decl@y_decl@new_vars), x_expr
  | TE_merge (id, el) -> 
    let ctx, el' = List.fold_left_map (fun ctx (ctor, e) ->
        let ctx, e' = normalize ctx e in
        ctx, (ctor, e'))
        ctx
        el
    in
    ctx, {e with texpr_desc = TE_merge (id, el')}
  | TE_when (e1, b, e2) ->
      let ctx, e1' = normalize ctx e1 in
      let ctx, e2' = normalize ctx e2 in
      begin
        match e2'.texpr_desc with
        | TE_ident _ ->
          ctx, {e with texpr_desc = TE_when (e1', b, e2')}
        | _ ->
          let (new_eqs, new_vars) = ctx in
          let y_decl, y_patt, y_expr = new_patt e2' in
          let y_eq = {teq_patt = y_patt; teq_expr = e2'} in
          (y_eq::new_eqs, y_decl@new_vars), {e with texpr_desc = TE_when (e1', b, y_expr)}
      end
  | TE_pre _
  | TE_arrow _ -> failwith "Unreachable"


and normalize_list ctx el =
  let ctx, el =
    List.fold_left (fun (ctx, el) e ->
        let ctx, e' = normalize ctx e in
        ctx, e'::el)
      (ctx, [])
      el
  in
  ctx, List.rev el

let normalize_eq node eq =
  let (new_eqs, locals), e' = normalize ([], []) eq.teq_expr in
  let locals = List.map (fun e -> e, None) locals in
  {node with
   tn_local = locals@node.tn_local;
   tn_equs = {eq with teq_expr = e'}::(List.rev new_eqs) @ node.tn_equs}

let file f =
  { f with
      t_nodes = List.map (fun node ->
        let node = List.fold_left normalize_eq {node with tn_equs = []} node.tn_equs in
        {node with tn_equs = List.rev node.tn_equs}) f.t_nodes }
