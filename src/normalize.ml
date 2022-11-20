open Clocks
open Clocked_ast

let new_local =
  let cpt = ref 0 in fun () -> incr cpt;
    Ident.make ("aux'"^(string_of_int !cpt)) Ident.Stream

let new_patt ({cexpr_type = ty; cexpr_loc = loc; cexpr_clock = ct; _} as e) =
  match ty, ct with
  | [t], Ck ck ->
    let x = new_local () in
    let decl = [x, t, ck] in
    let patt = { cpatt_desc = [x]; cpatt_type = [t]; cpatt_clock = Ck ck; cpatt_loc = loc; } in
    let expr = {e with cexpr_desc = CE_ident x; } in
    decl, patt, expr
  | tl, Cprod cl ->
    let xl = List.map (fun _ -> new_local ()) tl in
    let ckl = List.map (function Ck ck -> ck | _ -> failwith "Not good clock") cl in
    let decl = Utils.combine3 xl tl ckl in
    let patt = {cpatt_desc = xl; cpatt_type = tl; cpatt_clock = Cprod cl; cpatt_loc = loc} in
    let le =
      List.map (fun (id,ty,cl) ->
          {cexpr_desc = CE_ident id; cexpr_type = [ty]; cexpr_clock = Ck cl; cexpr_loc = loc;})
        decl
    in
    decl, patt, {e with cexpr_desc = CE_tuple le;}
  | _ -> failwith "Check later"

let rec normalize ctx e =
  match e.cexpr_desc with
  | CE_const _ | CE_ident _ ->
      ctx, e
  | CE_op (op, el) ->
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
      ctx, {e with cexpr_desc = CE_op (op, el');}
  | CE_app (f, el) ->
      let (new_equs, new_vars), el' = normalize_list ctx el in
      let x_decl, x_patt, x_expr = new_patt e in
      let x_eq = {ceq_patt = x_patt;
                  ceq_expr = {e with cexpr_desc = CE_app (f, el')}}
      in
      (x_eq::new_equs, x_decl@new_vars), x_expr
  | CE_prim (f, el) ->
      let (new_equs, new_vars), el' = normalize_list ctx el in
      let x_decl, x_patt, x_expr = new_patt e in
      let x_eq = {ceq_patt = x_patt;
                  ceq_expr = {e with cexpr_desc = CE_prim (f, el')}}
      in
      (x_eq::new_equs, x_decl@new_vars), x_expr
  | CE_tuple l ->
      let ctx, l' = normalize_list ctx l in
      ctx, {e with cexpr_desc = CE_tuple l'}
  | CE_fby (e1, e2) ->
      let ctx, e1' = normalize ctx e1 in
      let (new_eqs, new_vars), e2' = normalize ctx e2 in
      let y_decl, y_patt, y_expr = new_patt e2' in
      let y_eq = {ceq_patt = y_patt; ceq_expr = e2'} in
      let x_decl, x_patt, x_expr = new_patt e in
      let x_eq = {ceq_patt = x_patt;
                  ceq_expr = {e with cexpr_desc = CE_fby (e1', e2')}}
      in
      (x_eq::y_eq::new_eqs, x_decl@y_decl@new_vars), x_expr
  | CE_merge (id, ["True", e_true; "False", e_false]) ->
      let ctx, e_true' = normalize ctx e_true in
      let ctx, e_false' = normalize ctx e_false in
      ctx, {e with cexpr_desc = CE_merge (id, ["True", e_true'; "False", e_false'])}
  | CE_merge (id, el) -> 
    let ctx, el' = List.fold_left_map (fun ctx (ctor, e) ->
        let ctx, e' = normalize ctx e in
        ctx, (ctor, e'))
        ctx
        el
    in
    ctx, {e with cexpr_desc = CE_merge (id, el')}
  | CE_when (e1, b, id) ->
      let ctx, e1' = normalize ctx e1 in
      ctx, {e with cexpr_desc = CE_when (e1', b, id)}
  | CE_pre _
  | CE_arrow _ -> failwith "Unreachable"


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
  let (new_eqs, locals), e' = normalize ([], []) eq.ceq_expr in
  {node with
   cn_local = locals@node.cn_local;
   cn_equs = {eq with ceq_expr = e'}::(List.rev new_eqs) @ node.cn_equs}

let file =
  List.map (fun node ->
      let node = List.fold_left normalize_eq {node with cn_equs = []} node.cn_equs in
      {node with cn_equs = List.rev node.cn_equs})
