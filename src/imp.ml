open Imp_ast
open Clocked_ast
open Clocks

let rec untulify el =
  List.fold_right
    (fun e acc ->
      match e.cexpr_desc with
      | CE_tuple el -> (untulify el @ acc)
      | _ -> e :: acc)
    el []

let empty_mem = { fby_mem = []; node_mem = [] }

let empty_init = { fby_init = []; node_init = [] }

let gen_next_id =
  let open Ident in
  let cpt = ref 0 in
  fun n -> incr cpt;
    Ident.make (n.name^"_next"^(string_of_int !cpt)) Ident.Stream

let gen_mem_id =
  let open Ident in
  let cpt = ref 0 in
  fun n -> incr cpt;
    Ident.make (n.name^"_next"^(string_of_int !cpt)) Ident.Stream

let rec list_of_ct = function
  | Ck ck -> [ck]
  | Cprod ((Ck ct)::cl) ->
    ct::list_of_ct (Cprod cl)
  | _ -> []

let compile_patt {cpatt_desc = d ; cpatt_type = t; cpatt_clock = c; _} =
  let tv = try List.combine d t with Invalid_argument _ -> assert false in
  let cl = list_of_ct c in
  List.map2 (fun (id, ty) c -> id, ty, c) tv cl


let rec compile_base_expr e =
  let desc =
    match e.cexpr_desc with
    | CE_const c -> IE_const c
    | CE_ident x -> IE_ident x
    | CE_op (op, el) ->
        IE_op(op, List.map compile_base_expr el)
    | CE_tuple el -> IE_tuple (List.map compile_base_expr el)
    | CE_fby _ -> assert false (* impossible car en forme normale *)
    | CE_app _ -> assert false (* impossible car en forme normale *)
    | CE_pre _ | CE_arrow _ -> assert false
    | CE_prim(f, el) ->
        IE_prim(f, List.map compile_base_expr el)
    | CE_when (e, _, _) ->
        let e' = compile_base_expr e in
        e'.iexpr_desc
    | CE_merge (id, ["True", e_t; "False", e_f]) ->
        let ide = compile_base_expr id in
        let e_t' = compile_base_expr e_t in
        let e_f' = compile_base_expr e_f in
        let case_t = {iexpr_desc = IE_const (Cbool true);
                      iexpr_type = [Tbool];}
        in
        let case_f = {iexpr_desc = IE_const (Cbool false);
                      iexpr_type = [Tbool];}
        in
        IE_case (ide, [case_t, e_t'; case_f, e_f'])
    | CE_merge (id, l) ->
        let ty = id.cexpr_type in
        let ide = compile_base_expr id in
        let l = List.map (fun (name, e) ->
          let lty = match ty with [Tadt s] -> s | _ -> assert false in
          let case = { iexpr_desc = IE_const (Cadt (lty, Some name)); iexpr_type = ty } in
          case, compile_base_expr e
          ) l in
        IE_case (ide, l)
  in
  { iexpr_desc = desc; iexpr_type = e.cexpr_type; }

let compile_atom a =
  match a.cexpr_desc with
  | CE_const c -> Const c
  | CE_ident x -> Ident x
  | _ -> assert false

let compile_atoms a =
  match a.cexpr_desc with
  | CE_tuple el -> List.map compile_atom el
  | CE_const _ | CE_ident _ -> [compile_atom a]
  | _ -> assert false

let rec ck_of_ct = function
  | Ck ck -> [ck]
  | Cprod ((Ck ck)::cl) ->
    ck::(ck_of_ct (Cprod cl))
  | Cprod [] -> []
  | _ -> assert false

let compile_equation
    {ceq_patt = p; ceq_expr = e}
    ((mem_acc: Imp_ast.mem),
     (init_acc: Imp_ast.init),
     (compute_acc: Imp_ast.i_equation list),
     (update_acc: (Ident.t * Imp_ast.atom) list)) =
  let cvars = compile_patt p in
  match e.cexpr_desc with
  | CE_fby(e1,e2) ->
    let id, ty, ck =
      match p.cpatt_desc, p.cpatt_type, p.cpatt_clock with
      | [id], [ty], Ck ck -> id, ty, ck
      | _ -> assert false
    in
      let next_id = gen_next_id id in
      let cnext_id = (next_id, ty, ck)  in
      let fby_init = (next_id, compile_atom e1) in
      let compute =
        let ce = { iexpr_desc = IE_mem next_id; iexpr_type = [ty]; } in
        { ieq_patt = cvars ; ieq_expr = ce }
      in
      let update = (next_id, compile_atom e2) in
      { mem_acc with fby_mem = cnext_id::mem_acc.fby_mem } ,
      { init_acc with fby_init = fby_init::init_acc.fby_init } ,
      compute::compute_acc , update::update_acc

  | CE_app(n,el) ->
      let mem_id = gen_mem_id n in
      let node_mem = [mem_id, n] in
      let step_in = (untulify el) in
      let node_init = [mem_id, n] in
      let compute =
        let expr =
          { iexpr_desc = IE_app (n, mem_id, List.map compile_base_expr step_in);
            iexpr_type =
              List.map
                (function
                  | { cexpr_type = [t] ; _} -> t
                  | _ -> assert false)
                step_in; }
        in
        { ieq_patt = cvars ; ieq_expr = expr }
      in
      { mem_acc with node_mem = node_mem@mem_acc.node_mem } ,
      { init_acc with node_init = node_init@init_acc.node_init } ,
      compute::compute_acc , update_acc

  | _ ->
      let eq = {ieq_patt = cvars; ieq_expr = compile_base_expr e} in
      mem_acc, init_acc, eq::compute_acc, update_acc


let compile_equations l =
  List.fold_right compile_equation l (empty_mem,empty_init,[],[])

let compile_node n =
  let input_step = n.cn_input in
  let output_step = n.cn_output in
  let (mem , init , compute , update) = compile_equations n.cn_equs in
  { in_name = n.cn_name;
    in_input_step = input_step;
    in_output_step = output_step;
    in_local = n.cn_local;
    in_mem = mem;
    in_init = init;
    in_compute = compute;
    in_update = update }

let compile f = { i_nodes = List.map compile_node f.c_nodes; i_types = f.c_types }

let gen_node_id =
  let cpt = ref 0 in
  fun s -> incr cpt ;
    Ident.(make (s.name^"'_"^(string_of_int !cpt)) Node)

let rename_expr env e =
  match e.iexpr_desc with
  | IE_app(f,mem,args) ->
      { e with iexpr_desc = IE_app(List.assoc f env, mem, args) }
  | _ -> e

let rename_equation env eq =
  { eq with ieq_expr = rename_expr env eq.ieq_expr; }

let rename_node env n =
  let id = gen_node_id n.in_name in
  let mem =
    { n.in_mem with
      node_mem =
        List.map (fun (x,t) -> (x, List.assoc t env)) n.in_mem.node_mem; }
  in
  let init =
    { n.in_init with
      node_init =
        List.map
          (fun (x,f) -> (x, List.assoc f env))
          n.in_init.node_init; }
  in
  let compute =
    List.map (rename_equation env) n.in_compute
  in
  ((n.in_name, id)::env),
  { n with in_name = id;
           in_mem = mem;
           in_init = init;
           in_compute = compute; }

let rename_nodes f main =
  let env , f' =
    List.fold_left
      (fun (env,f) n -> let env', n' = rename_node env n in (env', n'::f))
      ([],[]) f
  in
  main := (try List.assoc !main env with Not_found -> Ident.{name = ""; id = -1; kind = Node});
  List.rev f'
