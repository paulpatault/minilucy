open Clocked_ast

exception Causality


module S = Set.Make(Ident)
module Graph = Set.Make(
  struct
    type t = Ident.t * S.t * c_equation
    let compare (x1,s1,_) (x2,s2,_) =
      let c = Ident.compare x1 x2 in
      if c<>0 then c else S.compare s1 s2
  end)


(** [add_vars_of_patt s patt] ajoute à l'ensemble [s] les variables
    introduites par le motif [patt]. *)
let add_vars_of_patt s {cpatt_desc=p;} =
  List.fold_left (fun s x -> S.add x s) s p


(** [add_vars_of_exp s exp] ajoute à l'ensemble [s] les variables
    dont l'expression [exp] dépend instantanément. *)
let rec add_vars_of_exp s {cexpr_desc=e} =
  match e with
  | CE_const _ -> s
  | CE_ident x -> S.add x s
  | CE_arrow (e1, e2) -> add_vars_of_exp (add_vars_of_exp s e1) e2
  | CE_pre e -> s
  | CE_op (_, el) -> List.fold_left (fun s e -> add_vars_of_exp s e) s el
  | CE_app (_,l) -> List.fold_left add_vars_of_exp s l
  | CE_prim (_,l) -> List.fold_left add_vars_of_exp s l
  | CE_tuple l -> List.fold_left add_vars_of_exp s l
  | CE_merge (cid, le) ->
      List.fold_left
        (fun acc (_, e) -> add_vars_of_exp acc e)
        (add_vars_of_exp s cid)
        le
  | CE_fby _ -> s
  | CE_when (e, _, _) -> add_vars_of_exp s e


let schedule_equs inputs equs =
  (* Construction du graphe de dépendance entre les variables. *)
  let equs = List.filter_map (function e -> Some e) equs in
  let g =
    List.fold_left
      (fun g eq ->
        let vp = add_vars_of_patt S.empty eq.ceq_patt in
        let ve = add_vars_of_exp S.empty eq.ceq_expr in
        S.fold (fun x g -> Graph.add (x, ve, eq) g) vp g)
      Graph.empty equs
  in
  (* Suppression des dépendances aux entrées. *)
  let g =
    let s_inputs =
      List.fold_left (fun acc (x, _, _) -> S.add x acc) S.empty inputs
    in
    Graph.fold
      (fun (y,s,e) g -> Graph.add (y,S.diff s s_inputs,e) g)
      g
      Graph.empty
  in
  (* Tri topologique des equations *)
  let rec exists_loop topo g =
    if Graph.is_empty g then List.rev topo
    else
      let g1 , g2 = Graph.partition (fun (_,s,_) -> S.is_empty s) g in
      if Graph.is_empty g1 then raise Causality;
      let sv =
        Graph.fold (fun (x,_,_) s -> S.add x s) g1 S.empty
      in
      let g =
        Graph.fold
          (fun (y,s,e) g -> Graph.add (y,S.diff s sv,e) g)
          g2 Graph.empty
      in
      let topo =
        Graph.fold
          (fun (_,_,e) l -> if List.mem e l then l else e::l)
          g1 topo
      in
      exists_loop topo g
  in
  exists_loop [] g

let schedule_node n =
  let equs = schedule_equs n.cn_input n.cn_equs in
  { n with cn_equs = equs; }

let schedule f =
  { f with c_nodes = List.map schedule_node f.c_nodes }

(* let rec left_expr s = function
  | CE_const _ -> s
  | CE_ident id -> S.add id s
  | CE_op (_, el) | CE_app (_, el) | CE_prim (_, el) 
  | CE_tuple el ->
    List.fold_left (fun s e -> left_expr s e) s
  | CE_app (_, el) ->
    List.fold_left (fun s e -> left_expr s e) s
  | CE_merge (id, )
  | CE_pre _
  | CE_arrow _ -> failwith "Unreachable" *)
