open Asttypes
open Parse_ast
open Typed_ast
open Format
open Print_base

module S = Set.Make(Ident)
module M = Map.Make(String)

type error =
  | ExpectedType of ty * ty
  | ExpectedPattern of ty
  | ExpectedBase of ty
  | ExpectedNum of ty
  | UnboundVar of string
  | UnboundNode of string
  | TooFewArguments
  | TooManyArguments
  | Clash of string
  | ConstantExpected
  | Other of string
  | FlatTuple
  | UndefinedOutputs of string list
  | InputVar of string
  | Causality
  | BadMain of ty * ty
  | BadMerge
  | UnknownConstructor of string
  | UnknownAdtType of string
  | Unreachable of string
  | NotExhaustiveMerge of string

exception Error of location * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let nil = function
  | Tbool -> TE_const (Cbool false)
  | Tint -> TE_const (Cint 0)
  | Treal -> TE_const (Creal 0.0)
  | Tadt s -> TE_const (Cadt (s, None))

let error loc e = raise (Error (loc, e))
let errors loc s = error loc (Other s)

let report fmt = function
  | UnboundVar id -> fprintf fmt "unbound variable %s" id
  | UnboundNode id -> fprintf fmt "unbound node %s" id
  | ExpectedType (t1,t2) ->
    fprintf fmt
      "this expression has type %a but is expected to have type %a"
      print_type t1 print_type t2
  | ExpectedPattern ty ->
    fprintf fmt "this pattern is expected to have type %a"
      print_type ty
  | ExpectedBase ty ->
    fprintf fmt
      "this expression has type %a but is expected to have a type simple type"
      print_type ty
  | ExpectedNum ty ->
    fprintf fmt
      "this expression has type %a but is expected to have type int or real"
      print_type ty
  | Clash id -> fprintf fmt "The variable %s is defined several times" id
  | TooFewArguments -> fprintf fmt "too few arguments"
  | TooManyArguments -> fprintf fmt "too many arguments"
  | ConstantExpected -> fprintf fmt "this expression sould be a constant"
  | Other s -> fprintf fmt "%s" s
  | FlatTuple -> fprintf fmt "nested tuples are forbidden"
  | UndefinedOutputs l ->
      fprintf fmt "those output variables are undefined:%a"
        (fun fmt -> List.iter (fun x -> fprintf fmt " %s" x)) l
  | InputVar s -> fprintf fmt "%s is an input variable" s
  | Causality -> fprintf fmt "problem of causality"
  | BadMain (t_in, t_out) ->
    fprintf fmt "The main node has type %a -> %a but is expected to have type %a -> bool"
      print_type t_in print_type t_out
      print_type t_in
  | BadMerge ->
    fprintf fmt "There must be two merge branches with one that matches true and the other false"
  | UnknownConstructor s -> fprintf fmt "[%s] constructor doesn't appear in automaton definition" s
  | UnknownAdtType s -> fprintf fmt "[%s] type unfound" s
  | Unreachable s -> fprintf fmt "Internal error (should be an unreachable point) [%s]" s
  | NotExhaustiveMerge s -> fprintf fmt "the merge is not exhaustive over [%s] type" s

let int_of_real = Ident.make "int_of_real" Ident.Prim
let real_of_int = Ident.make "real_of_int" Ident.Prim

module Delta = struct

  let prims = [
    "int_of_real", (int_of_real, ([Treal] , [Tint])) ;
    "real_of_int", (real_of_int, ([Tint] , [Treal])) ; ]

  let nodes = Hashtbl.create 97

  let is_primitive f = List.mem_assoc f prims

  let find n =
    try Hashtbl.find nodes n , false with
      Not_found -> List.assoc n prims , true

  let add x t =
    let x' = Ident.make x Ident.Node in
    Hashtbl.replace nodes x (x', t);
    x'

  let save () = Hashtbl.fold (fun key (_,ty) env -> (key,ty)::env) nodes []
end

type io = Vinput | Vpatt
module Gamma = struct

  type t = (Ident.t * base_ty * io) M.t

  let empty = M.empty

  let add loc env x t io =
    let prefix = "print_var" in
    let kind = if String.starts_with ~prefix x then Ident.Print else Ident.Stream in
    if M.mem x env then error loc (Clash x);
    let x' = Ident.make x kind in
    M.add x (x',t,io) env

  let adds loc io =
    List.fold_left (fun env (x,t) -> add loc env x t io)

  let find loc env x = try
      M.find x env
    with Not_found ->  error loc (UnboundVar x)

  let patts_vars env =
    M.fold (fun _ (x,_,io) s -> if io=Vpatt then S.add x s else s) env S.empty

  let in_vars env =
    M.fold (fun _ (x,_,io) s -> if io=Vinput then S.add x s else s) env S.empty

end

type env = { vars : (Ident.t * base_ty * io) M.t; types : adt_type list}

let base_ty_of_ty loc t =
  match t with
  | [t'] -> t'
  | _ -> error loc (ExpectedBase t)

let compatible_base actual_ty expected_ty =
  actual_ty = expected_ty

let compatible actual_ty expected_ty =
  try
    List.fold_left2
      (fun well_t ac_t ex_t ->
         let well_t' = compatible_base ac_t ex_t in
         (well_t && well_t'))
      true actual_ty expected_ty
  with Invalid_argument _ -> false


let real_expr_of_expr te =
  match te.texpr_type with
  | [Treal] -> te
  | [Tint] ->
    { texpr_desc = TE_prim (real_of_int,[te]);
      texpr_type = [Treal];
      texpr_loc = (Lexing.dummy_pos,Lexing.dummy_pos);
    }
  | _ -> assert false

let real_op_of_int_op op =
  match op with
  | Op_add -> Op_add_f
  | Op_sub -> Op_sub_f
  | Op_mul -> Op_mul_f
  | Op_div -> Op_div_f
  | _ -> op

let not_a_nested_tuple e loc =
  match e with
  | PE_tuple el ->
    List.iter
      (fun e ->
         match e.pexpr_desc with
           PE_tuple _ -> error loc FlatTuple;
         | _ -> ()) el
  | _ -> assert false

let rec is_constant env e =
  match e.texpr_desc with
  | TE_const _ -> true
  | TE_tuple el -> List.for_all (is_constant env) el
  | _ -> false

let rec const_of_expr e =
  match e.texpr_desc with
  | TE_const c -> [c]
  | TE_tuple el ->
    List.fold_right (fun e acc -> const_of_expr e @ acc) el []
  | _ -> assert false

let type_constant = function
  | Cbool _ -> [Tbool]
  | Cint _ -> [Tint]
  | Creal _ -> [Treal]
  | Cadt (s, _) -> [Tadt s]

let rec type_expr env e =
  let desc,t = type_expr_desc env e.pexpr_loc e.pexpr_desc in
  { texpr_desc = desc; texpr_type = t; texpr_loc = e.pexpr_loc; }

and type_expr_desc env loc = function
  | PE_const c ->
    TE_const c , type_constant c

  | PE_ident x ->
    let x, ty, _ = Gamma.find loc env.vars x in
    TE_ident x , [ty]

  | PE_op (Op_not, [e]) ->
    let tt = [Tbool] in
    let te = expected_type env e tt in
    TE_op (Op_not, [te]) , tt

  | PE_op (Op_sub, [e]) ->
    let te = type_expr env e in
    begin match te.texpr_type with
      | [Tint] -> TE_op (Op_sub, [te]) , [Tint]
      | [Treal] -> TE_op (Op_sub_f, [te]) , [Treal]
      | ty -> error e.pexpr_loc (ExpectedNum (ty))
    end

  | PE_op (Op_sub_f, [e]) ->
    let tt = [Treal] in
    let te = expected_type env e tt in
    TE_op (Op_sub_f, [te]) , tt

  | PE_op ((Op_and | Op_or | Op_impl as op), [e1; e2]) ->
    let tt = [Tbool] in
    let te1 = expected_type env e1 tt in
    let te2 = expected_type env e2 tt in
    TE_op (op, [te1; te2]) , tt

  | PE_op ((Op_add | Op_sub | Op_mul | Op_div as op), [e1; e2]) ->
    let te1 = type_expr env e1 in
    let te2 = type_expr env e2 in
    begin match te1.texpr_type, te2.texpr_type with
      | [Tint], [Tint] ->
        TE_op (op, [te1; te2]), [Tint]
      | [(Tint | Treal)], [(Tint | Treal)] ->
        TE_op(real_op_of_int_op op, [ real_expr_of_expr te1 ;
                                      real_expr_of_expr te2 ]),
        [Treal]
      | [(Tint | Treal)], ty -> error e2.pexpr_loc (ExpectedNum (ty))
      | ty, _ -> error e1.pexpr_loc (ExpectedNum (ty))
    end

  | PE_op (Op_mod, [e1; e2]) ->
    let tt = [Tint] in
    let te1 = expected_type env e1 tt in
    let te2 = expected_type env e2 tt in
    TE_op(Op_mod, [te1; te2]) , tt

  | PE_op ((Op_div_f | Op_mul_f | Op_sub_f | Op_add_f as op), [e1; e2]) ->
    let tt = [Treal] in
    let te1 = expected_type env e1 tt in
    let te2 = expected_type env e2 tt in
    TE_op (op, [te1; te2]), tt

  | PE_op (Op_eq | Op_neq as op, [e1; e2]) ->
    let te1 = type_expr env e1 in
    let ty1 = te1.texpr_type in
    let te2 = type_expr env e2 in
    let ty2 = te2.texpr_type in
    begin match ty1, ty2 with
      | [t1], [t2] when t1 = t2 ->
        TE_op (op, [te1; te2]), [Tbool]
      | _ ->
        error loc (Other "invalid operands to equality")
    end

  | PE_op (Op_lt | Op_le | Op_gt | Op_ge as op, [e1; e2]) ->
    let te1 = type_expr env e1 in
    let ty1 = te1.texpr_type in
    let te2 = type_expr env e2 in
    let ty2 = te2.texpr_type in
    begin match ty1, ty2 with
      | [Tint], [Tint]
      | [Treal], [Treal] ->
        TE_op (op, [te1; te2]), [Tbool]
      | _ ->
        error loc (Other "invalid operands to comparison")
    end

  | PE_op (Op_if, [e1; e2; e3]) ->
    let te1 = expected_type env e1 ([Tbool]) in
    let te2 = type_expr env e2 in
    let te3 = type_expr env e3 in
    let well_typed = compatible te2.texpr_type te3.texpr_type in
    if well_typed then
      let tt = te2.texpr_type in
      match te1 with
      | {texpr_desc = TE_ident _; _}
      (* | {texpr_desc = TE_const (Cbool _); _} *)
      | {texpr_desc = TE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge | Op_not | Op_and | Op_or), _); _} ->
          let c_true = {texpr_desc = TE_const (Cbool true); texpr_type = [Tbool]; texpr_loc = Lexing.dummy_pos, Lexing.dummy_pos} in
          let c_false = {texpr_desc = TE_const (Cbool false); texpr_type = [Tbool]; texpr_loc = Lexing.dummy_pos, Lexing.dummy_pos} in
          let then_ = {texpr_desc = TE_when (te2, "True", te1); texpr_type = tt; texpr_loc = te2.texpr_loc} in
          let else_ = {texpr_desc = TE_when (te3, "False", te1); texpr_type = tt; texpr_loc = te3.texpr_loc} in
          TE_merge (te1, [c_true, then_; c_false, else_]), tt
      | {texpr_desc = TE_app (f, args); _} ->
          failwith "ne devrait pas arriver ?"
      | {texpr_desc = TE_const (Cbool true); _} ->
          te2.texpr_desc, tt
      | {texpr_desc = TE_const (Cbool false); _} ->
          te3.texpr_desc, tt
      | _ -> error loc (Other "The condition must be an identifier")
    else
      error loc (ExpectedType (te3.texpr_type, te2.texpr_type))

  | PE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
           | Op_add | Op_sub | Op_mul | Op_div | Op_mod
           | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
           | Op_not
           | Op_and | Op_or | Op_impl
           | Op_if), []) -> error loc TooFewArguments

  | PE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
           | Op_add | Op_mul | Op_div | Op_mod
           | Op_add_f | Op_mul_f | Op_div_f
           | Op_and | Op_or | Op_impl
           | Op_if), [ _ ]) -> error loc TooFewArguments

  | PE_op (Op_not, [ _; _ ]) -> error loc TooManyArguments

  | PE_op (Op_if, [ _; _ ]) -> error loc TooFewArguments

  | PE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
           | Op_add | Op_sub | Op_mul | Op_div | Op_mod
           | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
           | Op_not
           | Op_and | Op_or | Op_impl
           | Op_if), _) -> error loc TooManyArguments

  | PE_app (f, el) ->
    begin try
        let (f, (t_in,t_out)), is_prim = Delta.find f in
        let tel = type_args env loc t_in el in
        let app_node = if is_prim then TE_prim(f, tel) else TE_app(f, tel) in
        app_node ,
        begin match t_out with
          | [] -> assert false
          | _ -> t_out
        end
      with Not_found ->
        error loc (UnboundNode f)
    end

  | PE_arrow (e1, e2) ->
    let te1 = type_expr env e1 in
    let ty1 = te1.texpr_type in
    let te2 = type_expr env e2 in
    let ty2 = te2.texpr_type in
    let well_typed = compatible ty1 ty2 in
    if well_typed then
      let true_ = {texpr_desc = TE_const (Cbool (true)); texpr_type = [Tbool]; texpr_loc = dummy_loc} in
      let false_ = {texpr_desc = TE_const (Cbool (false)); texpr_type = [Tbool]; texpr_loc = dummy_loc} in
      let cond = {texpr_desc = TE_fby (true_, false_); texpr_type = [Tbool]; texpr_loc = dummy_loc} in
      TE_op (Op_if, [cond; te1; te2;]), ty1
    else error te2.texpr_loc (ExpectedType (ty2, ty1))

  | PE_pre e ->
    let te = type_expr env e in
    let ty = te.texpr_type in
    let nil = {texpr_desc = nil (List.hd te.texpr_type); texpr_type = ty; texpr_loc = te.texpr_loc} in
    TE_fby (nil, te), ty

  | PE_when (e1, c, e2) ->
    let te1 = type_expr env e1 in
    let ty1 = te1.texpr_type in
    let te2 = type_expr env e2 in
    let ty2 = te2.texpr_type in
    if ty2 = [Tbool] then TE_when (te1, c, te2), ty1
    else begin
        match List.find_opt (fun {name; constr} -> List.mem c constr) env.types with
        | Some e when [Tadt e.name] = ty2 -> TE_when (te1, c, te2), ty1
        | _ -> error te2.texpr_loc (ExpectedType(ty2, [Tbool])) (* probleme ici pour when classique, voir ex/neg001 *)
      end

  | PE_tuple el as n ->
    not_a_nested_tuple n loc;
    let tel = List.map (type_expr env) el in
    TE_tuple tel,
    (List.map (fun e -> base_ty_of_ty e.texpr_loc e.texpr_type) tel)

  | PE_merge (x, l) ->
      let id = (match x.pexpr_desc with PE_ident id -> id | _ -> assert false) in
      let mergebody = List.map (fun (l, r) -> type_expr env l, type_expr env r) l in
      let t_match, t_expr = ref None, ref None in
      List.iter (fun ({texpr_type = t1; texpr_loc = l1; _}, {texpr_type = t2; texpr_loc = l2; _}) ->
        match !t_match, !t_expr with
          | None, None ->
            t_match := Some t1; t_expr := Some t2;
          | Some t1', Some t2' ->
            if compatible t1 t1' then
              if compatible t2 t2' then
                ()
              else
                error l2 (ExpectedType (t2', t2))
            else
              error l1 (ExpectedType (t1', t1))
          | _ -> assert false)
        mergebody;
      let id_loc = x.pexpr_loc in
      let x, ty, _ = Gamma.find id_loc env.vars id in
      begin match ty with
        | Tbool -> ()
        | Tadt tname ->
            verif_mergebody env tname mergebody id_loc
        | _ -> error id_loc (ExpectedType ([ty], [Tadt "?t"]))
      end;
      let typ = (snd @@ List.hd mergebody).texpr_type in
      let exr =
        { texpr_desc = TE_ident x;
          texpr_type = [ty];
          texpr_loc  = id_loc }
      in
      TE_merge (exr, mergebody), typ
  | PE_print (s, e) ->
      let te = List.map (type_expr env) e in
      TE_print (s, te), (List.hd te).texpr_type

  | PE_reset (id, el, e) ->
    begin try
        let (f, (t_in, t_out)), is_prim = Delta.find id in
        let tel = type_args env loc t_in el in
        if is_prim then
          error loc (Other "You cannot reset a primitive call");
        let te = type_expr env e in
        if te.texpr_type <> [Tbool] then
          error loc (ExpectedType ([Tbool], te.texpr_type));
        TE_reset (f, tel, te), begin match t_out with
          | [] -> assert false
          | _ -> t_out
        end
      with Not_found ->
        error loc (UnboundNode id)
    end

and verif_mergebody env tname l id_loc =
  let tl = match List.find_opt (fun {name; _} -> name = tname) env.types with
    | None -> error id_loc (UnknownAdtType tname)
    | Some e -> e in
  let compare_f a b = match a, b with
    | {texpr_desc = TE_const (Cadt (_, Some id1)); _},
      {texpr_desc = TE_const (Cadt (_, Some id2)); _} -> compare id1 id2
    | _ -> assert false
  in
  let sl = List.map fst l |> List.sort compare_f in
  let stl = List.sort compare tl.constr in

  try
    (* may raise Invalid arg if lists aren't same length *)
    if not @@ List.for_all2
        (fun a b -> match a, b with
          | {texpr_desc = TE_const (Cadt (_, Some id1)); _}, id -> id1 = id
          | _ -> assert false)
        sl stl
    then
      error id_loc (NotExhaustiveMerge tname)
  with Invalid_argument _ ->
      error id_loc (NotExhaustiveMerge tname)

and type_args env loc params_ty el =
  let tel = List.map (type_expr env) el in
  let actual_types =
    List.rev
      begin
        List.fold_left
          (fun res te -> List.rev_append te.texpr_type res)
          [] tel
      end
  in
  let well_typed =
    compatible actual_types params_ty
  in
  if well_typed then tel
  else error loc (ExpectedType (actual_types, params_ty));


and expected_type env e tt =
  let te = type_expr env e in
  let ty = te.texpr_type in
  if ty = tt then te
  else error e.pexpr_loc (ExpectedType (ty, tt))

and expected_base_type env e =
  let te = type_expr env e in
  match te.texpr_type with
  | [_] -> te
  |  _ ->  error e.pexpr_loc (ExpectedBase (te.texpr_type))

let rec type_patt env p =
  let desc, t = type_patt_desc env p.ppatt_loc p.ppatt_desc in
  { tpatt_desc = desc; tpatt_type = t; tpatt_loc = p.ppatt_loc; }

and type_patt_desc env loc patt =
  match patt with
  | PP_ident x -> begin
      let x, ty =
        match Gamma.find loc env.vars x with
        | x, t, Vpatt -> x, t
        | _  -> error loc (InputVar x)
      in
      [x], [ty]
    end
  | PP_tuple pl ->
    let pl_tyl =
      List.map
        (fun x ->
           match Gamma.find loc env.vars x with
           | x, ty, Vpatt -> x, ty
           | _  -> error loc (InputVar x)
        ) pl
    in
    List.split pl_tyl

let type_equation env eq =
  match eq with
  | PE_eq eq ->
      let patt = type_patt env eq.peq_patt in
      let expr = type_expr env eq.peq_expr in
      let well_typed = compatible expr.texpr_type patt.tpatt_type in
      if well_typed then
        { teq_patt = patt; teq_expr = expr; }
      else
        error
          eq.peq_expr.pexpr_loc (ExpectedType (expr.texpr_type, patt.tpatt_type))
  | PE_match _ ->
      failwith "not implemented 4"
  | PE_automaton {pautom_loc = loc; _} ->
      error loc (Unreachable "uncompiled automaton/printf")
  | PE_print     (_, el) ->
      (match el with
       | [] -> assert false
       | e :: _ -> error e.pexpr_loc (Unreachable "uncompiled automaton/printf"))

(* let type_case env adt {pn_case; pn_cond; pn_out} =
  (* let env = Gamma.adds n.pn_loc Vpatt Gamma.empty (n.pn_output@n.pn_local) in
  let env = Gamma.adds n.pn_loc Vinput env n.pn_input in *)
let {pn_constr; pn_equation; pn_loc } = pn_case in
  let tn_equation = type_equation env pn_equation in
  let tn_cond = type_expr env pn_cond in

  let tn_constr =
    if List.mem pn_constr adt then pn_out
    else error pn_loc (UnknownConstructor pn_constr) in
  let tn_out =
    if List.mem pn_out adt then pn_out
    else error pn_loc (UnknownConstructor pn_out) in

  { tn_case = {tn_constr; tn_equation}; tn_cond; tn_out } *)

(* let type_automaton env automaton =
  let adt = List.fold_left (fun acc e -> e.pn_case.pn_constr :: acc) [] automaton in
  List.map (type_case env adt) automaton *)

let add_vars_of_patt loc s eq =
  let add x s =
    if S.mem x s then error loc (Clash x.Ident.name);
    S.add x s
  in
  List.fold_left (fun s x -> add x s) s eq.teq_patt.tpatt_desc

let check_outputs loc env equs =
  let s = List.fold_left (add_vars_of_patt loc) S.empty equs in
  let not_defined = S.diff (Gamma.patts_vars env.vars) s in
  if not (S.is_empty not_defined) then
    error loc (UndefinedOutputs
                 (List.map (fun x -> x.Ident.name) (S.elements not_defined)))

let type_node ptypes n =
  let env = Gamma.adds n.pn_loc Vpatt Gamma.empty (n.pn_output@n.pn_local) in
  let env = Gamma.adds n.pn_loc Vinput env n.pn_input in
  let env = { vars = env; types = ptypes } in
  let equs = List.map (type_equation env) n.pn_equs in
  check_outputs n.pn_loc env equs;
  let t_in = List.map (fun (_, ty) -> ty) n.pn_input in
  let t_out = List.map (fun (_, ty) -> ty) n.pn_output in
  let name = Delta.add n.pn_name (t_in,t_out) in
  let input =
    List.map
      (fun (x, ty) -> let x', _, _ = Gamma.find n.pn_loc env.vars x in (x', ty))
      n.pn_input
  in
  let output =
    List.map
      (fun (x, ty) -> let x', _, _ = Gamma.find n.pn_loc env.vars x in (x', ty))
      n.pn_output
  in
  let local =
    List.map
      (fun (x, ty) -> let x', _, _ = Gamma.find n.pn_loc env.vars x in (x', ty), None)
      n.pn_local
  in
  let node =
    { tn_name = name;
      tn_input = input;
      tn_output = output;
      tn_local = local;
      tn_equs = equs;
      tn_loc = n.pn_loc; }
  in
  node

let check_main ft main =
  let (_, ty), is_prim =
    try Delta.find main with Not_found -> error dummy_loc (UnboundNode main)
  in
  if is_prim then errors dummy_loc "The main node cannot be a primitive function"
  (* match ty, is_prim with
  | (_, [Tbool]), false -> ()
  | (t_in, t_out), false ->
    let n = List.find (fun n -> n.tn_name.Ident.name = main) (List.rev ft) in
    error n.tn_loc (BadMain (t_in, t_out))
  | _ -> errors dummy_loc "The main node cannot be a primitive function" *)

let translate_types = Fun.id

let type_file f main =
  let ft = List.map (type_node f.p_types) (f.p_nodes) in
  let tt = List.map translate_types f.p_types in
  if main <> "" then check_main ft main;
  { t_nodes = ft; t_types = tt }
