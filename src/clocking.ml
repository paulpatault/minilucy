open Asttypes
open Typed_ast
open Clocked_ast
open Ident
open Format
open Clocks

module S = Set.Make(Ident)
module M = Map.Make(Ident)

type error =
  | ExpectedClock of ct * ct
  | ExpectedBaseClock of ct
  | UnboundVar of Ident.t
  | UnboundNode of string
  | Clash of Ident.t
  | Other of string
  | InputVar of string
  | BadMerge
  | NotNormalized
  | Unreachable
  | EmptyClock of Ident.t
  | Causality

let rec pp_ck fmt = function
  | Cbase -> fprintf fmt "Base"
  | Con (c, s, id) ->
    fprintf fmt "@[%a on %s(%a)@]"
      pp_ck c
      s
      Ident.print id
  | (Cvar {contents = Clink _}) as c ->
    let c = ck_repr c in
    pp_ck fmt c
  | (Cvar {contents = Cindex i}) ->
    fprintf fmt "@[ck%i@]" i


let rec pp_ct fmt = function
  | Ck ck -> fprintf fmt "%a" pp_ck ck
  | Cprod cl ->
    fprintf fmt "(%a)"
      (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt " * ") pp_ct) cl

let report fmt = function
  | UnboundVar id -> fprintf fmt "unbound variable %a" Ident.print id
  | UnboundNode id -> fprintf fmt "unbound node %s" id
  | Clash id -> fprintf fmt "The variable %a is defined several times" Ident.print id
  | Other s -> fprintf fmt "%s" s
  | InputVar s -> fprintf fmt "%s is an input variable" s
  | BadMerge ->
    fprintf fmt "There must be two merge branches with one that matches true and the other false"
  | ExpectedClock (c1, c2) ->
    fprintf fmt "The expected clock is %a, got %a" pp_ct c1 pp_ct c2
  | ExpectedBaseClock cl ->
    fprintf fmt "A base clock was expected, got %a" pp_ct cl
  | EmptyClock id ->
    fprintf fmt "Found an empty clock for %a" Ident.print id
  | NotNormalized ->
    fprintf fmt "A unexpected expression was found"
  | Unreachable ->
    fprintf fmt "Unreachable"
  | Causality ->
    fprintf fmt "Causality"

exception Error of location * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let error loc e = raise (Error (loc, e))
let errors loc s = error loc (Other s)

let int_of_real = Ident.make "int_of_real" Ident.Prim
let real_of_int = Ident.make "real_of_int" Ident.Prim

module Delta = struct

  (* let prims = [ *)
  (*   "int_of_real", (int_of_real, ([Treal] , [Tint])) ; *)
  (*   "real_of_int", (real_of_int, ([Tint] , [Treal])) ; ] *)

  let nodes = Hashtbl.create 97

  (* let is_primitive f = List.mem_assoc f prims *)

  let find n =
    Hashtbl.find nodes n.name


  let add x' t =
    let x = x'.name in
    Hashtbl.replace nodes x (x', t);
    x'

  let save () = Hashtbl.fold (fun key (_,ty) env -> (key,ty)::env) nodes []
end

type io = Vinput | Vlocal | Voutput
module Gamma = struct

  type t = ck M.t

  let empty = M.empty

  let add loc env x c =
    if M.mem x env then error loc (Clash x);
    M.add x c env

  let adds loc = function
    | Vlocal -> List.fold_left (fun env (x,t) -> add loc env x Cbase)
    | Vinput -> List.fold_left (fun env (x,t) -> add loc env x Cbase )
    | Voutput -> List.fold_left (fun env (x,t) -> add loc env x (fresh_ck ()) )

  let find loc env x = try
    M.find x env
  with Not_found ->  error loc (UnboundVar x)

end

let full_clock c1 c2 env =
  match c1, c2 with
  | Ck Cbase, Ck Cbase -> Cbase
  | Ck (Con (ck1, b1, i1)), Ck (Con (ck2, b2, i2)) when b1 <> b2 ->
    begin
      try unify_ck ck1 ck2;
        ck1
      with Unify ->
        error dummy_loc (ExpectedClock (Ck ck1, Ck ck2))
    end
  | _ -> error dummy_loc (Other "Cannot merge clocks")

let full_clock_list types cl =
  let rec aux ck' ctors = function
    | [] -> ()
    | Ck (Con (ck, b, i))::k -> begin
        unify_ck ck ck';
        if List.mem b ctors then
          aux ck (List.filter ((<>) b) ctors) k
        else
          error dummy_loc (Other "Cannot merge clocks")
      end
    | _ -> assert false
  in
  let is_base = List.for_all (function
      | Ck Cbase -> true
      | _ -> false)
    cl
  in
  if is_base then
    Cbase
  else
    if
      not @@ List.for_all (function
            Ck (Con (ck, b, i)) ->
            true
          | _ -> false)
        cl
    then
      error dummy_loc (Other "Cannot merge clocks")
    else
      let (ck, ctor)  =
        match List.hd cl with
        | Ck (Con (ck, ctor, _)) -> ck, ctor
        | _ -> assert false
      in
      let pt = match List.find_opt (fun {constr; _} -> List.mem ctor constr) types with
        | None -> assert false
        | Some e -> e in
      aux (ck) pt.constr cl;
      ck

let rec clock_expr env types e =
  let desc, cl = clock_expr_desc env types e.texpr_loc e.texpr_desc in
  {cexpr_desc = desc; cexpr_type = e.texpr_type; cexpr_clock = cl; cexpr_loc = e.texpr_loc; }

and clock_expr_desc env types loc = function
  | TE_const c ->
      CE_const c, Ck (fresh_ck ())
  | TE_ident x ->
      let ck = Gamma.find loc env x in
      CE_ident x, Ck ck
  | TE_op (op, el) ->
      let el, cl =
        match op, el with
        | (Op_not | Op_sub | Op_sub_f), [e] ->
          let e = clock_expr env types e in
          let cl = e.cexpr_clock in
          [e], cl
        | (Op_and | Op_or | Op_impl
          | Op_add | Op_sub | Op_mul | Op_div | Op_mod
          | Op_div_f | Op_mul_f | Op_sub_f | Op_add_f
          | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge), [e1; e2] ->
          let ce1 = clock_expr env types e1 in
          let ce2 = clock_expr env types e2 in
          let ct1 = ce1.cexpr_clock in
          let ct2 = ce2.cexpr_clock in
          begin
            try
              unify ct1 ct2;
              [ce1; ce2], Ck (first_ck ct1)
            with Unify ->
              error loc (ExpectedClock (ct1, ct2))
          end
        | Op_if, [e1; e2; e3] ->
          let ce1 = clock_expr env types e1 in
          let ce2 = clock_expr env types e2 in
          let ce3 = clock_expr env types e3 in
          begin
            try unify ce1.cexpr_clock ce2.cexpr_clock
            with Unify ->
              error loc (ExpectedClock (ce1.cexpr_clock, ce2.cexpr_clock))
          end;
          begin
            try unify ce2.cexpr_clock ce3.cexpr_clock
            with Unify ->
              error loc (ExpectedClock (ce2.cexpr_clock, ce3.cexpr_clock))
          end;
          [ce1; ce2; ce3], ce1.cexpr_clock
        | _ -> error loc Unreachable

      in
      CE_op (op, el), cl
  | TE_app (f, el) ->
      let (f, (c_in, c_out)) = Delta.find f in
      let cel = clock_args env types loc c_in el in
      CE_app (f, cel), c_out
  | TE_tuple el ->
      let cel = List.map (clock_expr env types) el in
      CE_tuple cel,
      Cprod (List.map (fun {cexpr_clock; _} -> cexpr_clock) cel)
  | TE_merge (id, tes) ->
      let cid = clock_expr env types id in
      let ces = List.map (fun (l, r) -> clock_expr env types l, clock_expr env types r) tes in
      let c = full_clock_list types (List.map (fun (_, {cexpr_clock; _}) -> cexpr_clock) ces) in
      CE_merge (cid, ces), Ck c
  | TE_fby (e1, e2) ->
      let ce1 = clock_expr env types e1 in
      let ce2 = clock_expr env types e2 in
      CE_fby (ce1, ce2), ce1.cexpr_clock
  | TE_when (e, b, ({texpr_desc = TE_ident id; _} as eid)) ->
      let ck = Gamma.find loc env id in
      let ce = clock_expr env types e in
      let ceid = clock_expr env types eid in
      begin
        match ce.cexpr_clock with
        | Ck cke ->
          begin
            try unify_ck ck cke;
              CE_when (ce, b, ceid), Ck (Con (ck, b, id))
            with Unify ->
              error loc (ExpectedClock (Ck ck, ce.cexpr_clock))
          end
        | _ as ct ->
          error loc (ExpectedBaseClock ct)
      end
  | TE_when _ as e->
    Typed_ast_printer.print_exp Format.std_formatter {texpr_desc = e; texpr_loc = Lexing.dummy_pos, Lexing.dummy_pos; texpr_type = []};
    failwith "Not normalized"
  | TE_pre _
  | TE_prim _
  | TE_arrow _ -> error loc Unreachable

and clock_args env types loc params_cl el =
  let cel = List.map (clock_expr env types) el in
  let actual_clocks =
    Cprod (List.map (fun {cexpr_clock; _} -> cexpr_clock) cel)
  in
  try
    unify params_cl actual_clocks;
    cel
  with Unify ->
    error loc (ExpectedClock (actual_clocks, params_cl))

let rec clock_patt env patt cl =
  {cpatt_desc = patt.tpatt_desc; cpatt_type = patt.tpatt_type; cpatt_clock = cl; cpatt_loc = patt.tpatt_loc; },
  env

let clock_equation env types eq =
  let expr = clock_expr env types eq.teq_expr in
  let patt, env = clock_patt env eq.teq_patt expr.cexpr_clock in
  {ceq_patt = patt; ceq_expr = expr; }

let check_causality loc inputs equs =
  begin try ignore (Scheduling.schedule_equs inputs equs)
    with Scheduling.Causality ->
      List.iter (Clocked_ast_printer.pp_eq std_formatter) equs;
      error loc Causality
  end


let clock_node types n =
  let env0 = Gamma.adds n.tn_loc Vlocal Gamma.empty (List.map fst n.tn_local) in
  let env0 = Gamma.adds n.tn_loc Vinput env0 n.tn_input in

  let env = Gamma.adds n.tn_loc Voutput env0 n.tn_output in
  let equs = List.map (clock_equation env types) n.tn_equs in
  M.iter (fun _ ck -> unify_ck Cbase (root_ck_of ck)) env0;
  let rec ct_of_ck = function
    | [ck] -> Ck ck
    | cl -> Cprod (List.fold_left (fun cl ck -> (Ck ck)::cl) [] cl |> List.rev)
  in
  let c_in = ct_of_ck @@ List.map (fun (id, _) ->
    Gamma.find n.tn_loc env id) n.tn_input in
  let c_out = ct_of_ck @@ List.map (fun (id, _) ->
    Gamma.find n.tn_loc env id) n.tn_output in
  let name = Delta.add n.tn_name (c_in,c_out) in
  let input = List.map (fun (id, ty) ->
    id, ty, Gamma.find n.tn_loc env id)
      n.tn_input
  in
  let output = List.map (fun (id, ty) ->
    id, ty, Gamma.find n.tn_loc env id)
      n.tn_output
  in
  let local = List.map (fun ((id, ty), v) ->
    (id, ty, Gamma.find n.tn_loc env id), v)
      n.tn_local
  in
  let node =
    { cn_name = name;
      cn_input = input;
      cn_output = output;
      cn_local = local;
      cn_equs = equs;
      cn_loc = n.tn_loc; }
  in
  check_causality node.cn_loc input equs;
  node

let clock_file f main =
  let fc = List.map (clock_node f.t_types) f.t_nodes in
  { c_nodes = fc; c_types = f.t_types }
