(* code d'Adrien Guatto *)

open Format
open Asttypes
open Clocked_ast
open Clocks
open Print_base

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

let pp_comma fmt () =
  fprintf fmt " ,"

let pp_const fmt c = match c with
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let pp_op fmt op = match op with
  | Op_eq -> fprintf fmt "eq"
  | Op_neq -> fprintf fmt "neq"
  | Op_lt -> fprintf fmt "lt"
  | Op_le -> fprintf fmt "le"
  | Op_gt -> fprintf fmt "gt"
  | Op_ge -> fprintf fmt "ge"
  | Op_add -> fprintf fmt "add"
  | Op_sub -> fprintf fmt "sub"
  | Op_mul -> fprintf fmt "mul"
  | Op_div -> fprintf fmt "div"
  | Op_mod -> fprintf fmt "mod"
  | Op_add_f -> fprintf fmt "add_f"
  | Op_sub_f -> fprintf fmt "sub_f"
  | Op_mul_f -> fprintf fmt "mul_f"
  | Op_div_f -> fprintf fmt "div_f"
  | Op_not -> fprintf fmt "not"
  | Op_and -> fprintf fmt "and"
  | Op_or -> fprintf fmt "or"
  | Op_impl -> fprintf fmt "impl"
  | Op_if -> fprintf fmt "ite"

let rec pp_exp fmt e = match e.cexpr_desc with
  | CE_const c -> pp_const fmt c
  | CE_ident x -> fprintf fmt "%a" Ident.print x
  | CE_op (op, el) -> fprintf fmt "%a(%a)" pp_op op pp_arg_list el
  | CE_app (name, e_list) | CE_prim (name, e_list) ->
      fprintf fmt "%a(@[%a@])" Ident.print name pp_arg_list e_list
  | CE_arrow (l, r) ->
      fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" pp_exp l pp_exp r
  | CE_pre e ->
      fprintf fmt "pre (@[%a@])" pp_exp e
  | CE_tuple e_list ->
      fprintf fmt "(@[%a@])" pp_tuple_arg_list e_list
  | CE_merge (m, l) ->
      fprintf fmt "@[merge %a @\n  @[%a@]@]" pp_exp m
        (print_list_nl (fun fmt (id,exp) -> fprintf fmt "(%s -> %a)" id pp_exp exp)) l
  | CE_fby (e1, e2) ->
    fprintf fmt "@[%a fby %a@]" pp_exp e1 pp_exp e2
  | CE_when (e1, s, eid) ->
    fprintf fmt "@[%a when %s(%a)@]" pp_exp e1 s pp_exp eid

and pp_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" pp_exp x
  | h :: t -> fprintf fmt "%a,@ %a" pp_exp h pp_arg_list t

and pp_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" pp_exp x
  | h :: t -> fprintf fmt "%a,@ %a" pp_exp h pp_arg_list t

and pp_const_exp fmt ce_list = match ce_list with
  | [] -> assert false
  | [c] -> fprintf fmt "%a" pp_const c
  | h :: t -> fprintf fmt "%a,@ %a" pp_const h pp_const_exp t

let pp_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (pp_print_list ~pp_sep:pp_comma Ident.print) eq.ceq_patt.cpatt_desc
    pp_exp eq.ceq_expr

(* let pp_type = pp_list pp_cbase_type "*" *)

let pp_var fmt (name, ty, ck) =
  fprintf fmt "%a : %a (%a)"
    Ident.print name
    print_base_type ty
    pp_ck ck

let pp_var_init fmt ((name, ty, ck), v) =
  fprintf fmt "%a : %a (%a) init %s"
    Ident.print name
    print_base_type ty
    pp_ck ck
    v

let pp_var_list fmt =
  pp_print_list ~pp_sep:pp_comma pp_var fmt

let pp_var_init_list fmt =
  pp_print_list ~pp_sep:pp_comma pp_var_init fmt

let pp_eol fmt () =
  fprintf fmt ";@\n"

let pp_node fmt nd =
  fprintf fmt
    "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\nlocal @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print nd.cn_name
    pp_var_list nd.cn_input
    pp_var_list nd.cn_output
    pp_var_list nd.cn_local
    pp_var_init_list nd.cn_init_local
    (pp_print_list ~pp_sep:pp_eol pp_eq) nd.cn_equs

let pp fmt =
  fprintf fmt "%a@." (pp_print_list ~pp_sep:pp_print_newline pp_node)

(* let print_file file *)
