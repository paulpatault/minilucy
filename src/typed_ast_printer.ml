(* code d'Adrien Guatto *)

open Format
open Asttypes
open Typed_ast
open Print_base

let rec print_exp fmt e = match e.texpr_desc with
  | TE_const c -> print_const fmt c
  | TE_constr s -> fprintf fmt "%s" s
  | TE_default s -> fprintf fmt "default(%s)" s
  | TE_ident x -> fprintf fmt "%a" Ident.print x
  | TE_op (op, el) -> fprintf fmt "%a(%a)" print_op op print_arg_list el
  | TE_app (name, e_list) | TE_prim (name, e_list) ->
      fprintf fmt "%a(@[%a@])" Ident.print name print_arg_list e_list
  | TE_arrow (l, r) ->
      fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" print_exp l print_exp r
  | TE_pre e ->
      fprintf fmt "pre (@[%a@])" print_exp e
  | TE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list
  | TE_merge (name, l) ->
    fprintf fmt "@[merge %a @\n  @[%a@]@]" Ident.print name
    (print_list_nl (fun fmt (id,exp) -> fprintf fmt "(%s -> %a)" id print_exp exp)) l
  | TE_fby (e1, e2) ->
    fprintf fmt "@[%a fby %a@]" print_exp e1 print_exp e2
  | TE_when (e1, s, id) ->
    fprintf fmt "@[%a when %s(%a)@]" print_exp e1 s Ident.print id

and print_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_const_exp fmt ce_list = match ce_list with
  | [] -> assert false
  | [c] -> fprintf fmt "%a" print_const c
  | h :: t -> fprintf fmt "%a,@ %a" print_const h print_const_exp t

let print_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list_sp Ident.print ",") eq.teq_patt.tpatt_desc
    print_exp eq.teq_expr

(* let print_type = print_list print_cbase_type "*" *)

let print_var_dec fmt (name, ty) =
  fprintf fmt "%a : %a" Ident.print name print_base_type ty

let rec print_var_dec_list = print_list_sp print_var_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print nd.tn_name
    print_var_dec_list nd.tn_input
    print_var_dec_list nd.tn_output
    print_var_dec_list nd.tn_local
    (print_list_eol print_eq ";") nd.tn_equs

let print_node_list_std ndl =
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl
