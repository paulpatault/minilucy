(* code d'Adrien Guatto *)

open Format
open Asttypes
open Parse_ast
open Print_base

let rec print_exp fmt e = match e.pexpr_desc with
  | PE_const c -> print_const fmt c
  | PE_constr x
  | PE_ident x -> fprintf fmt "%s" x
  | PE_op (op, el) -> fprintf fmt "%a(%a)" print_op op print_arg_list el
  | PE_app (name, e_list) ->
      fprintf fmt "%s(@[%a@])" name print_arg_list e_list
  | PE_arrow (l, r) ->
      fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" print_exp l print_exp r
  | PE_pre e ->
      fprintf fmt "pre (@[%a@])" print_exp e
  | PE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list
  | PE_merge (name, e1, e2) ->
    fprintf fmt "@[merge %a %a@]" print_exp name
    (print_list_sp (fun fmt (b, exp) -> fprintf fmt "@\n(%b -> %a)" b print_exp exp) "") [e1;e2]
  | PE_merge_adt (name, l) ->
    fprintf fmt "@[merge %a @\n  @[%a@]@]" print_exp name
    (print_list_nl (fun fmt (id,exp) -> fprintf fmt "(%s -> %a)" id print_exp exp)) l

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

let print_ident fmt = function
  | PP_ident s -> fprintf fmt "%s" s
  | PP_tuple t -> print_list_sp (fun fmt s -> fprintf fmt "%s" s) "," fmt t

let print_eq fmt = function
  | PE_eq eq ->
      fprintf fmt "@[(%a) = @[%a@]@]"
        print_ident eq.peq_patt.ppatt_desc
        print_exp eq.peq_expr
  | PE_automaton autom ->
      fprintf fmt "@[automaton ...@]" (* TODO *)
  | PE_match _ ->
      fprintf fmt "@[match ...@]" (* TODO *)

let print_var_dec fmt (name, ty) =
  fprintf fmt "%s : %a" name print_base_type ty

let print_var_init_dec fmt (name, ty, init) =
  fprintf fmt "%s: %a init %s" name print_base_type ty init

let rec print_var_dec_list = print_list_sp print_var_dec ";"

let rec print_var_init_dec_list = print_list_sp print_var_init_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %s(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\nlocal @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    nd.pn_name
    print_var_dec_list nd.pn_input
    print_var_dec_list nd.pn_output
    print_var_dec_list nd.pn_local
    print_var_init_dec_list nd.pn_init_local
    (print_list_eol print_eq ";") nd.pn_equs

let print_node_list_std fmt ndl =
  List.iter (fun nd -> Format.fprintf fmt "%a@\n@." print_node nd) ndl

let print_adttype_list_std fmt tl =
  fprintf fmt "%a"
    (print_list_nl
      (fun fmt {pt_name; pt_constr} ->
        fprintf fmt "type %s =@\n  @[%a@]"
          pt_name
          (print_list_nl (fun fmt e -> fprintf fmt "| %s" e))
          pt_constr))
    tl

let print_file_std {p_types; p_nodes} =
  Format.printf "%a@\n@\n%a"
  print_adttype_list_std p_types
  print_node_list_std p_nodes
