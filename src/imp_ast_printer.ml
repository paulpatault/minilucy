open Format
open Clocked_ast_printer
open Imp_ast
open Print_base

let rec pp_iexp fmt e = match e.iexpr_desc with
  | IE_const c -> pp_const fmt c
  | IE_mem   i
  | IE_ident i -> Ident.print fmt i
  | IE_op (op, el) -> fprintf fmt "%a(%a)" pp_op op pp_arg_list el
  | IE_tuple el           -> fprintf fmt "(@[%a@])" pp_tuple_arg_list el
  | IE_app   (i1, i2, el) -> fprintf fmt "%a <- %a(@[%a@])" Ident.print i1 Ident.print i2 pp_arg_list el
  | IE_prim  (i, el)      -> fprintf fmt "%a(@[%a@])" Ident.print i pp_arg_list el
  | IE_case  (e, li)       ->
      fprintf fmt "case(%a)@[<hov 2>%a@]"
        pp_iexp e
        (pp_print_list ~pp_sep:pp_eol (fun fmt (l, r) -> fprintf fmt "%a: %a"pp_iexp l pp_iexp r)) li
  | IE_print (s, e) ->
      fprintf fmt "print(%S, @[%a@])" s (pp_print_list pp_iexp) e
  | IE_reset (i1, i2, el, e) ->
    fprintf fmt "%a <- %a(@[@%a]) every %a" Ident.print i1 Ident.print i2 pp_arg_list el pp_iexp e

and pp_arg_list fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" pp_iexp x
  | h :: t -> fprintf fmt "%a,@ %a" pp_iexp h pp_arg_list t

and pp_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" pp_iexp x
  | h :: t -> fprintf fmt "%a,@ %a" pp_iexp h pp_arg_list t

let pp_ieq fmt {ieq_patt; ieq_expr} =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (pp_print_list ~pp_sep:pp_comma pp_var) ieq_patt
    pp_iexp ieq_expr

let pp_node_mem_init fmt (i1, i2) = fprintf fmt "(%a,%a)" Ident.print i1 Ident.print i2

let pp_mem fmt {fby_mem; node_mem} =
  fprintf fmt "node=[@[%a@]]@\nfby=[@[%a@]]"
    (pp_print_list ~pp_sep:pp_comma pp_node_mem_init) node_mem
    (pp_print_list ~pp_sep:pp_comma pp_var) fby_mem

let pp_atom fmt = function
  | Const c -> pp_const fmt c
  | Ident i -> Ident.print fmt i

let pp_ident_atom fmt (id, at) =  fprintf fmt "(%a,%a)" Ident.print id pp_atom at

let pp_init fmt {fby_init;node_init} =
  fprintf fmt "node=[@[%a@]]@\nfby=[@[%a@]]"
    (pp_print_list ~pp_sep:pp_comma pp_node_mem_init) node_init
    (pp_print_list ~pp_sep:pp_comma pp_ident_atom) fby_init

let pp_update fmt m  = fprintf fmt "todo"

let pp_compute fmt eq_list =
  fprintf fmt "%a"
    (pp_print_list ~pp_sep:pp_eol pp_ieq) eq_list

let pp_update fmt up =
  fprintf fmt "%a"
    (pp_print_list ~pp_sep:pp_eol pp_ident_atom) up

let pp_node fmt (nd: Imp_ast.i_node) =
  fprintf fmt
    "@[node %a(@[%a@])@\n  returns (@[%a@])@\nvar @[%a;@]@\n@]"
    Ident.print nd.in_name
    pp_var_list nd.in_input_step
    pp_var_list nd.in_output_step
    pp_var_init_list nd.in_local;

  fprintf fmt "  @[<v>mem={@[%a@]}@\ninit={@[%a@]}@\ncompute={@[%a@]}@\nupdate={@[%a@]}@\n@]"
    pp_mem nd.in_mem
    pp_init nd.in_init
    pp_compute nd.in_compute
    pp_update nd.in_update

let pp fmt (f : Imp_ast.i_file) =
  fprintf fmt "%a@\n@\n%a@."
  print_adttype_list_std f.i_types
  (pp_print_list ~pp_sep:pp_print_newline pp_node) f.i_nodes
