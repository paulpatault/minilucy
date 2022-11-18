open Format
open Asttypes
open Parse_ast

let rec print_list_sp f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list_sp f sep) t

let rec print_list_nl f fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a@\n%a" f h (print_list_nl f) t

let rec print_list_eol f sep fmt l = match l with
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let print_const fmt c = match c with
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let print_op fmt op = match op with
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
  | Op_not -> fprintf fmt "~"
  | Op_and -> fprintf fmt "and"
  | Op_or -> fprintf fmt "or"
  | Op_impl -> fprintf fmt "impl"
  | Op_if -> fprintf fmt "ite"
