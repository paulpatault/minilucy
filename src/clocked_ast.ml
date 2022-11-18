open Asttypes
open Clocks

type clocked_var = Ident.t * base_ty * ck

type c_expr = {
  cexpr_desc: c_expr_desc;
  cexpr_type: ty;
  cexpr_clock: ct;
  cexpr_loc: location;
}

and c_expr_desc =
  | CE_const of const
  | CE_ident of Ident.t
  | CE_op of op * c_expr list
  | CE_app of Ident.t * c_expr list
  | CE_prim of Ident.t * c_expr list
  | CE_arrow of c_expr * c_expr
  | CE_pre of c_expr
  | CE_tuple of c_expr list
  | CE_merge of Ident.t * c_expr * c_expr
  | CE_fby of c_expr * c_expr
  | CE_when of c_expr * bool * Ident.t

type c_patt =
  { cpatt_desc: Ident.t list;
    cpatt_type: ty;
    cpatt_clock: ct;
    cpatt_loc: location; }

type c_equation =
  | CE_eq of c_basic_equation

and c_basic_equation =
  { ceq_patt: c_patt;
    ceq_expr: c_expr; }


type c_node =
  { cn_name: Ident.t;
    cn_input: clocked_var list;
    cn_output: clocked_var list;
    cn_local: clocked_var list;
    cn_equs: c_equation list;
    cn_loc: location; }

type c_file = c_node list
