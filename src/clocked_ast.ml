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
  | CE_merge of c_expr * (string * c_expr) list
  | CE_fby of c_expr * c_expr
  | CE_when of c_expr * constr * c_expr

type c_patt =
  { cpatt_desc: Ident.t list;
    cpatt_type: ty;
    cpatt_clock: ct;
    cpatt_loc: location; }

type c_equation =
  { ceq_patt: c_patt;
    ceq_expr: c_expr; }


type c_node =
  { cn_name: Ident.t;
    cn_input: clocked_var list;
    cn_output: clocked_var list;
    cn_local: clocked_var list;
    cn_init_local: (clocked_var * constr) list;
    cn_equs: c_equation list;
    cn_loc: location; }

type c_file = c_node list
