open Asttypes
open Clocks

type atom =
  | Const of const
  | Ident of Ident.t

type cvar = Ident.t * base_ty * ck

type mem = {
  fby_mem: cvar list;
  node_mem: (Ident.t * Ident.t) list
}

type init = {
  fby_init: (Ident.t * atom) list;
  node_init: (Ident.t * Ident.t) list;
}

and i_expr = {
  iexpr_desc: i_expr_desc;
  iexpr_type: base_ty list;
}

and i_expr_desc =
  | IE_const of const
  | IE_ident of Ident.t
  | IE_mem of Ident.t
  | IE_op of op * i_expr list
  | IE_app of Ident.t * Ident.t * i_expr list
  | IE_prim of Ident.t * i_expr list
  | IE_tuple of i_expr list
  | IE_case of i_expr * (i_expr * i_expr) list
  | IE_print of i_expr
  | IE_reset of Ident.t * Ident.t * i_expr list * i_expr

type i_equation = {
  ieq_patt: cvar list;
  ieq_expr: i_expr;
}

type i_node = {
  in_name: Ident.t;
  in_input_step: cvar list;
  in_output_step: cvar list;
  in_local: (cvar * const option) list;
  in_mem: mem;
  in_init: init;
  in_compute: i_equation list;
  in_update: (Ident.t * atom) list;
  need_mem: bool;
}

type i_file = { i_nodes: i_node list; i_types: adt_type list }
