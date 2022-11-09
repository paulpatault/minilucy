(* Arbres de syntaxe abstraite typés *)

open Asttypes

type typed_var = Ident.t * base_ty

type t_expr =
    { texpr_desc: t_expr_desc;
      texpr_type:  ty;
      texpr_loc: location; }

and t_expr_desc =
  | TE_const of const
  | TE_ident of Ident.t
  | TE_op of op * t_expr list
  | TE_app of Ident.t * t_expr list
  | TE_prim of Ident.t * t_expr list
  | TE_arrow of t_expr * t_expr
  | TE_pre of t_expr
  | TE_tuple of t_expr list
  | TE_merge of Ident.t * t_expr * t_expr (* true, false *)
  | TE_fby of t_expr * t_expr
  | TE_when of t_expr * bool * Ident.t

type t_patt =
    { tpatt_desc: Ident.t list;
      tpatt_type: ty;
      tpatt_loc: location; }

type constr = string

type t_equation =
  | TE_eq of t_basic_equation
  | TE_automaton of t_automaton list
  | TE_match of t_expr * t_case list

and t_basic_equation = { teq_patt: t_patt; teq_expr: t_expr; }

and t_automaton =
  { tn_case: t_case;
    tn_cond: t_expr;
    tn_out:  constr;
  }

and t_case =
  { tn_constr:    string;
    tn_equation: t_equation ; }

type t_node =
    { tn_name: Ident.t;
      tn_input: typed_var list;
      tn_output: typed_var list;
      tn_local: typed_var list;
      tn_equs: t_equation list;
      tn_loc: location; }

type t_file = t_node list
