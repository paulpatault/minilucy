(* Arbres de syntaxe abstraite *)

open Asttypes

type ident = string

type p_expr =
  { pexpr_desc: p_expr_desc;
    pexpr_loc: location; }

and p_expr_desc =
  | PE_const of const
  | PE_ident of ident
  | PE_op of op * p_expr list
  | PE_app of ident * p_expr list
  | PE_arrow of p_expr * p_expr
  | PE_pre of p_expr
  | PE_tuple of p_expr list
  | PE_merge of p_expr * (bool * p_expr) * (bool * p_expr)

  (* | PE_when of p_expr * p_expr *)
  (* | PE_whenot of p_expr * p_expr *)

type p_patt =
  { ppatt_desc: p_patt_desc;
    ppatt_loc: location; }

and p_patt_desc =
  | PP_ident of ident
  | PP_tuple of ident list

type p_equation =
    { peq_patt: p_patt;
      peq_expr: p_expr; }

(* automaton
   | A ->
       o1 = ... ;
       o2 = ... ;
       unless (...) then B
   | B -> ...
 *)
type p_case =
  { pn_constr: string;
    pn_cond: p_expr;
    pn_out: string;
    pn_loc: location;
    pn_equations: p_equation list;
  }

type p_node =
    { pn_name: ident;
      pn_input: (ident * base_ty) list;
      pn_output: (ident * base_ty) list;
      pn_local: (ident * base_ty) list;
      pn_equs: p_equation list;
      pn_automaton: p_case list;
      pn_loc: location;
      (* pn_reset: ident option; *)
    }

type p_file = p_node list
