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
  | PE_merge_adt of p_expr * (constr * p_expr) list
  | PE_constr of constr

type p_patt =
  { ppatt_desc: p_patt_desc;
    ppatt_loc: location; }

and p_patt_desc =
  | PP_ident of ident
  | PP_tuple of ident list

and p_equation =
  | PE_eq of p_eq
  | PE_automaton of p_automaton
  | PE_match of p_expr * p_case list

and p_eq = { peq_patt: p_patt; peq_expr: p_expr; }

and p_automaton =
  {pautom : p_automaton_core list; pautom_loc: location}

and p_automaton_core =
  { pn_case: p_case;
    pn_cond: p_expr;
    pn_out: constr;
  }

and p_case =
  { pn_constr: constr;
    (* pn_locals:  *)
    pn_equation: p_equation;
    pn_loc: location;
  }

type p_node =
    { pn_name: ident;
      pn_input: (ident * base_ty) list;
      pn_output: (ident * base_ty) list;
      pn_local: (ident * base_ty) list;
      pn_init_local: (ident * base_ty * constr) list;
      pn_equs: p_equation list;
      pn_loc: location;
      (* pn_reset: ident option; *)
    }

type p_type = { pt_name: ident; pt_constr: constr list }

type p_file =
  { p_types: p_type list;
    p_nodes: p_node list;
  }
