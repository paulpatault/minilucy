%{

  open Asttypes
  open Parse_ast

  exception RedefinedConstructor of location * string
  exception RedefinedType of location * string

  let mk_expr e l = { pexpr_desc = e; pexpr_loc = l }
  let mk_patt p l = { ppatt_desc = p; ppatt_loc = l }

  let h_constr_ty = Hashtbl.create 10
  let h_ty_constr = Hashtbl.create 10

  (* let get_typ =
    let h = Hashtbl.create 10 in
    fun s ->
      try Hashtbl.find h s 
 *)

%}

%token AND
%token ARROW
%token BAR
%token BOOL
%token CONST
%token COLON
%token COMMA
%token <Asttypes.op> COMP
%token <bool> CONST_BOOL
%token <int> CONST_INT
%token <float> CONST_REAL
%token <string> STR
%token DIV
%token ELSE
%token END
%token EOF
%token EQUAL
%token NEQ
%token REAL
%token <string> IDENT
%token IF
%token IMPL
%token INT
%token LET
%token LPAREN
%token MINUS
%token MOD
%token NODE
%token NOT
%token OR
%token PLUS
%token PRE
%token RETURNS
%token RPAREN
%token SEMICOL
%token SLASH
%token STAR
%token TEL
%token THEN
%token VAR

%token MERGE
%token EVERY RESET
%token WHEN WHENOT

%token AUTOMATON
%token UNLESS
%token UNTIL
%token CONTINUE
%token DONE
%token TYPE
%token INIT
%token LOCAL
%token FBY
%token <string> CONSTR
%token <string * string> TCONSTR
%token PRINT
%token CONST_MAIN

%nonassoc THEN
%nonassoc ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%right WHEN WHENOT
%left COMP EQUAL NEQ                          /* < <= > >= <> = <> */
%left PLUS MINUS                              /* + -  */
%left STAR SLASH DIV MOD                      /* * /  mod */
%nonassoc uminus                              /* - */
%nonassoc NOT PRE EVERY                       /* not pre */
%right FBY
%left DOT

/* Point d'entrée */

%start file
%type <Parse_ast.p_file> file

%%

file: CONST_MAIN? type_decs node_decs EOF
  { { p_types = $2 ; p_nodes = $3; const_main_input = Option.is_some $1} }
;

type_decs:
| /* empty */       { [] }
| ptype type_decs    { $1 :: $2 }
;

node_decs:
| /* empty */       { [] }
| node node_decs    { $1 :: $2 }
;

ptype:
| TYPE tname=IDENT EQUAL BAR? l=separated_list(BAR, constr)
  { 
    if Hashtbl.mem h_ty_constr tname then raise (RedefinedType ($sloc, tname));
    Hashtbl.add h_ty_constr tname l;
    List.iter (fun e ->
       if Hashtbl.mem h_constr_ty e then raise (RedefinedConstructor ($sloc, e));
       Hashtbl.add h_constr_ty e tname) l;
    {name = tname; constr = l} }
;

node:
| NODE n=IDENT LPAREN p=in_params RPAREN
  RETURNS LPAREN op=out_params RPAREN SEMICOL
  lp=local_params
  /* lip=list(local_params_init) */
  LET eql=eq_list TEL semi_opt
    { { pn_name = n;
    pn_input = p;
    pn_output = op;
    pn_local = lp;
    (* pn_init_local = lip; *)
    pn_equs = eql;
    pn_loc = $sloc; } }
;

local_params_init:
| LOCAL id=IDENT COLON t=IDENT INIT vo=constr
    { (id, Tadt t, vo) }
;

%public %inline loc(X):
 | x = X { x &@ $sloc }
;

in_params: { [] }
| param_list
    { $1 }
;

out_params:
| param_list
    { $1 }
;

local_params:
| /* empty */
    { [] }
| VAR param_list_semicol
    { $2 }
;

param_list:
| param
    { $1 }
| param SEMICOL param_list
    { $1 @ $3 }
;

param_list_semicol:
| param  SEMICOL
    { $1 }
| param SEMICOL param_list_semicol
    { $1 @ $3 }
;

param:
| ident_comma_list COLON typ
    { let typ = $3 in
      List.map (fun id -> (id, typ)) $1 }
;

eq_list:
| eq
    { [$1] }
| eq eq_list
    { $1 :: $2 }
;

eq:
| PRINT separated_nonempty_list(COMMA, expr) SEMICOL
    { PE_print ("", $2) }
| PRINT LPAREN s=STR COMMA e=separated_list(COMMA, expr) RPAREN SEMICOL
    { PE_print (s, e) }
| pattern EQUAL expr SEMICOL
    { PE_eq { peq_patt = $1; peq_expr = $3; } }
| AUTOMATON pautom=list(case_autom) END semi_opt
    { PE_automaton ({pautom ; pautom_loc = $sloc}) }
;

case_autom:
| pn_case=case UNTIL pn_cond=expr CONTINUE pn_out=constr
  { {pn_case; pn_cond; pn_out; pn_weak=true}  }
| pn_case=case UNLESS pn_cond=expr THEN pn_out=constr
  { {pn_case; pn_cond; pn_out; pn_weak=false}  }
;

case:
| BAR pn_constr=constr ARROW pn_equation=eq
  { {pn_constr; pn_equation; pn_loc = $sloc} }
;

pattern:
| IDENT
    { mk_patt (PP_ident $1) $sloc}
| LPAREN IDENT RPAREN
    { mk_patt (PP_ident $2) $sloc}
| LPAREN IDENT COMMA ident_comma_list RPAREN
    { mk_patt (PP_tuple($2::$4)) $sloc}
;


expr:
| LPAREN expr RPAREN
    { $2 }
| const
    { mk_expr (PE_const $1) $sloc }
| IDENT
    { mk_expr (PE_ident $1) $sloc }
| IDENT LPAREN expr_comma_list_empty RPAREN
    { mk_expr (PE_app ($1, $3)) $sloc}
| RESET id=IDENT LPAREN el=expr_comma_list_empty RPAREN EVERY e=expr
    { mk_expr (PE_reset (id, el, e)) $sloc }
| IF expr THEN expr ELSE expr
    { mk_expr (PE_op (Op_if, [$2; $4; $6]))  $sloc}
| expr PLUS expr
    { mk_expr (PE_op (Op_add, [$1; $3]))  $sloc}
| expr MINUS expr
    { mk_expr (PE_op (Op_sub, [$1; $3]))  $sloc}
| expr STAR expr
    { mk_expr (PE_op (Op_mul, [$1; $3]))  $sloc}
| expr SLASH expr
    { mk_expr (PE_op (Op_div, [$1; $3]))  $sloc}
| expr DIV expr
    { mk_expr (PE_op (Op_div, [$1; $3]))  $sloc}
| expr MOD expr
    { mk_expr (PE_op (Op_mod, [$1; $3]))  $sloc}
| expr COMP expr
    { mk_expr (PE_op ($2, [$1; $3]))  $sloc}
| expr EQUAL expr
    { mk_expr (PE_op (Op_eq, [$1; $3]))  $sloc}
| expr NEQ expr
    { mk_expr (PE_op (Op_neq, [$1; $3]))  $sloc}
| expr AND expr
    { mk_expr (PE_op (Op_and, [$1; $3]))  $sloc}
| expr OR expr
    { mk_expr (PE_op (Op_or, [$1; $3]))  $sloc}
| expr IMPL expr
    { mk_expr (PE_op (Op_impl, [$1; $3]))  $sloc}
| expr ARROW expr
    { mk_expr (PE_arrow ($1, $3))  $sloc}
| expr FBY expr
    { mk_expr (PE_arrow ($1, mk_expr (PE_pre ($3)) $sloc))  $sloc}
| MINUS expr /* %prec uminus */
    { mk_expr (PE_op (Op_sub, [$2]))  $sloc}
| NOT expr
    { mk_expr (PE_op (Op_not, [$2]))  $sloc}
| PRE expr
    { mk_expr (PE_pre ($2))  $sloc}
| LPAREN expr COMMA expr_comma_list RPAREN
    { mk_expr (PE_tuple ($2::$4))  $sloc}
| MERGE IDENT list(merge_branche)
    { let ident = mk_expr (PE_ident $2) $loc($2) in
      mk_expr (PE_merge (ident, $3)) $sloc }
| e1=expr WHEN c=constr LPAREN e2=expr RPAREN
    { mk_expr (PE_when (e1, c, e2)) $sloc }
| e1=expr WHEN e2=expr
    { mk_expr (PE_when (e1, "True", e2)) $sloc }
| e1=expr WHENOT e2=expr
    { mk_expr (PE_when (e1, "False", e2)) $sloc }
;

merge_branche:
 LPAREN const ARROW expr RPAREN { (mk_expr (PE_const $2) $loc($2), $4) }
;

constr:
| CONSTR { $1 }
| TCONSTR { fst $1 }
;

const:
| CONST_BOOL
    { Cbool $1 }
| CONST_INT
    { Cint $1 }
| CONST_REAL
    { Creal $1 }
| TCONSTR
    { let c, t = $1 in
      Cadt (t, Some c) }
| CONSTR
    { let t = Hashtbl.find h_constr_ty $1 in
      Cadt (t, Some $1) }
;

ident_comma_list:
| IDENT COMMA ident_comma_list
    { $1 :: $3 }
| IDENT { [$1] }
;

expr_comma_list_empty:
    { [] }
| expr_comma_list { $1 }
;

expr_comma_list:
| expr COMMA expr_comma_list
    { $1 :: $3 }
| expr { [$1] }
;

typ:
| BOOL   { Tbool }
| INT    { Tint }
| REAL   { Treal }
| IDENT  { Tadt $1 }
;

semi_opt:
    { () }
| SEMICOL
    { () }
;
