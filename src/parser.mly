%{

  open Asttypes
  open Parse_ast

  let mk_expr e l = { pexpr_desc = e; pexpr_loc = l }
  let mk_patt p l = { ppatt_desc = p; ppatt_loc = l }

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
%token RESET
%token WHEN WHENOT

%token AUTOMATON
%token UNLESS
%token UNTIL
%token CONTINUE
%token DONE
%token TYPE
%token INIT
%token LOCAL

%nonassoc THEN
%nonassoc ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%left COMP EQUAL NEQ                          /* < <= > >= <> = <> */
%left PLUS MINUS                              /* + -  */
%left STAR SLASH DIV MOD                      /* * /  mod */
%nonassoc uminus                              /* - */
%nonassoc NOT PRE                             /* not pre */
%left DOT

/* Point d'entrée */

%start file
%type <Parse_ast.p_file> file

%%

file: type_decs node_decs EOF
  { { p_types = $1 ; p_nodes = $2; } }
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
| TYPE tname=IDENT EQUAL BAR? l=separated_list(BAR, IDENT)
  { {pt_name = tname; pt_constr = l} }
;

node:
| NODE n=IDENT LPAREN p=in_params RPAREN
  RETURNS LPAREN op=out_params RPAREN SEMICOL
  lp=local_params
  lip=list(local_params_init)
  LET eql=eq_list TEL semi_opt
    { { pn_name = n;
    pn_input = p;
    pn_output = op;
    pn_local = lp;
    pn_init_local = lip;
    pn_equs = eql;
    pn_loc = $sloc; } }
;

local_params_init:
| LOCAL id=IDENT COLON t=IDENT INIT vo=IDENT
    { (id, Tadt t, vo) }
;

%public %inline loc(X):
 | x = X { x &@ $sloc }
;

in_params:
| /* empty */
    { [] }
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
| pattern EQUAL expr SEMICOL
    { PE_eq { peq_patt = $1; peq_expr = $3; } }
| AUTOMATON pautom=list(case_autom) END semi_opt
    { PE_automaton ({pautom ; pautom_loc = $sloc}) }
;

case_autom:
| pn_case=case UNTIL pn_cond=expr CONTINUE pn_out=IDENT
  { {pn_case; pn_cond; pn_out; pn_weak=true}  }
| pn_case=case UNLESS pn_cond=expr THEN pn_out=IDENT
  { {pn_case; pn_cond; pn_out; pn_weak=false}  }
/* | pn_case=case
  { {pn_case; pn_cond = mk_expr (PE_const (Cbool false)) $sloc; pn_out = "todo"; pn_weak=false}  } */
;

case:
| BAR pn_constr=IDENT ARROW pn_equation=eq
  { {pn_constr; pn_equation; pn_loc = $sloc} }
;

pattern:
| IDENT
    { mk_patt (PP_ident $1) $sloc}
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
| MINUS expr /* %prec uminus */
    { mk_expr (PE_op (Op_sub, [$2]))  $sloc}
| NOT expr
    { mk_expr (PE_op (Op_not, [$2]))  $sloc}
| PRE expr
    { mk_expr (PE_pre ($2))  $sloc}
| LPAREN expr COMMA expr_comma_list RPAREN
    { mk_expr (PE_tuple ($2::$4))  $sloc}
| MERGE IDENT merge_branche(CONST_BOOL) merge_branche(CONST_BOOL)
    { let ident = mk_expr (PE_ident $2) $loc($2) in
    mk_expr (PE_merge (ident, $3, $4)) $sloc }

| MERGE IDENT list(merge_branche(IDENT))
    { let ident = mk_expr (PE_ident $2) $loc($2) in
    mk_expr (PE_merge_adt (ident, $3)) $sloc }
/* | expr WHEN expr       { mk_expr (PE_when ($2, $3)) } */
/* | expr WHENOT expr     { mk_expr (PE_whenot ($2, $3)) } */
;

merge_branche(X):
 LPAREN X ARROW expr RPAREN { ($2, $4) }
;

const:
| CONST_BOOL
    { Cbool $1 }
| CONST_INT
    { Cint $1 }
| CONST_REAL
    { Creal $1 }
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
;

semi_opt:
    { () }
| SEMICOL
    { () }
;
