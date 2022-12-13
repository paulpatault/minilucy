{

  open Lexing
  open Parser
  open Asttypes
  open Parse_ast

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [
        "and", AND;
        "bool", BOOL;
        "const", CONST;
        "div", DIV;
        "else", ELSE;
        "end", END;
        "false", CONST_BOOL(false);
        "if", IF;
        "int", INT;
        "let", LET;
        "mod", MOD;
        "node", NODE;
        "not", NOT;
        "or", OR;
        "pre", PRE;
        "real", REAL;
        "returns", RETURNS;
        "tel", TEL;
        "then", THEN;
        "true", CONST_BOOL(true);
        "var", VAR;

        "merge", MERGE;
        "every", EVERY;
        "reset", RESET;
        "when", WHEN;
        "whenot", WHENOT;
        "print", PRINT;

        "automaton", AUTOMATON;
        "fby", FBY;
        "unless", UNLESS;
        "until", UNTIL;
        "type", TYPE;
        "init", INIT;
        "continue", CONTINUE;
        "done", DONE;
        "local", LOCAL;
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let lalpha = ['a'-'z']
let ualpha = [ 'A'-'Z']
let alpha = lalpha | ualpha
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent?
          | digit* '.'digit+ exponent?
      | digit+ exponent

let ident = lalpha (alpha | '_' | digit)*
let constr = ualpha (alpha | '_' | digit)*
let tconstr = ualpha (alpha | '_' | digit)* "'" ident

rule token = parse
  | '\n'
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "--" [^ '\n']* ['\n']
      { newline lexbuf; token lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | "(*"
      { comment2 lexbuf; token lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | constr
      { CONSTR (lexeme lexbuf) }
  | tconstr
      { let a = String.split_on_char '\'' (lexeme lexbuf) in
        match a with
        | [constr;ty] -> TCONSTR (constr, ty)
        | _ -> assert false }
  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | float
      { CONST_REAL (float_of_string (lexeme lexbuf)) }
  | '"' [^ '\n']* '"'
      { STR (lexeme lexbuf) }
  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }
  | ">"
      { COMP Op_gt }
  | ">="
      { COMP Op_ge }
  | "<"
      { COMP Op_lt }
  | "<="
      { COMP Op_le }
  | "<>"
      { NEQ }
  | "=>"
      { IMPL }
  | "->"
      { ARROW }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | ":"
      { COLON }
  | ";"
      { SEMICOL }
  | "="
      { EQUAL }
  | "|"
      { BAR }
  | ","
      { COMMA }
  | _
      { raise (Lexical_error (lexeme lexbuf)) }
  | eof
      { EOF }

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }

and comment2 = parse
  | "*)" { () }
  | '\n' { newline lexbuf; comment2 lexbuf }
  | _    { comment2 lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }
