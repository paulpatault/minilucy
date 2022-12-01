open GoblintCil.Cil
open GoblintCil
open Format

let rec pp_type fmt = function
  | TInt (ity, _) ->
    begin match ity with
      | IInt -> fprintf fmt "int"
      | IChar -> fprintf fmt "char"
      | _ -> assert false
    end
  | TFloat (_, _) -> fprintf fmt  "float"
  | TPtr (ty, _) -> fprintf fmt "%a*" pp_type ty
  | TVoid _ -> fprintf fmt "void"
  | TComp ({cname; _}, _) -> fprintf fmt "struct %s" cname
  | TEnum (enuminfo, _) -> fprintf fmt "enum %s" enuminfo.ename
  | _ as ty ->
    let d = defaultCilPrinter#pType None () ty in
    print_endline @@ Pretty.sprint ~width:80 d;
    assert false

let pp_fieldinfo fmt fieldinfo =
  fprintf fmt "%a %s;"
    pp_type fieldinfo.ftype
    fieldinfo.fname

let pp_comp_info fmt compinfo =
  fprintf fmt "struct %s {@\n@;<2 2>@[<v>%a@]@;};@\n"
    compinfo.cname
    (pp_print_list ~pp_sep:pp_print_cut pp_fieldinfo) compinfo.cfields

let pp_comma fmt () =
  fprintf fmt ", "

let pp_eol_semi fmt () =
  fprintf fmt ";@\n"

let pp_2eol_semi fmt () =
  fprintf fmt ";@;@;"

let pp_access fmt is_mem =
  fprintf fmt (if is_mem then "->" else ".")

let pp_const fmt = function
  | CInt (i, _, _) -> fprintf fmt "%i" (cilint_to_int i)
  | CReal (f, _, _) -> fprintf fmt "%f" f
  | CEnum (Const (CInt (i,_,_)), _, enuminfo) ->
      let i = Z.to_int i in
      let e, _, _ = List.nth enuminfo.eitems i in
      fprintf fmt "%s" e
  | CStr (s, _) ->
      fprintf fmt "%S" s
  | _ -> assert false

let pp_unop fmt = function
  | Neg -> fprintf fmt "-"
  | BNot -> fprintf fmt "~"
  | LNot -> fprintf fmt "!"

let pp_binop fmt = function
  | PlusA | PlusPI | IndexPI -> fprintf fmt "+"
  | MinusA | MinusPI | MinusPP -> fprintf fmt "-"
  | Mult -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Mod -> fprintf fmt "%%"
  | Shiftlt -> fprintf fmt "<<"
  | Shiftrt -> fprintf fmt ">>"
  | Lt -> fprintf fmt "<"
  | Gt -> fprintf fmt ">"
  | Le -> fprintf fmt "<="
  | Ge -> fprintf fmt ">="
  | Eq -> fprintf fmt "=="
  | Ne -> fprintf fmt "!="
  | BAnd -> fprintf fmt "&"
  | BXor -> fprintf fmt "^"
  | BOr -> fprintf fmt "|"
  | LAnd -> fprintf fmt "&&"
  | LOr -> fprintf fmt "||"

let rec pp_offset is_mem fmt = function
  | NoOffset -> fprintf fmt ""
  | Field (field, offset) ->
    fprintf fmt "%a%s%a"
      pp_access is_mem
      field.fname
      (pp_offset is_mem) offset
  | Index (idx, offset) ->
    fprintf fmt "[%a]%a"
      pp_exp idx
      (pp_offset is_mem) offset

and pp_exp fmt = function
  | Const c -> pp_const fmt c
  | Lval lval -> pp_lval fmt lval
  | UnOp (op, e, _) -> fprintf fmt "%a(%a)" pp_unop op pp_exp e
  | BinOp (op, e1, e2, _) -> fprintf fmt "(%a %a %a)" pp_exp e1 pp_binop op pp_exp e2
  | Question (e1, e2, e3, _) -> fprintf fmt "%a ? %a : %a" pp_exp e1 pp_exp e2 pp_exp e3
  | CastE (ty, e) -> fprintf fmt "(%a)%a" pp_type ty pp_exp e
  | AddrOf lval -> fprintf fmt "&(%a)" pp_lval lval
  | _ -> assert false

and pp_var fmt var =
  fprintf fmt "%s" var.vname

and pp_lval fmt = function
  | Var var, offset -> fprintf fmt "%a%a" pp_var var (pp_offset false) offset
  | Mem e, NoOffset -> fprintf fmt "*(mem)"
  | Mem e, offset -> fprintf fmt "mem%a" (pp_offset true) offset

and pp_call fmt ret call args =
  match ret with
  | Some lval ->
    fprintf fmt "%a = %a(%a)"
      pp_lval lval
      pp_exp call
      (pp_print_list ~pp_sep:pp_comma pp_exp) args
  | None ->
    fprintf fmt "%a(%a)"
      pp_exp call
      (pp_print_list ~pp_sep:pp_comma pp_exp) args

let pp_arg fmt (n, ty, _) =
  match ty with
  | TArray (ty, size, _) ->
    fprintf fmt "%a %s[%a]"
      pp_type ty
      n
      (fun fmt -> function
         | Some e -> pp_exp fmt e
         | None -> ()) size
  | _ ->
    fprintf fmt "%a %s"
      pp_type ty
      n

let pp_args fmt = function
  | Some args -> fprintf fmt "%a" (pp_print_list ~pp_sep:pp_comma pp_arg) args
  | None -> fprintf fmt ""


let pp_local fmt local =
  let ty, name = local.vtype, local.vname in
  let init =
    match local.vinit.init with
    | Some (SingleInit exp) -> asprintf " = %a" pp_exp exp
    | _ -> ""
  in
  match ty with
  | TArray (t, size, _) ->
    fprintf fmt "%a %s[%a];"
      pp_type ty
      name
      (fun fmt -> function
         | Some s -> pp_exp fmt s
         | None -> ()) size
  | _ ->
    fprintf fmt "%a %s%s;"
      pp_type ty
      name
      init

let pp_locals fmt locals =
  fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_cut pp_local) locals


let pp_instr fmt = function
  | Set (lval, e, _, _) -> fprintf fmt "%a = %a" pp_lval lval pp_exp e
  | Call (ret, call, args, _, _) -> pp_call fmt ret call args
  | _ -> assert false

let rec pp_stmt_case fmt stmts =
  let cases =
    List.fold_left (fun l ({labels; _} as stmt) ->
        match labels with
        | [Case _] ->
          [stmt]::l
        | _ ->
          begin
            match l with
            | [] -> assert false
            | [hd] -> [stmt::hd]
            | hd::tl -> (stmt::hd)::tl
          end)
      []
      stmts
    |> List.map List.rev
    |> List.rev
  in
  fprintf fmt "%a"
    begin
      pp_print_list
        ~pp_sep:pp_print_cut
        (fun fmt stmts ->
           let hd = List.hd stmts in
           match hd.labels with
           | [Case (e, _, _)] ->
             fprintf fmt "case %a: {@;<2 2>@[<v>%a;@]@;}"
               pp_exp e
               (pp_print_list ~pp_sep:pp_eol_semi pp_stmt) stmts
           | _ -> assert false)
    end
    cases

and pp_stmt fmt stmt =
  match stmt.skind with
  | Instr il -> fprintf fmt "%a" (pp_print_list ~pp_sep:pp_eol_semi pp_instr) il
  | Return (e, _) ->
    begin
      match e with
      | Some e -> fprintf fmt "return %a" pp_exp e
      | None -> fprintf fmt "return"
    end
  | Switch (e, _, stmts, _, _) ->
    fprintf fmt "switch (%a) {@;<2 2>@[<v>%a@]@;}"
      pp_exp e
      pp_stmt_case stmts
  | Break _ -> fprintf fmt "break"
  | Loop (b, _, _, _, _) ->
    fprintf fmt "while (1) {@;<2 2>@[<v>%a@]@;}"
      (pp_block false) b
  | If (cond, b1, b2, _, _) when b2.bstmts = [] ->
    fprintf fmt "if (%a) {@;<2 2>@[<v>%a@]@;}"
      pp_exp cond
      (pp_block false) b1
  | If (cond, b1, b2, _, _) ->
    fprintf fmt "if (%a) {@;<2 2>@[<v>%a@]@;} else {@;<2 2>@[<v>%a@]@;}"
      pp_exp cond
      (pp_block false) b1
      (pp_block false) b2
  | _ -> assert false

and pp_block brackets fmt block =
  begin
    if brackets then
      fprintf fmt "{@;<2 2>@[<v>%a;@]@;}@\n"
    else
      fprintf fmt "%a;"
  end
    (pp_print_list ~pp_sep:pp_2eol_semi pp_stmt) block.bstmts

let pp_fundec fmt fundec =
  let ret_ty, args = match fundec.svar.vtype with
    | TFun (typ, args, _, _) -> typ, args
    | _ -> assert false
  in
  fprintf fmt "%a %s (%a) {@;<2 2>@[<v>%a%a%a@]@\n}@\n"
    pp_type ret_ty
    fundec.svar.vname
    pp_args args
    pp_locals fundec.slocals
    (fun fmt () -> if fundec.slocals <> [] then fprintf fmt "@;@;") ()
    (pp_block false) fundec.sbody


let pp_comma_cut fmt () = fprintf fmt ",@;"

let pp_typeinfo fmt typeinfo =
  match typeinfo.ttype with
  | TEnum (enuminfo, _) ->
      fprintf fmt "enum %s {@;<2 2>@[<v>%a@]@\n};@\n"
        typeinfo.tname
        (pp_print_list ~pp_sep:pp_comma_cut (fun fmt (e, _, _) -> pp_print_string fmt e)) enuminfo.eitems
  | _ -> assert false

let pp_global fmt = function
  | GCompTag (compinfo, _) -> pp_comp_info fmt compinfo
  | GFun (fundec, _) -> pp_fundec fmt fundec
  | GType (typeinfo, _) -> pp_typeinfo fmt typeinfo
  | GText s -> fprintf fmt "%s\n" s
  | _ -> assert false

let pp fmt file =
  fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_newline pp_global) file.globals
