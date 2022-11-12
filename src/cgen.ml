open GoblintCil.Cil
open Imp_ast
open Cil_utils
open Ident

module M = Map.Make(String)

(* Returns a local varinfo, and struct list *)
(* let compile_output_type output =
   if List.length output > 1 then
    let struct_ = mk_struct output in
    TComp (struct_, [])
   else
    let var = List.hd output in
    snd var |> translate_type

   let compile_output_step fundec typ output =
   if List.length output > 1 then
    makeLocalVar fundec "res" typ
   else
    let var = List.hd output in
    let name = let ident = fst var in ident.name in
    makeLocalVar fundec name typ *)

let eq_ty_cnt = ref 0
let gen_eq_ty_name () =
  incr eq_ty_cnt; "eq_ty_"^(string_of_int !eq_ty_cnt)

let tuple_ty_cnt = ref 0
let gen_tuple_ty_name () =
  incr tuple_ty_cnt; "tuple_ty_"^(string_of_int !tuple_ty_cnt)

let tuple_field_cnt = ref 0
let gen_tuple_field_name () =
  incr tuple_field_cnt; "tuple_field_"^(string_of_int !tuple_field_cnt)

let call_cnt = ref 0
let gen_call_name () =
  incr call_cnt; "call_"^(string_of_int !call_cnt)

let switch_cnt = ref 0
let gen_switch_name () =
  incr switch_cnt; "switch_"^(string_of_int !switch_cnt)

let pp_global fmt global =
  let open GoblintCil in
  let doc = Pretty.dprintf "%a" Cil.d_global global in
  Format.fprintf fmt "%s" (Pretty.sprint ~width:80 doc)

let pp fmt file =
  Format.(fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_newline pp_global)) file.globals

let write_out c_ast out =
  Format.fprintf out "%a" pp c_ast

let mk_mem_node_fields globals mem_comp node_mem =
  let fields =
    List.map (fun (field_id, node_id) ->
        let mem_name = (node_id.name^"_mem") in
        let field_name = clean_name field_id.name in
        let node_mem_comp = find_gcomp mem_name globals in
        let mem_typ = TComp (node_mem_comp, []) in
        {fcomp = mem_comp; fname = field_name; ftype = mem_typ; fbitfield = None; fattr = []; floc = locUnknown;})
      node_mem
  in
  mem_comp.cfields <- fields@(mem_comp.cfields);
  mem_comp

let compile_mem_comp globals node =
  let mem = node.in_mem in
  let mem_fby_only = mk_struct (node.in_name.name^"_mem") mem.fby_mem in
  let mem_comp = mk_mem_node_fields globals mem_fby_only mem.node_mem in
  mem_comp

let compile_return_type file node =
  let ret_vars = node.in_output_step in
  match ret_vars with
  | [] -> file, TVoid ([])
  | [_, ty, _] -> file, translate_type ty
  | l ->
    let ret_name = (node.in_name.name^"_ret") in
    let ret_comp = mk_struct ret_name l in
    file.globals <- (GCompTag (ret_comp, locUnknown))::file.globals;
    file, TComp (ret_comp, [])

let compile_func_type file node return_type mem_comp =
  let params = List.map (fun (id, ty, _) ->
      let name = clean_name id.name in
      let typ = translate_type ty in
      (name, typ, []))
      node.in_input_step
  in
  let params =
    if mem_comp.cfields <> [] then
      ("mem", TPtr (TComp (mem_comp, []), []), [])::params
    else
      params
  in
  match params with
  | [] -> file, TFun (return_type, None, false, [])
  | l -> file, TFun (return_type, Some l, false, [])

let compile_locals file fundec =
  List.fold_left (fun (fundec, locals) ({name; _}, ty, _) ->
      let loc = makeLocalVar fundec (clean_name name) (translate_type ty) in
      fundec, loc::locals)
    (fundec, [])

let compile_fundec file node mem_comp =
  let file, return_type = compile_return_type file node in
  let file, func_type = compile_func_type file node return_type mem_comp in
  let func_var = makeGlobalVar (node.in_name.name) func_type in
  let fundec = mk_fundec func_var in
  let fundec, locals = compile_locals file fundec node.in_local in
  let fundec, locals = compile_locals file fundec node.in_output_step in (* INFO: Maybe do this another way *)
  begin
    match return_type with
    | TComp (compinfo, _) ->
      ignore @@ makeLocalVar fundec "ret_" return_type
    | _ -> () (* already added with output_step above *)
  end;
  file, fundec

let compile_init file node mem_comp =
  let name = node.in_name.name in
  let fun_ty = TFun (TVoid [], Some ["mem", TPtr (TComp (mem_comp, []), []), []], false, []) in
  let fun_var = makeGlobalVar (name^"_init") fun_ty in
  let fundec = mk_fundec fun_var in
  let mem_var = find_formal fundec "mem" in
  let init_fby =
    List.map (fun (id, atom) ->
        match atom with
        | Ident _ -> failwith "Not implemented yet"
        | Const c ->
          let field = find_field_list (id.name) mem_comp.cfields in
          let c = translate_const c in
          let field_lval = (Mem (Lval (Var mem_var, NoOffset)), Field (field, NoOffset)) in
          let set_instr = Set(field_lval, Const c, locUnknown, locUnknown) in
          mkStmtOneInstr set_instr)
      node.in_init.fby_init
  in
  let init_node =
    List.map (fun (f_id, init_id) ->
        let init_fun = find_fun (init_id.name^"_init") file.globals in
        let field = find_field_list (f_id.name) mem_comp.cfields in
        let field_mem = (Mem (Lval (Var mem_var, NoOffset)), Field (field, NoOffset)) in
        let field_addr = AddrOf field_mem in
        let call_instr = Call (None, Lval (Var init_fun.svar, NoOffset), [field_addr], locUnknown, locUnknown) in
        mkStmtOneInstr call_instr)
      node.in_init.node_init
  in
  let fun_block = List.fold_left (fun block stmt -> append_stmt stmt block) fundec.sbody init_fby in
  let fun_block = List.fold_left (fun block stmt -> append_stmt stmt block) fun_block init_node in
  fundec.sbody <- fun_block;
  file.globals <- (GFun (fundec, locUnknown))::file.globals;
  file, fundec

let compile_eq_type file patt =
  match patt with
  | [_, ty, _] -> file, translate_type ty
  | l ->
    let name = gen_eq_ty_name () in
    let compinfo = mk_struct name l in
    file.globals <- (GCompTag (compinfo, locUnknown))::file.globals;
    file, TComp (compinfo, [])

let rec compile_expr file node fundec expr =
  match expr.iexpr_desc with
  | IE_const c ->
    file, GoblintCil.Const (translate_const c),
    begin
      match c with
      | Cbool _ -> bool_t
      | Cint _ -> int_t
      | Creal _ -> real_t
    end
  | IE_ident id ->
    let lval, ty =
      try 
        let var = find_formal fundec (id.name) in
        Lval (Var var, NoOffset), var.vtype
      with Not_found ->
        begin try
            let var = find_local fundec (id.name) in
            Lval (Var var, NoOffset), var.vtype
          with Not_found ->
            let ret_v = find_local fundec "ret_" in
            let fieldinfo = find_field_globals (node.in_name.name^"_ret") id.name file.globals in
            Lval (Var ret_v, Field (fieldinfo, NoOffset)), fieldinfo.ftype
        end
    in
    file, lval , ty
  | IE_mem id ->
    let mem_var =
      try find_formal fundec "mem"
      with Not_found -> failwith (Format.sprintf "Mem not found for node: %s" node.in_name.name) in
    let fieldinfo =
      try find_field_globals (node.in_name.name^"_mem") (id.name) file.globals 
      with Not_found ->
        pp Format.std_formatter file;
        flush_all ();
        failwith (Format.sprintf "Mem or field not found: %s %s" (node.in_name.name^"_mem") (id.name))
    in
    file, Lval (Mem (Lval (Var mem_var, NoOffset)), Field (fieldinfo, NoOffset)), fieldinfo.ftype
  | IE_op (op, el) ->
    begin
      match op, el with
      | Op_eq, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Eq, e1', e2', ty), bool_t
      | Op_neq, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Ne, e1', e2', ty), bool_t
      | Op_lt, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Lt, e1', e2', ty), bool_t
      | Op_le, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Le, e1', e2', ty), bool_t
      | Op_gt, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Gt, e1', e2', ty), bool_t
      | Op_ge, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Ge, e1', e2', ty), bool_t
      | Op_add, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (PlusA, e1', e2', ty), int_t
      | Op_sub, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (MinusA, e1', e2', ty), int_t
      | Op_mul, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Mult, e1', e2', ty), int_t
      | Op_div, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Div, e1', e2', ty), int_t
      | Op_mod, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Mod, e1', e2', ty), int_t
      | Op_add_f, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (PlusA, e1', e2', ty), real_t
      | Op_sub_f, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (MinusA, e1', e2', ty), real_t
      | Op_mul_f, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Mult, e1', e2', ty), real_t
      | Op_div_f, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (Div, e1', e2', ty), real_t
      | Op_not, [e1] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, UnOp (LNot, e1', ty), bool_t
      | Op_sub, [e1] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, UnOp (Neg, e1', ty), int_t
      | Op_sub_f, [e1] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, UnOp (Neg, e1', ty), real_t
      | Op_and, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (LAnd, e1', e2', ty), bool_t
      | Op_or, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (LOr, e1', e2', ty), bool_t
      | Op_impl, [e1; e2] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let ty = translate_type @@ List.hd e1.iexpr_type in
        file, BinOp (LOr, UnOp (LNot, e1', ty), e2', ty), bool_t
      | Op_if, [e1; e2; e3] ->
        let file, e1', _ = compile_expr file node fundec e1 in
        let file, e2', _ = compile_expr file node fundec e2 in
        let file, e3', _ = compile_expr file node fundec e3 in
        let ty = translate_type @@ List.hd e2.iexpr_type in
        file, Question (e1', e2', e3', ty), ty
      | _ -> assert false;
    end
  | IE_tuple (el) ->
    let tuple_ty =
      List.map (fun {iexpr_type = ty; _} -> ty) el
      |> List.flatten
      |> List.map (fun ty ->  Ident.make (gen_tuple_field_name ()) Stream, ty, ())
    in
    let tuple_comp = mk_struct (gen_tuple_ty_name ()) tuple_ty in
    file.globals <- (GCompTag (tuple_comp, locUnknown))::file.globals;
    let tuple_var = makeLocalVar fundec (tuple_comp.cname^"__") (TComp (tuple_comp, [])) in
    let file, el' =
      List.fold_left_map (fun file e ->
          let file, e', _ = compile_expr file node fundec e in
          file, e')
        file
        el
    in
    List.iter2 (fun fieldinfo e ->
        let tuple_lval = Var tuple_var, Field (fieldinfo, NoOffset) in
        let set_instr = Set (tuple_lval, e, locUnknown, locUnknown) in
        let stmt = mkStmtOneInstr set_instr in
        fundec.sbody <- append_stmt stmt fundec.sbody;)
      tuple_comp.cfields
      el';
    file, Lval (Var tuple_var, NoOffset), TComp (tuple_comp, [])
  | IE_app (n_name, mem_field, args) ->
    let file, args' =
      List.fold_left_map (fun file e ->
          let file, arg, _ = compile_expr file node fundec e in
          file, arg)
        file
        args
    in
    let args' = try
        let field_info = find_field_globals (node.in_name.name^"_mem") (mem_field.name) file.globals in
        let mem_var = find_formal fundec "mem" in
        let lval = Mem (Lval (Var mem_var, NoOffset)), Field (field_info, NoOffset) in
        AddrOf(lval)::args'
      with Not_found ->
        args'
    in
    let callee = find_fun (n_name.name) file.globals in
    let callee_lval = Lval (Var callee.svar, NoOffset) in
    let ret_ty =
      match callee.svar.vtype with
      | TFun (ret_ty, _, _, _) -> ret_ty
      | _ -> assert false
    in
    let res_var = makeLocalVar fundec (gen_call_name ()) ret_ty in
    let res_lval = Var res_var, NoOffset in
    let call_instr = Call (Some res_lval, callee_lval, args', locUnknown, locUnknown) in
    let call_stmt = mkStmtOneInstr call_instr in
    fundec.sbody <- append_stmt call_stmt fundec.sbody;
    file, Lval res_lval, ret_ty
  | IE_case (id, cases) ->
    let res_ty =
      let hd, _ = List.hd cases in
      let ty = hd.iexpr_type in
      match ty with
      | [ty] -> translate_type ty
      | tl ->
        let field_ty = List.map (fun ty -> Ident.make (gen_tuple_field_name ()) Stream, ty, ()) tl in
        let compinfo = mk_struct (gen_tuple_ty_name ()) field_ty in
        file.globals <- (GCompTag (compinfo, locUnknown))::file.globals;
        TComp (compinfo, [])
    in
    let switch_res = makeLocalVar fundec (gen_switch_name ()) res_ty in
    let switch_lval = Var switch_res, NoOffset in
    let file, switch_stmts =
      List.fold_left_map (fun file (case, e) ->
          let file, case', _ = compile_expr file node fundec case in
          let file, e', _ = compile_expr file node fundec e in
          let set_instr = Set (switch_lval, e', locUnknown, locUnknown) in
          let stmt = mkStmtOneInstr set_instr in
          stmt.labels <- (Case (case', locUnknown, locUnknown))::stmt.labels;
          file, stmt)
        file
        cases
    in
    let switch_block = mkBlock switch_stmts in
    let lval =
      try
        let var = find_formal fundec (id.name) in
        Lval (Var var, NoOffset)
      with Not_found ->
        begin try
            let var = find_local fundec (id.name) in
            Lval (Var var, NoOffset)
          with Not_found ->
            let ret_v = find_local fundec "ret_" in
            let fieldinfo = find_field_globals (node.in_name.name^"_ret") id.name file.globals in
            Lval (Var ret_v, Field (fieldinfo, NoOffset))
        end
    in
    let switch_stmt = mkStmt (Switch (lval, switch_block, switch_stmts, locUnknown, locUnknown)) in
    fundec.sbody <- append_stmt switch_stmt fundec.sbody;
    file, Lval (Var switch_res, NoOffset), res_ty
  | IE_prim (n, el) ->
    match n.name, el with
    | "int_of_real", [e] ->
      let file, e', _ = compile_expr file node fundec e in
      file, CastE (int_t, e'), int_t
    | "real_of_int", [e] ->
      let file, e', _ = compile_expr file node fundec e in
      file, CastE (real_t, e'), real_t
    | _ -> assert false


let compile_equation file node fundec ({ieq_patt = patt; ieq_expr = expr}) =
  let file, expr, expr_ty = compile_expr file node fundec expr in
  begin
    match patt with
    | [] -> assert false
    | [n, ty, _] ->
      let lval = try
          Var (find_local fundec (n.name)), NoOffset
        with Not_found ->
          let ret_v = find_local fundec "ret_" in
          let fieldinfo = find_field_globals (node.in_name.name^"_ret") n.name file.globals in
          Var ret_v, Field (fieldinfo, NoOffset)
      in
      let set_instr = Set (lval, expr, locUnknown, locUnknown) in
      fundec.sbody <- append_stmt (mkStmtOneInstr set_instr) fundec.sbody;
    | pl ->
      let expr_fields =
        match expr_ty with
        | TComp ({cfields; _}, _) ->
          cfields
        | _ -> assert false
      in
      List.iter2 (fun (pname, _, _) expr_field ->
          let lval = try
              Var (find_local fundec (pname.name)), NoOffset
            with Not_found ->
              let ret_v = find_local fundec "ret_" in
              let fieldinfo = find_field_globals (node.in_name.name^"_ret") pname.name file.globals in
              Var ret_v, Field (fieldinfo, NoOffset)
          in
          let e = match expr with
            | Lval (Var v, _) ->
              Lval (Var v, Field (expr_field, NoOffset))
            | _ -> assert false
          in
          let set_instr = Set (lval, e, locUnknown, locUnknown) in
          fundec.sbody <- append_stmt (mkStmtOneInstr set_instr) fundec.sbody;)
        pl
        expr_fields;
  end;
  file


let compile_compute file node fundec =
  let block = fundec.sbody in
  let file =
    List.fold_left (fun file eq ->
        compile_equation file node fundec eq)
      file
      node.in_compute
  in
  fundec.sbody <- block;
  file, fundec


let compile_node file node =
  let mem_comp = compile_mem_comp file.globals node in
  let file =
    if mem_comp.cfields <> [] then
      file.globals <- (GCompTag (mem_comp, locUnknown))::file.globals;
    let file, init_fundec = compile_init file node mem_comp in
    file
  in
  let file, fundec = compile_fundec file node mem_comp in
  let file, fundec = compile_compute file node fundec in
  file.globals <- (GFun (fundec, locUnknown))::file.globals;
  file

let compile ast file_name =
  let file = {
    fileName = file_name;
    globals = [];
    globinit = None;
    globinitcalled = false;
  } in
  let file = List.fold_left compile_node file ast in
  file.globals <- List.rev file.globals;
  file
