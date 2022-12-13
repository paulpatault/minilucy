open GoblintCil.Cil
open Asttypes
open Typed_ast
open Ident
open Utils

let comp_num = ref 0

let get_next_comp_name () =
  let name = "comp_"^(string_of_int !comp_num) in
  incr comp_num;
  name

let mk_int_exp i =
    let cilint = mkCilint IInt (Int64.of_int i) in
    Const (CInt (cilint, IInt, None))

let rec translate_const types = function
  | Cbool b ->
      let cilint = mkCilint IInt (if b then 1L else 0L) in
      CInt (cilint, IInt, None)
  | Cint i ->
      let cilint = mkCilint IInt (Int64.of_int i) in
      CInt (cilint, IInt, None)
  | Creal f ->
      CReal (f, FFloat, None)

  | Cadt (s, Some e) ->
      let enuminfo = match mk_enum types s with
        | TEnum (enuminfo, attribute) -> enuminfo
        | _ -> assert false in
      let e_uppercase = Bytes.of_string e |> Bytes.uppercase_ascii |> Bytes.to_string in
      let _, exp, _ = List.find (fun (name, exp, loc) ->
        name = e_uppercase) enuminfo.eitems in
      CEnum (exp, enuminfo.ename, enuminfo)

  | Cadt (s, None) -> (* get_default s *)
      let ts = List.find (fun {name; constr} -> name = s) types in
      translate_const types (Cadt (s, Some (List.hd ts.constr)))

and mk_enum =
  let h = Hashtbl.create 20 in
  fun types ename ->
    if Hashtbl.mem h ename then Hashtbl.find h ename
    else
      let enum =
        let ts = List.find (fun {name; constr} -> name = ename) types in
        let eitems = List.mapi (fun i c ->
          let ename = Bytes.of_string c |> Bytes.uppercase_ascii |> Bytes.to_string in
          let exp = Const (translate_const types (Cint i)) in
          ename, exp, locUnknown) ts.constr in
        let enuminfo = { ename; eitems; ekind = IInt; eattr = []; ereferenced = false; } in
        let attributes = [] in
        TEnum (enuminfo, attributes) in
      Hashtbl.add h ename enum;
      enum

and translate_type types = function
  | Tbool -> TInt (IInt, [])
  | Tint -> TInt (IInt, [])
  | Treal -> TFloat (FFloat, [])
  | Tadt s -> mk_enum types s

let rec base_ty_to_format_string = function
  | Tbool
  | Tint  -> "\"%d\""
  | Treal -> "\"%f\""
  | _ -> failwith "todo"
(* failwith ou "%d" par défaut ? *)

let rec typ_to_format_string = function
  | TInt   _ -> "\"%d\""
  | TFloat _ -> "\"%f\""
  (* | TPtr (t, _) -> typ_to_format_string t *)
  | t ->
      (* failwith ou "%d" par défaut ? *)
      failwith (Format.asprintf "not implemented : typ_to_format_string [%a]" C_printer.pp_type t)

let true_const = integer 1
let false_const = integer 0
let bool_t = TInt (IInt, [])
let int_t = TInt (IInt, [])
let real_t = TFloat (FFloat, [])
let adt_t types s = translate_type types (Tadt s)

let clean_name name =
  let str = Str.regexp {|'|} in
  Str.global_replace str "__" name

let mk_struct types name var_l =
  let comp = mkCompInfo true name (fun _ -> []) [] in
  let fields = List.map (fun ({name; _}, typ, _) ->
      {fcomp = comp; fname = clean_name name; ftype = translate_type types typ; fbitfield = None; fattr = []; floc = locUnknown})
      var_l
  in
  comp.cfields <- fields;
  comp

let rec find_gcomp name = function
  | GCompTag (compinfo, _)::tl ->
    if compinfo.cname = name then
      compinfo
    else
      find_gcomp name tl
  | _::tl -> find_gcomp name tl
  | [] -> raise Not_found

let rec find_field_list name = function
  | ({fname; _} as field)::tl when (clean_name name) = fname ->
    field
  | _::tl -> find_field_list name tl
  | [] -> raise Not_found

let find_field_globals comp field globals =
  let compinfo = find_gcomp comp globals in
  find_field_list field compinfo.cfields

let mk_fundec varinfo =
  let args = match varinfo.vtype with
    | TFun (_, Some args, _, _) -> args
    | TFun (_, None, _, _) -> []
    | _ -> raise (Invalid_argument "Not a function type")
  in
  let fundec = {
    svar = varinfo;
    sformals = [];
    slocals = [];
    smaxid = 0;
    sbody = mkBlock [];
    smaxstmtid = None;
    sallstmts = []; }
  in
  let fundec =
    List.fold_left (fun fundec (n, ty, _) ->
        let _ = makeFormalVar fundec n ty in
        fundec)
      fundec
      args
  in
  fundec

let find_formal fundec name =
  let formals = fundec.sformals in
  let rec aux name = function
    | ({vname; _} as f)::tl when (clean_name name) = vname ->
      f
    | _::tl -> aux name tl
    | [] -> raise Not_found
  in
  aux name formals

let append_stmt stmt block =
  block.bstmts <- block.bstmts@[stmt];
  block

let append_stmts stmts block =
  block.bstmts <- block.bstmts @ stmts;
  block

let rec find_fun name = function
  | GFun (({svar = {vname; _}; _}) as f, _)::tl when vname = (clean_name name) ->
    f
  | _::tl -> find_fun name tl
  | [] -> raise Not_found

let find_local fundec name =
  let locals = fundec.slocals in
  let rec aux name = function
    | ({vname; _} as f)::tl when (clean_name name) = vname ->
      f
    | _::tl -> aux name tl
    | [] -> raise Not_found
  in
  aux name locals

let printf_info = makeGlobalVar "printf" (TFun (TInt (IInt, []), Some ["format", charConstPtrType, []], true, []))
let sleep_info  = makeGlobalVar "usleep" (TFun (TInt (IInt, []), Some ["microseconds", TInt (IInt, []), []], true, []))
let atoi_info   = makeGlobalVar "atoi"   (TFun (TInt (IInt, []), Some ["str",    charConstPtrType, []], true, []))
let exit_info   = makeGlobalVar "exit"   (TFun (TVoid [],        Some ["status",  TInt (IInt, []), []], true, []))
let fflush_info = makeGlobalVar "fflush" (TFun (TInt (IInt, []), Some ["status",  TInt (IInt, []), []], true, []))
let printf_lval = Lval (Var printf_info, NoOffset)
let sleep_lval  = Lval (Var sleep_info, NoOffset)
let fflush_lval = Lval (Var fflush_info, NoOffset)
let atoi_lval   = Lval (Var atoi_info,   NoOffset)
let exit_lval   = Lval (Var exit_info,   NoOffset)

let int_read_info   = makeGlobalVar "int_read"    (TFun (TInt (IInt, []), None, true, []))
let float_read_info = makeGlobalVar "float_read"  (TFun (TFloat (FFloat, []), None, true, []))
let int_read_lval   = Lval (Var int_read_info,   NoOffset)
let float_read_lval = Lval (Var float_read_info, NoOffset)
