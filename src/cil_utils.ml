open GoblintCil.Cil
open Asttypes
open Typed_ast
open Ident

let comp_num = ref 0

let get_next_comp_name () =
  let name = "comp_"^(string_of_int !comp_num) in
  incr comp_num;
  name

let translate_type = function
  | Tbool -> TInt (IInt, [])
  | Tint -> TInt (IInt, [])
  | Treal -> TFloat (FFloat, [])

let translate_const = function
  | Cbool b ->
    let cilint = mkCilint IInt (if b then 1L else 0L) in
    CInt (cilint, IInt, None)
  | Cint i ->
    let cilint = mkCilint IInt (Int64.of_int i) in
    CInt (cilint, IInt, None)
  | Creal f ->
    CReal (f, FFloat, None)

let true_const = integer 1
let false_const = integer 0
let bool_t = TInt (IInt, [])
let int_t = TInt (IInt, [])
let real_t = TFloat (FFloat, [])

let clean_name name =
  let str = Str.regexp {|'|} in
  Str.global_replace str "__" name

let mk_struct name var_l =
  let comp = mkCompInfo true name (fun _ -> []) [] in
  let fields = List.map (fun ({name; _}, typ, _) ->
      {fcomp = comp; fname = clean_name name; ftype = translate_type typ; fbitfield = None; fattr = []; floc = locUnknown})
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
