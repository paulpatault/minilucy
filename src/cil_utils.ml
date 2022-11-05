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

let mk_struct var_l =
  let comp = mkCompInfo true (get_next_comp_name ()) (fun _ -> []) [] in
  let fields = List.map (fun ({name; _}, typ) ->
      {fcomp = comp; fname = name; ftype = translate_type typ; fbitfield = None; fattr = []; floc = locUnknown})
      var_l
  in
  comp.cfields <- fields;
  comp

let mk_fundec varinfo = {
    svar = varinfo;
    sformals = [];
    slocals = [];
    smaxid = 0;
    sbody = mkBlock [];
    smaxstmtid = None;
    sallstmts = [];
  }
