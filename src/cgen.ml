open GoblintCil.Cil
open Typed_ast
open Cil_utils
open Ident

module StringMap = Map.Make(String)

(* Returns a local varinfo, and struct list *)
let compile_output_type output =
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
    makeLocalVar fundec name typ

let mk_mem_struct mem_structs fby_mem node_mem =
  ()

let compile_node globals node =
  (* TODO: Add mem struct *)
  let fun_args = List.map (fun ({name; _}, typ) ->
      name, translate_type typ, []) node.tn_input in
  let output_type = compile_output_type  node.tn_output in
  let funtyp = TFun (output_type, Some fun_args, false, []) in
  let funglob = makeGlobalVar node.tn_name.name funtyp in
  let fundec = mk_fundec funglob in
  let output_step = compile_output_step fundec output_type node.tn_output in
  let funblock = mkBlock [mkStmt (Return (Some (Lval (Var output_step, NoOffset)), locUnknown))] in
  let _locals =
    List.map
      (fun ({name; _}, typ) ->
         makeLocalVar fundec name (translate_type typ))
      node.tn_local
    |> List.fold_left (fun locals local ->
        StringMap.add local.vname local locals)
      StringMap.empty
  in
  fundec.sbody <- funblock;
  GFun (fundec, locUnknown)

let compile ast =
  let globs = List.map (compile_node []) ast in
  let open GoblintCil.Errormsg in
  List.iter (log "%a" d_global) globs;
  ast

let pp = Obj.magic ()

let write_out c_ast out =
  Format.fprintf out "%a" pp c_ast
