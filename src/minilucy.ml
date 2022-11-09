
(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Parse_ast

let usage = "usage: "^Sys.argv.(0)^" [options] file.lus [main]"

let parse_only = ref false
let type_only = ref false
let clock_only = ref false
let norm_only = ref false
let sched_only = ref false
let imp_only = ref false
let c_only = ref false
let lucy_printer = ref false
let ocaml_printer = ref true
let verbose = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-type-only",  Arg.Set type_only,  "  stops after typing";
   "-clock-only",  Arg.Set clock_only,  "  stops after clocking";
   "-norm-only",  Arg.Set norm_only,  "  stops after normalization";
   "-sched-only", Arg.Set sched_only, "  stops after scheduling";
   "-imp-only", Arg.Set imp_only, "  stops after imp translation";
   "-c-only", Arg.Set c_only, "  stops after imp translation";
   "-verbose",    Arg.Set verbose,    "print intermediate transformations";
   "-v",          Arg.Set verbose,    "print intermediate transformations";
  ]

let file, main_node =
  let file = ref None in
  let main = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".lus") then
      raise (Arg.Bad "no .lus extension");
    file := Some s
  in
  let set_main s =
    main := Some s
  in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> set_file s
    | 2 -> set_main s
    | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  (match !file with Some f -> f | None -> Arg.usage spec usage; exit 1),
  (match !main with Some n -> n | None -> Arg.usage spec usage; exit 1)

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let ft = Typing.type_file f main_node in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Typed ast                          */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_node_list_std ft
    end;
    if !type_only then exit 0;

    let fc = Clocking.clock_file ft main_node in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Clocked ast                          */@.";
      Format.printf "/**************************************/@.";
      Clocked_ast_printer.pp std_formatter fc
    end;
    if !clock_only then exit 0;

    let fn = Normalize.file fc in (** TODO *)
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Normalized ast                     */@.";
      Format.printf "/**************************************/@.";
      Clocked_ast_printer.pp std_formatter fn
    end;
    if !norm_only then exit 0;

    let fs = Scheduling.schedule fn in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Scheduled ast                      */@.";
      Format.printf "/**************************************/@.";
      Clocked_ast_printer.pp std_formatter fs;
    end;
    if !sched_only then exit 0;

    let fi = Imp.compile fs in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Imp ast                            */@.";
      Format.printf "/**************************************/@.";
      Format.printf "TODO\n";
    end;
    if !imp_only then exit 0;

    let file_name = (Format.sprintf "%s.c" (Filename.remove_extension file)) in

    let fc = Cgen.compile fi main_node file_name in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* C file                             */@.";
      Format.printf "/**************************************/@.";
      C_printer.pp std_formatter fc;
    end;
    if !c_only then exit 0;

    let file_c = open_out file_name in
    let out = Format.formatter_of_out_channel file_c in
    C_printer.pp out fc;
    close_out file_c;

    exit 0
  with
    | Lexical_error s ->
        report_loc (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "%s%slexical error: %s%s\n@." "\027[31m" "\027[1m" "\027[0m" s;
        exit 1
    | Parser.Error ->
        report_loc (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "%s%ssyntax error%s\n@." "\027[31m" "\027[1m" "\027[0m";
        exit 1
    | Typing.Error(l,e) ->
        report_loc l;
        eprintf "%a\n@." Typing.report e;
        exit 1
    | Clocking.Error (l, e) ->
      report_loc l;
      eprintf  "%a\n@." Clocking.report e;
    | e ->
        let _ = Fmt.comma in
        eprintf "%s%sAnomaly:%s %s\n@." "\027[31m" "\027[1m" "\027[0m" (Printexc.to_string e);
        exit 2
