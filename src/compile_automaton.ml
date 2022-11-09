open Parse_ast

let gen = let r = ref 0 in fun () -> incr r; Printf.sprintf "x__%d" !r

let trad autom = assert false

let map =
  List.fold_left_map (fun acc e -> match e with
    | PE_automaton a -> acc, trad a
    | _ -> acc, Fun.id e) ([], [])

let apply node =
  let (locals, types), pn_equs = map node.pn_equs in
  types, { node with pn_local = node.pn_local @ locals ; pn_equs }

let compile { p_nodes; p_types } =
  let acc, nodes = List.fold_left_map (fun acc e -> let types, trad = apply e in types @ acc, trad) [] p_nodes in
  { p_nodes = nodes; p_types = p_types @ acc }
