let rec combine3 l1 l2 l3 =
  match l1, l2, l3 with
  | h1::t1, h2::t2, h3::t3 ->
    (h1, h2, h3)::(combine3 t1 t2 t3)
  | [], [], [] -> []
  | _ -> raise (Invalid_argument "Not same length")


let rec list_get_idx x = function
  | [] -> raise Not_found
  | e::_ when e = x -> 0
  | _::k -> 1 + list_get_idx x k
