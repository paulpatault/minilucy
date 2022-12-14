type location = Lexing.position * Lexing.position

type base_ty =
  | Tbool
  | Tint
  | Treal
  | Tadt of string

type ty = base_ty list

type adt_type = { name: string; constr: string list }

type const =
  | Cbool of bool
  | Cint of int
  | Creal of float
  | Cadt of string * string option

type op =
  | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod
  | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
  | Op_not
  | Op_and | Op_or | Op_impl
  | Op_if

