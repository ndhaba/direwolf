type comparator =
  | Eq
  | Neq
  | Gt
  | Ge
  | Lt
  | Le
  | Custom of raw_expr

and raw_expr =
  | Literal of float
  | String of string
  | Subscript of raw_expr * raw_expr
  | Negate of raw_expr
  | Add of raw_expr list
  | Subtract of raw_expr list
  | Multiply of raw_expr list
  | Divide of raw_expr * raw_expr
  | Exponentiate of raw_expr * raw_expr
  | ExpParen of raw_expr * raw_expr
  | Factorial of raw_expr
  | Comparison of raw_expr * (comparator * raw_expr) list

type expr =
  | Literal of float
  | Variable of string * expr option
  | Function of string * int * expr option * expr list
  | Negate of expr
  | Add of expr list
  | Subtract of expr list
  | Multiply of expr list
  | Divide of expr * expr
  | Exponentiate of expr * expr
  | Factorial of expr
  | Comparison of raw_expr * (comparator * raw_expr) list
