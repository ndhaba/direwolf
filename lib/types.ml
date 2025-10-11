type subscript = SubString of string

type raw_expr =
  | Literal of float
  | String of string * subscript option
  | Parentheses of raw_expr
  | Tuple of raw_expr list
  | Negate of raw_expr
  | Add of raw_expr list
  | Subtract of raw_expr * raw_expr
  | ImplicitMult of raw_expr list
  | ExplicitMult of raw_expr list
  | Divide of raw_expr * raw_expr
  | Power of raw_expr * raw_expr
  | Factorial of raw_expr

type expr =
  | Literal of float
  | Variable of string * subscript option
  | Function of string * subscript option
  | FunctionCall of expr * expr list
  | Negate of expr
  | Add of expr list
  | Subtract of expr * expr
  | Multiply of expr list
  | Divide of expr * expr
  | Power of expr * expr
  | Factorial of expr
