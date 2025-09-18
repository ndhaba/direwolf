type text_token =
  | Number of float
  | String of string
  | Plus
  | Minus
  | Asterisk
  | LSlash
  | Caret
  | LParen
  | RParen
  | Comma
  | Exclamation
  | SingleQuote
  | Equal
  | NotEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Assign

let lex_text text = Obj.magic ()
