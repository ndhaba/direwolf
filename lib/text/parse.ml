open Types

type text_token =
  | Number of float
  | Word of string
  | Gap
  | Plus
  | Minus
  | Asterisk
  | LSlash
  | Caret
  | LParen
  | RParen
  | Comma
  | Exclamation
  | Assign
  | Underscore

(** [is_whitespace chr] is whether or not [chr] is a whitespace character *)
let is_whitespace chr = chr = ' ' || chr = '\n' || chr = '\t'

(** [is_numeric chr] is whether or not [chr] can be part of a number *)
let is_numeric chr = (chr >= '0' && chr <= '9') || chr = '.'

(** [is_alphabetic chr] is whether or not [chr] represents a character in the
    alphabet *)
let is_alphabetic chr = (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z')

let is_special_symbol_start chr = chr = '\xCF'

let is_special_symbol c1 c2 =
  c1 = '\xCF' && (c2 = '\x80' || c2 = '\x84' || c2 = '\x95')

(** [lex_number str i] is a tuple containing the parsed number in [str] at [i]
    as a token and the position in [str] immediately after the number *)
let lex_number str i =
  let rec find_end_of_number i =
    if i >= String.length str then i
    else if is_numeric str.[i] then find_end_of_number (i + 1)
    else i
  in
  let rest = find_end_of_number i in
  let num_string = String.sub str i (rest - i) in
  match float_of_string_opt num_string with
  | Some value -> (Number value, rest)
  | None -> failwith ("Invalid number: " ^ num_string)

(** [lex_string str i] is a tuple containing a string of characters, along with
    the position in [str] immediately after *)
let lex_string str i =
  (* [find_end_of_word i] is the position of the end of a string of possible
     variable names *)
  let rec find_end_of_word i =
    if
      i >= String.length str
      || (not (is_alphabetic str.[i]))
         && (not (is_special_symbol_start str.[i]))
         && not (is_special_symbol '\xCF' str.[i])
    then i
    else find_end_of_word (i + 1)
  in
  let end_of_word = find_end_of_word i in
  (Word (String.sub str i (end_of_word - i)), end_of_word)

(** [lex_raw i str] is a list of tokens representing [str] starting at [i] *)
let rec lex_raw acc i str =
  let strlen = String.length str in
  if i >= strlen then List.rev acc
  else
    match str.[i] with
    | '+' -> lex_raw (Plus :: acc) (i + 1) str
    | '-' -> lex_raw (Minus :: acc) (i + 1) str
    | '*' -> lex_raw (Asterisk :: acc) (i + 1) str
    | '/' -> lex_raw (LSlash :: acc) (i + 1) str
    | '^' -> lex_raw (Caret :: acc) (i + 1) str
    | '(' -> lex_raw (LParen :: acc) (i + 1) str
    | ')' -> lex_raw (RParen :: acc) (i + 1) str
    | ',' -> lex_raw (Comma :: acc) (i + 1) str
    | '!' -> lex_raw (Exclamation :: acc) (i + 1) str
    | '_' -> lex_raw (Underscore :: acc) (i + 1) str
    | c when is_alphabetic c ->
        let token, rest = lex_string str i in
        lex_raw (token :: acc) rest str
    | c
      when is_special_symbol_start c
           && i + 1 < strlen
           && is_special_symbol c str.[i + 1] ->
        let token, rest = lex_string str i in
        lex_raw (token :: acc) rest str
    | c when is_numeric c ->
        let token, rest = lex_number str i in
        lex_raw (token :: acc) rest str
    | c when is_whitespace c -> (
        match acc with
        | Gap :: _ -> lex_raw acc (i + 1) str
        | _ -> lex_raw (Gap :: acc) (i + 1) str)
    | c -> failwith ("Invalid character: " ^ String.make 1 c)

let lex_text text = lex_raw [] 0 text

let rec parse_value text : raw_expr * text_token list =
  match text with
  (* {literal} *)
  | Number n :: t -> (Literal n, t)
  (* {variable} *)
  | Word base :: t -> (
      match t with
      | Underscore :: Word sub :: t -> (String (base, Some (SubString sub)), t)
      | Underscore :: t -> failwith "Invalid subscript"
      | t -> (String (base, None), t))
  (* (expr) ... *)
  | LParen :: Gap :: t | LParen :: t -> begin
      let rec chain acc l =
        match l with
        | RParen :: t | Gap :: RParen :: t -> (List.rev acc, t)
        | Gap :: Comma :: Gap :: t
        | Gap :: Comma :: t
        | Comma :: Gap :: t
        | Comma :: t ->
            let next, t = parse_add_sub t in
            chain (next :: acc) t
        | _ :: t -> failwith "Unexpected token"
        | [] -> failwith "Unexpected end of input"
      in
      let left, t = parse_add_sub t in
      let args, t = chain [ left ] t in
      match args with
      | [ expr ] -> (Parentheses expr, t)
      | l -> (Tuple l, t)
    end
  (* unknown token *)
  | token :: _ -> failwith "Unexpected token"
  (* end of input *)
  | [] -> failwith "Unexpected end of input"

and parse_factorial text =
  let rec chain text (left : raw_expr) : raw_expr * text_token list =
    match text with
    | Exclamation :: t -> chain t (Factorial left)
    | rest -> (left, rest)
  in
  let left, text = parse_value text in
  chain text left

and parse_pow text =
  let left, rest = parse_factorial text in
  match rest with
  | Gap :: Caret :: Gap :: rest
  | Gap :: Caret :: rest
  | Caret :: Gap :: rest
  | Caret :: rest ->
      let right, rest = parse_pow rest in
      let node : raw_expr = Power (left, right) in
      (node, rest)
  | rest -> (left, rest)

and parse_negate tokens =
  match tokens with
  | Minus :: rest ->
      let expr, rest = parse_negate rest in
      let node : raw_expr = Negate expr in
      (node, rest)
  | text -> parse_pow text

and parse_implicit_mult tokens =
  let rec chain acc = function
    | (Word _ :: t | LParen :: t | Number _ :: t) as tokens ->
        let right, rest = parse_negate tokens in
        chain (right :: acc) rest
    | tokens -> (
        match acc with
        | [ node ] -> (node, tokens)
        | acc -> (ImplicitMult (List.rev acc), tokens))
  in
  match tokens with
  | Gap :: tokens | tokens ->
      let left, rest = parse_negate tokens in
      chain [ left ] rest

and parse_mult_div tokens =
  let rec mult_chain acc = function
    | Asterisk :: t | Gap :: Asterisk :: t ->
        let right, t = parse_implicit_mult t in
        mult_chain (right :: acc) t
    | (LSlash :: t | Gap :: LSlash :: t) as tokens ->
        let node = ExplicitMult (List.rev acc) in
        div_chain node tokens
    | tokens ->
        let node = ExplicitMult (List.rev acc) in
        (node, tokens)
  and div_chain left = function
    | LSlash :: t | Gap :: LSlash :: t ->
        let right, t = parse_implicit_mult t in
        let node : raw_expr = Divide (left, right) in
        div_chain node t
    | (Asterisk :: t | Gap :: Asterisk :: t) as tokens ->
        mult_chain [ left ] tokens
    | tokens -> (left, tokens)
  in
  let left, rest = parse_implicit_mult tokens in
  match rest with
  | Asterisk :: _ | Gap :: Asterisk :: _ -> mult_chain [ left ] rest
  | LSlash :: _ | Gap :: LSlash :: _ -> div_chain left rest
  | _ -> (left, rest)

and parse_add_sub tokens =
  let rec add_chain acc = function
    | Gap :: Plus :: t | Plus :: t ->
        let right, t = parse_mult_div t in
        add_chain (right :: acc) t
    | (Minus :: t | Gap :: Minus :: t) as tokens ->
        let node : raw_expr = Add (List.rev acc) in
        subtract_chain node tokens
    | tokens ->
        let node : raw_expr = Add (List.rev acc) in
        (node, tokens)
  and subtract_chain left = function
    | Gap :: Minus :: t | Minus :: t ->
        let right, t = parse_mult_div t in
        let node : raw_expr = Subtract (left, right) in
        subtract_chain node t
    | (Plus :: t | Gap :: Plus :: t) as tokens -> add_chain [ left ] tokens
    | tokens -> (left, tokens)
  in
  let left, rest = parse_mult_div tokens in
  match rest with
  | Plus :: _ | Gap :: Plus :: _ -> add_chain [ left ] rest
  | Minus :: _ | Gap :: Minus :: _ -> subtract_chain left rest
  | _ -> (left, rest)

let parse_text_tokens = parse_add_sub

let parse_text text =
  match parse_text_tokens (lex_text text) with
  | tree, [] | tree, [ Gap ] -> tree
  | _, _ -> failwith "Expected EOF"
