open OUnit2
open Direwolf.Types
open Direwolf.Text.Parse

let test_parse expected actual =
  "Parse tree of \"" ^ actual ^ "\"" >:: fun _ ->
  assert_equal expected (parse_text actual)

let test_parse_fail expr =
  "Parse failure of \"" ^ expr ^ "\"" >:: fun _ ->
  try
    let _ = parse_text expr in
    assert_failure "Parse succeded"
  with _ -> ()

let l v : raw_expr = Literal v
let s x = String (x, None)
let ss x s = String (x, Some (SubString s))

let tests = "Text Parsing" >::: [
  test_parse_fail "";
  (* Numbers *)
  test_parse (l 2.5) "2.5\t";
  test_parse (l 10.) "\n10";
  test_parse_fail "1.5.5";
  (* Strings *)
  test_parse (s "x") "x";
  test_parse (s "None") "None";
  test_parse (ss "A" "B") "A_B";
  test_parse (ss "Wo" "rd") "Wo_rd";
  test_parse_fail "x_1"; (* Unsupported *)
  (* Parentheses/Tuples *)
  test_parse (Parentheses (s "INNER")) "( INNER )";
  test_parse (Tuple [s "a"; s "b"]) "(a, b)";
  test_parse_fail "(in,)";
  test_parse_fail "(in 2)";
  test_parse_fail "(what";
  (* Factorial *)
  test_parse (Factorial (l 10.)) "10!";
  test_parse (Factorial (Factorial (Factorial (l 2.)))) "2!!!";
  test_parse (Factorial (ss "x" "n")) "x_n!";
  test_parse_fail "!2";
  (* Power *)
  test_parse (Power (l 2., l 3.)) "2^3";
  test_parse (Power (s "x", s "y")) "x^y";
  test_parse (Power ((ss "v" "x"), l 2.)) "v_x^2";
  test_parse (Power (l 1., Power (l 2., l 3.))) "1^2^3";
  test_parse (Power (Factorial (l 2.), Factorial (l 3.))) "2!^3!";
  (* Negate *)
  test_parse (Negate (l 5.)) "-5";
  test_parse (Negate (Parentheses (s "hey"))) "-(hey)";
  test_parse (Negate (Negate (Negate (l 100.)))) "---100";
  test_parse (Negate (Factorial (l 10.))) "-10!";
  test_parse (Negate (Power (l 5., Factorial (l 5.)))) "-5^5!";
  (* Implicit Multiplication *)
  test_parse (ImplicitMult [l 3.; s "x"]) "3x";
  test_parse (ImplicitMult [l 9.; s "π"]) "9π";
  test_parse (ImplicitMult [Negate (l 5.); Parentheses (l 24.)]) "-5(24)";
  test_parse (ImplicitMult [s "x"; l 2.]) "x2";
  test_parse (ImplicitMult [Factorial (l 6.); Factorial (l 7.)]) "6!7!";
  test_parse (ImplicitMult [Power (l 2., s "x"); l 3.]) "2^x3";
  (* Explicit Multiplication/Division *)
  test_parse (ExplicitMult [l 2.; l 3.; l 4.]) "2 * 3 * 4";
  test_parse (ExplicitMult [ImplicitMult [l 5.; s "x"]; l 9.]) "5x*9";
  test_parse (Divide (s "x", l 2.)) "x/2";
  test_parse (Divide (ImplicitMult [l 5.; s "x"], l 4.)) "5x/4";
  test_parse (Divide (ExplicitMult [l 9.; l 10.], l 21.)) "9*10/21";
  test_parse (ExplicitMult [Divide (l 10., l 100.); l 1000.]) "10/100*1000";
  (* Addition/Subtraction *)
  test_parse (Add [l 9.; l 10.; l 21.]) "9 + 10 + 21";
  test_parse (Subtract (l 90., l 9.)) "90-9";
  test_parse (Subtract (Add [l 1.; l 2.; l 3.], l 4.)) "1 + 2 + 3 - 4";
  test_parse (Subtract (Subtract (l 10., l 1.), l 1.)) "10 - 1 - 1";
  test_parse (Add [Subtract (l 4., l 3.); l 2.; l 1.]) "4 - 3 + 2 + 1"
]
[@@ocamlformat "disable"]
