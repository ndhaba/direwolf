open OUnit2

let tests = "Direwolf" >::: [ Test_trie.tests; Test_text_parse.tests ]
let _ = run_test_tt_main tests
