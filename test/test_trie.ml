open OUnit2
open Utils
open Direwolf

let test_key = make_test QCheck.string
let test_key_fail f = make_test_fail QCheck.string f
let test_key_value = make_test QCheck.(tup2 string int)

let test_key_list =
  make_test QCheck.(distinct_list (int_bound 20) string_printable)

let test_key2_value =
  make_test QCheck.(tup2 (distinct_tup2 string_printable) int)

let size_empty =
  "size(empty) = 0" >:: fun _ ->
  assert_equal 0 Trie.(size empty) ~printer:string_of_int

let size_one =
  test_key
    (fun k -> 1 = Trie.(size (insert k () empty)))
    "size(insert(empty, k, v)) = 1"

let size_arbitrary_inserts =
  test_key_list
    (fun l ->
      List.length l
      = Trie.size (List.fold_left (fun t k -> Trie.insert k () t) Trie.empty l))
    "size(empty after n inserts) = n"

let mem_empty =
  test_key (fun k -> false = Trie.mem k Trie.empty) "mem(empty, k) = false"

let mem_insert_same =
  test_key
    (fun k -> true = Trie.mem k (Trie.insert k () Trie.empty))
    "mem(insert(empty, k), k) = true"

let mem_insert_different =
  test_key2_value
    (fun ((k1, k2), _) -> false = Trie.mem k2 (Trie.insert k1 () Trie.empty))
    "mem(insert(empty, k1), k2) = false"

let get_opt_empty =
  test_key
    (fun k -> None = Trie.get_opt k Trie.empty)
    "get_opt(empty, k) = None"

let get_opt_insert_once =
  test_key_value
    (fun (k, v) -> Some v = Trie.get_opt k (Trie.insert k v Trie.empty))
    "get_opt(insert(empty, k, v), k) = Some v"

let get_empty =
  test_key_fail (fun k -> Trie.get k Trie.empty) "get(empty, k) = !"

let get_insert_once =
  test_key_value
    (fun (k, v) -> v = Trie.get k (Trie.insert k v Trie.empty))
    "get(insert(empty, k, v), k) = Some v"

let get_insert_twice =
  let fn (k, v1, v2) =
    let tree = Trie.set k v1 Trie.empty in
    let tree = Trie.set k v2 tree in
    v2 = Trie.get k tree
  in
  make_test
    QCheck.(tup3 string_printable int int)
    fn "get(set(set(empty, k, v1), k, v2), k) = v2"

let insert_twice =
  test_key_fail
    Trie.(fun k -> insert k () (insert k () empty))
    "insert(k, v2, insert(k, v1, empty)) = !"

let replace_empty =
  test_key_fail Trie.(fun k -> replace k () empty) "replace(k, v, empty) = !"

let replace_matches_insert =
  make_test
    QCheck.(tup3 string_printable int int)
    Trie.(
      fun (k, v1, v2) ->
        (v1, insert k v2 empty) = replace k v2 (insert k v1 empty))
    "replace(k, v2, insert(k, v1, empty)) = v1, insert(k, v2, empty)"

let largest_prefix_empty =
  test_key
    (fun k -> None = Trie.largest_prefix k Trie.empty)
    "largest_prefix(empty, k) = None"

let largest_prefix_one =
  make_test
    QCheck.(tup2 string_printable string_printable)
    (fun (s1, s2) ->
      Some s1 = Trie.largest_prefix (s1 ^ s2) (Trie.insert s1 () Trie.empty))
    "largest_prefix(insert(empty, s1), s1 ^ s2) = Some s1"

let largest_prefix_two =
  make_test
    QCheck.(tup3 string_printable string_printable string_printable)
    (fun (s1, s2, s3) ->
      Some (s1 ^ s2)
      = Trie.largest_prefix
          (s1 ^ s2 ^ s3)
          (Trie.set (s1 ^ s2) () (Trie.insert s1 () Trie.empty)))
    "largest_prefix(insert(insert(empty, s1), s1 ^ s2), s1 ^ s2 ^ s3) = Some \
     (s1 ^ s2)"

let remove_empty =
  test_key
    (fun k -> (None, Trie.empty) = Trie.remove k Trie.empty)
    "remove(k, empty) = None, empty"

let remove_after_insert_same =
  test_key_value
    (fun (k, v) ->
      (Some v, Trie.empty) = Trie.remove k (Trie.insert k v Trie.empty))
    "remove(k, insert(k, v)) = Some v, empty"

let remove_after_insert_diff =
  test_key2_value
    (fun ((k1, k2), v) ->
      let tree = Trie.insert k1 v Trie.empty in
      (None, tree) = Trie.remove k2 tree)
    "remove(k2, insert(k1, v, empty)) = None, insert(k1, v, empty)"

let remove_after_2_inserts =
  test_key2_value
    (fun ((k1, k2), v) ->
      let tree = Trie.insert k1 v Trie.empty in
      (Some v, tree) = Trie.remove k2 (Trie.insert k2 v tree))
    "remove(k2, insert(k2, v, insert(k1, v, empty))) = Some v, insert(k1, v, \
     empty)"

let tests = "Trie" >::: [
  size_empty;
  size_one;
  size_arbitrary_inserts;
  mem_empty;
  mem_insert_same;
  mem_insert_different;
  get_opt_empty;
  get_opt_insert_once;
  get_empty;
  get_insert_once;
  get_insert_twice;
  insert_twice;
  replace_empty;
  replace_matches_insert;
  largest_prefix_empty;
  largest_prefix_one;
  largest_prefix_two;
  remove_empty;
  remove_after_insert_same;
  remove_after_insert_diff;
  remove_after_2_inserts
]
[@@ocamlformat "disable"]
