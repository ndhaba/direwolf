open OUnit2
open Direwolf

let count_unique lst =
  let table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x ()) lst;
  Hashtbl.length table

let size_empty_test =
  "size(empty) = 0" >:: fun _ ->
  assert_equal 0 (Trie.size Trie.empty) ~printer:string_of_int

let size_one_test =
  let fn k =
    let _, trie = Trie.insert_or_replace Trie.empty k () in
    1 = Trie.size trie
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100
       ~name:"size(insert_or_replace(empty, k, v)) = 1"
       (QCheck.string_of_size (QCheck.Gen.int_range 8 10))
       fn)

let size_arbitrary_inserts =
  let rec insert_all tree = function
    | [] -> tree
    | h :: t -> insert_all (Trie.insert tree h ()) t
  in
  let fn keys =
    let trie = insert_all Trie.empty keys in
    count_unique keys = Trie.size trie
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"size(arbitrary inserts)"
       (QCheck.small_list (QCheck.string_of_size (QCheck.Gen.int_range 8 10)))
       fn)

let mem_empty =
  let fn k = false = Trie.mem Trie.empty k in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"mem(empty, k) = false" QCheck.string fn)

let mem_insert_same =
  let fn k =
    let tree = Trie.insert_or_replace Trie.empty k () |> snd in
    true = Trie.mem tree k
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100
       ~name:"mem(insert_or_replace(empty, k), k) = true" QCheck.string fn)

let mem_insert_different =
  let fn k =
    let k2 = String.sub k 0 (String.length k - 1) in
    let tree = Trie.insert_or_replace Trie.empty k () |> snd in
    false = Trie.mem tree k2
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100
       ~name:"mem(insert_or_replace(empty, k1), k2) = false"
       (QCheck.string_of_size (QCheck.Gen.int_range 8 10))
       fn)

let get_opt_empty_tree =
  let fn k = None = Trie.get_opt Trie.empty k in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"get_opt(empty, k) = None"
       (QCheck.string_of_size (QCheck.Gen.int_range 8 10))
       fn)

let get_opt_insert_once =
  let fn (k, v) =
    let tree = Trie.insert Trie.empty k v in
    Some v = Trie.get_opt tree k
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"get_opt(set(empty, k, v), k) = Some v"
       (QCheck.tup2
          (QCheck.string_of_size (QCheck.Gen.int_range 8 10))
          QCheck.int)
       fn)

let get_insert_twice =
  let fn (k, v1, v2) =
    let tree = Trie.set Trie.empty k v1 in
    let tree = Trie.set tree k v2 in
    v2 = Trie.get tree k
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100
       ~name:"get(set(set(empty, k, v1), k, v2), k) = v2"
       (QCheck.tup3
          (QCheck.string_of_size (QCheck.Gen.int_range 8 10))
          QCheck.int QCheck.int)
       fn)

let tests =
  "Trie"
  >::: [
         size_empty_test;
         size_one_test;
         size_arbitrary_inserts;
         mem_empty;
         mem_insert_same;
         mem_insert_different;
         get_opt_empty_tree;
         get_opt_insert_once;
         get_insert_twice;
       ]
