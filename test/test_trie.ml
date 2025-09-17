open OUnit2
open Direwolf

let small_string = QCheck.string_of_size (QCheck.Gen.int_range 8 10)

let count_unique lst =
  let table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x ()) lst;
  Hashtbl.length table

let size_empty =
  "size(empty) = 0" >:: fun _ ->
  assert_equal 0 (Trie.size Trie.empty) ~printer:string_of_int

let size_one =
  let fn k = 1 = Trie.size (Trie.insert Trie.empty k ()) in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"size(insert(empty, k, v)) = 1"
       small_string fn)

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
    (QCheck.Test.make ~count:100 ~name:"size(empty after n inserts) = n"
       (QCheck.list small_string) fn)

let mem_empty =
  let fn k = false = Trie.mem Trie.empty k in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"mem(empty, k) = false" QCheck.string fn)

let mem_insert_same =
  let fn k = true = Trie.mem (Trie.insert Trie.empty k ()) k in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"mem(insert(empty, k), k) = true"
       QCheck.string fn)

let mem_insert_different =
  let fn k =
    let k2 = String.sub k 0 (String.length k - 1) in
    false = Trie.mem (Trie.insert Trie.empty k ()) k2
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"mem(insert(empty, k1), k2) = false"
       small_string fn)

let get_opt_empty =
  let fn k = None = Trie.get_opt Trie.empty k in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"get_opt(empty, k) = None" small_string
       fn)

let get_opt_insert_once =
  let fn (k, v) = Some v = Trie.get_opt (Trie.insert Trie.empty k v) k in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100
       ~name:"get_opt(insert(empty, k, v), k) = Some v"
       (QCheck.tup2 small_string QCheck.int)
       fn)

let get_empty =
  let fn k =
    try
      let _ = Trie.get Trie.empty k in
      false
    with Trie.Key_not_found -> true
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"get(empty, k) = !" small_string fn)

let get_insert_once =
  let fn (k, v) = v = Trie.get (Trie.insert Trie.empty k v) k in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"get(insert(empty, k, v), k) = Some v"
       (QCheck.tup2 small_string QCheck.int)
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
       (QCheck.tup3 small_string QCheck.int QCheck.int)
       fn)

let largest_prefix_empty =
  let fn s = None = Trie.largest_prefix Trie.empty s in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100 ~name:"largest_prefix(empty, k) = None"
       small_string fn)

let largest_prefix_one =
  let fn (s1, s2) =
    Some s1 = Trie.largest_prefix (Trie.set Trie.empty s1 ()) (s1 ^ s2)
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100
       ~name:"largest_prefix(set(empty, s1), s1 ^ s2) = Some s1"
       (QCheck.tup2 small_string small_string)
       fn)

let largest_prefix_two =
  let fn (s1, s2, s3) =
    Some (s1 ^ s2)
    = Trie.largest_prefix
        (Trie.set (Trie.set Trie.empty s1 ()) (s1 ^ s2) ())
        (s1 ^ s2 ^ s3)
  in
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:100
       ~name:
         "largest_prefix(set(set(empty, s1), s1 ^ s2), s1 ^ s2 ^ s3) = Some \
          (s1 ^ s2)"
       (QCheck.tup3 small_string small_string small_string)
       fn)

let tests =
  "Trie"
  >::: [
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
         largest_prefix_empty;
         largest_prefix_one;
         largest_prefix_two;
       ]
