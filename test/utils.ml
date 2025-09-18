open QCheck

let distinct_list len elem =
  let open Gen in
  make
    ( len.gen >>= fun len ->
      let rec build acc tbl n =
        if n = 0 then return (List.rev acc)
        else
          elem.gen >>= fun x ->
          let found = Hashtbl.mem tbl x in
          if found then build acc tbl n
          else (
            Hashtbl.add tbl x ();
            build (x :: acc) tbl (n - 1))
      in
      let tbl = Hashtbl.create 16 in
      build [] tbl len )

let distinct_const_list n = distinct_list (QCheck.Gen.return n |> make)

let distinct_tup2 elem =
  let open Gen in
  let open List in
  make
    ((distinct_const_list 2 elem).gen >>= fun l -> Gen.return (hd l, hd (tl l)))

let make_test ?(count = 500) gen fn name =
  QCheck_ounit.to_ounit2_test (QCheck.Test.make ~count ~name gen fn)

let make_test_fail ?(count = 500) gen fn name =
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count ~name gen (fun x ->
         try
           let _ = fn x in
           false
         with _ -> true))
