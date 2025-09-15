type 'v trie = Node of 'v option * (char * 'v trie) list

let empty = Node (None, [])
let ( >> ) f g x = g (f x)

let get tree str =
  let slen = String.length str in
  let rec get_inner tree i =
    if i > slen then None
    else if i = slen then
      match tree with
      | Node (v, _) -> v
    else
      let c = String.get str i in
      match tree with
      | Node (_, subtrees) -> (
          match List.assoc_opt c subtrees with
          | Some subtree -> get_inner subtree (i + 1)
          | None -> None)
  in
  get_inner tree 0

let mem tree str = Option.is_some (get tree str)

let largest_prefix tree str =
  let rec largest_prefix_inner tree i =
    if i >= String.length str then None
    else
      let c = String.get str i in
      match tree with
      | Node (None, subtrees) -> (
          match List.assoc_opt c subtrees with
          | Some subtree -> largest_prefix_inner subtree (i + 1)
          | None -> None)
      | Node (Some _, subtrees) -> (
          match List.assoc_opt c subtrees with
          | None -> Some (String.sub str 0 (i + 1))
          | Some subtree -> (
              match largest_prefix_inner subtree (i + 1) with
              | None -> Some (String.sub str 0 (i + 1))
              | v -> v))
  in
  largest_prefix_inner tree 0

let insert_or_replace tree str value =
  let slen = String.length str in
  let rec insert_or_replace_inner tree i =
    match tree with
    | None ->
        if i = slen then (None, Node (Some value, []))
        else
          let c = String.get str i in
          let v, t = insert_or_replace_inner None (i + 1) in
          (v, Node (None, [ (c, t) ]))
    | Some (Node (v, subtrees)) ->
        if i = slen then (v, Node (Some value, subtrees))
        else
          let c = String.get str i in
          let rec replace_or_append = function
            | [] ->
                let fv, ftree =
                  insert_or_replace_inner (Some (Node (None, []))) (i + 1)
                in
                (fv, [ (c, ftree) ])
            | (k, v) :: t when k = c ->
                let fv, ftree = insert_or_replace_inner (Some v) (i + 1) in
                (fv, (k, ftree) :: t)
            | (k, v) :: t when k <> c ->
                let fv, ftrees = replace_or_append t in
                (fv, (k, v) :: ftrees)
            | _ -> assert false
          in
          let fv, subtrees = replace_or_append subtrees in
          (fv, Node (v, subtrees))
  in
  insert_or_replace_inner (Some tree) 0

let remove tree str =
  let slen = String.length str in
  let rec remove_inner tree i =
    if i = slen then
      match tree with
      | Node (value, subtrees) -> (value, Node (None, subtrees))
    else
      let c = String.get str i in
      match tree with
      | Node (root_value, subtrees) -> (
          match List.assoc_opt c subtrees with
          | None -> (None, tree)
          | Some subtree -> (
              let value, subtree = remove_inner subtree (i + 1) in
              match subtree with
              | Node (None, []) ->
                  (value, Node (root_value, List.remove_assoc c subtrees))
              | _ -> (value, tree)))
  in
  remove_inner tree 0

let rec size = function
  | Node (Some _, []) -> 1
  | Node (v, subtrees) ->
      List.fold_left
        (fun acc tree -> acc + (tree |> snd |> size))
        (if Option.is_some v then 1 else 0)
        subtrees

let nonredundancy_invar tree =
  let rec nonredundancy_invar_inner root = function
    | Node (None, []) -> root
    | Node (Some _, []) -> true
    | Node (_, subtrees) ->
        List.for_all (snd >> nonredundancy_invar_inner false) subtrees
  in
  nonredundancy_invar_inner true tree
