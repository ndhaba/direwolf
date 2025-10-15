type 'v t = Node of 'v option * (char * 'v t) list

exception Already_exists
(** An exception raised when a key already has an association *)

exception Key_not_found
(** An exception raised when a key is not found within a trie *)

(** [empty] is an empty trie *)
let empty = Node (None, [])

(** [get_opt k trie] returns the value associated with [k] in [trie]. Returns
    [Some v] if [k] exists in [trie] and [None] otherwise *)
let get_opt k trie =
  let slen = String.length k in
  let rec get_inner (Node (v, subtrees)) i =
    (* If [i = slen], [trie] is the node representing [k] *)
    if i = slen then v
    else
      (* If [i <> slen], keep traversing down [trie] *)
      let c = String.get k i in
      match List.assoc_opt c subtrees with
      (* If there is a subtree for [c], then our search continues *)
      | Some subtree -> get_inner subtree (i + 1)
      (* If there isn't, [k] does not exist in [trie] *)
      | None -> None
  in
  get_inner trie 0

(** [get k trie] is the value in [trie] associated with key [k]. Raises
    [Key_not_found] if [k] does not exist in [trie] *)
let get str tree =
  match get_opt str tree with
  | None -> raise Key_not_found
  | Some v -> v

(** [mem k trie] is true if and only if [k] is a key stored in [tree] *)
let mem str tree = Option.is_some (get_opt str tree)

(** [largest_prefix s trie] is the largest substring of [s] starting from the
    beginning that is contained within [trie] (if one exists) *)
let largest_prefix str tree =
  let slen = String.length str in
  let rec largest_prefix_inner tree i =
    (* If [i = slen], we've reached the end of the search *)
    if i = slen then
      match tree with
      | Node (Some _, _) -> Some str
      | Node (None, _) -> None
    else
      (* If [i <> slen], then we can still traverse deeper *)
      let c = String.get str i in
      match tree with
      (* If there's no value, then the largest prefix is NOT this string. It
         could only be a larger substring *)
      | Node (None, subtrees) -> (
          match List.assoc_opt c subtrees with
          | Some subtree -> largest_prefix_inner subtree (i + 1)
          | None -> None)
      (* If there is a value, then the largest prefix is the string at this
         point, or preferentially a larger substring *)
      | Node (Some _, subtrees) -> (
          match List.assoc_opt c subtrees with
          | None -> Some (String.sub str 0 i)
          | Some subtree -> (
              match largest_prefix_inner subtree (i + 1) with
              | None -> Some (String.sub str 0 i)
              | v -> v))
  in
  largest_prefix_inner tree 0

(** [insert_or_replace k v trie] is the previous value associated with [k] in
    [trie] (if there is one), and a modified version of [trie] with [k]
    associated with [v]. *)
let insert_or_replace str value tree =
  let slen = String.length str in
  let rec insert_or_replace_inner (Node (v, subtrees)) i =
    if i = slen then (v, Node (Some value, subtrees))
    else
      let c = String.get str i in
      (* Operates on the list of subtrees in a Node *)
      let rec replace_or_append = function
        (* Create a node if it doesn't exist *)
        | [] ->
            let fv, ftree = insert_or_replace_inner (Node (None, [])) (i + 1) in
            (fv, [ (c, ftree) ])
        (* Replace this entry if the key is the same *)
        | (k, v) :: t when k = c ->
            let fv, ftree = insert_or_replace_inner v (i + 1) in
            (fv, (k, ftree) :: t)
        (* Keep looking through the list of subtrees if the key isn't the
           same *)
        | (k, v) :: t when k <> c ->
            let fv, ftrees = replace_or_append t in
            (fv, (k, v) :: ftrees)
        (* Impossible *)
        | _ -> assert false
      in
      let fv, subtrees = replace_or_append subtrees in
      (fv, Node (v, subtrees))
  in
  insert_or_replace_inner tree 0

(** [insert k v trie] is a modified version of [trie] with [k] associated with
    [v]. Raises [Already_exists] if [k] is already associated with a value *)
let insert str value tree =
  match insert_or_replace str value tree with
  | None, tree -> tree
  | Some _, _ -> raise Already_exists

(** [replace k v trie] is the previous value associated with [k] in [trie], and
    a modified version of [trie] where [k] is associated with [v]. Raises
    [Key_not_found] if [k] is not already associated with a value *)
let replace str value tree =
  match insert_or_replace str value tree with
  | Some v, tree -> (v, tree)
  | None, _ -> raise Key_not_found

(** [remove k trie] is the value associated with [k] in [trie] (if there was
    one), and a modified version of [trie] with any associated with key [k]
    removed *)
let remove str tree =
  let slen = String.length str in
  let rec remove_inner (Node (root_value, subtrees)) i =
    if i = slen then (root_value, Node (None, subtrees))
    else
      let c = String.get str i in
      match List.assoc_opt c subtrees with
      (* No subtree => no value to remove *)
      | None -> (None, Node (root_value, subtrees))
      | Some subtree -> (
          (* Replace the subtree within the list of subtrees *)
          let value, subtree = remove_inner subtree (i + 1) in
          let subtrees = List.remove_assoc c subtrees in
          match subtree with
          (* Prune trees with no value or subtrees *)
          | Node (None, []) -> (value, Node (root_value, subtrees))
          | subtree -> (value, Node (root_value, (c, subtree) :: subtrees)))
  in
  remove_inner tree 0

(** Same as [insert_or_replace] but does not return the previous value
    associated with [k] *)
let set str value tree = snd (insert_or_replace str value tree)

(** [size trie] is the number of keys stored in [trie] *)
let rec size = function
  | Node (Some _, []) -> 1
  | Node (v, subtrees) ->
      List.fold_left
        (fun acc tree -> acc + (tree |> snd |> size))
        (if Option.is_some v then 1 else 0)
        subtrees
