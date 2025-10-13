type 'v edges = {
  mutable edges : 'v list;
  mutable reverse_edges : 'v list;
}

type 'v t = ('v, 'v edges) Hashtbl.t

let create initial_size = Hashtbl.create initial_size
let size graph = Hashtbl.length graph
let mem graph key = Hashtbl.mem graph key

let edges graph key =
  match Hashtbl.find_opt graph key with
  | Some { edges } -> edges
  | None -> []

let reverse_edges graph key =
  match Hashtbl.find_opt graph key with
  | Some { reverse_edges } -> reverse_edges
  | None -> []

let recursive_collect f root =
  let explored = Hashtbl.create 10 in
  let queue = Queue.create () in
  List.iter (fun v -> Queue.add v queue) (f root);
  while not (Queue.is_empty queue) do
    let value = Queue.take queue in
    if Hashtbl.mem explored value then ()
    else (
      Hashtbl.add explored value ();
      List.iter (fun v -> Queue.add v queue) (f value))
  done;
  explored |> Hashtbl.to_seq_keys |> List.of_seq

let dependencies graph value = recursive_collect (edges graph) value
let dependents graph value = recursive_collect (reverse_edges graph) value
let rec mems graph = List.for_all (fun v -> mem graph v)

let rec add_to_reverse_edges graph value =
  List.iter (fun v ->
      let obj = Hashtbl.find graph v in
      obj.reverse_edges <- value :: obj.reverse_edges)

let rec list_find_first_remove f = function
  | [] -> (false, [])
  | h :: t ->
      if f h then (true, t)
      else
        let b, t = list_find_first_remove f t in
        (b, h :: t)

let list_remove_first f l = snd (list_find_first_remove f l)

let rec remove_from_reverse_edges graph value =
  List.iter (fun v ->
      let obj = Hashtbl.find graph v in
      obj.reverse_edges <- list_remove_first (( = ) value) obj.reverse_edges)

let insert graph key edges =
  if Hashtbl.mem graph key then false
  else if List.mem key edges then false
  else if not (mems graph edges) then false
  else (
    Hashtbl.add graph key { edges; reverse_edges = [] };
    add_to_reverse_edges graph key edges;
    true)

let dfs_check graph start edges =
  let explored = Hashtbl.create 10 in
  let rec chain acc =
    match acc with
    | [] -> true
    | elt :: t -> (
        if Hashtbl.mem explored elt then chain t
        else if elt = start then false
        else
          match Hashtbl.find_opt graph elt with
          | None -> false
          | Some { edges } -> chain (edges @ t))
  in
  chain edges

let list_diff a b =
  let rec chain acc a b =
    match a with
    | [] -> (acc, b)
    | h :: t ->
        let found, b = list_find_first_remove (( = ) h) b in
        if found then chain acc t b else chain (h :: acc) t b
  in
  chain [] a b

let replace graph key edges =
  if not (dfs_check graph key edges) then false
  else
    let edges_obj = Hashtbl.find graph key in
    let add_edges, remove_edges = list_diff edges edges_obj.edges in
    edges_obj.edges <- edges;
    add_to_reverse_edges graph key add_edges;
    remove_from_reverse_edges graph key remove_edges;
    true

let connect graph f t =
  match (Hashtbl.find_opt graph f, Hashtbl.find_opt graph t) with
  | Some fe, Some te ->
      if List.mem t fe.edges then true
      else if not (dfs_check graph f [ t ]) then false
      else (
        fe.edges <- t :: fe.edges;
        te.reverse_edges <- f :: te.reverse_edges;
        true)
  | _, _ -> false

let disconnect graph f t =
  match (Hashtbl.find_opt graph f, Hashtbl.find_opt graph t) with
  | Some fe, Some te ->
      if List.mem t fe.edges then (
        fe.edges <- list_remove_first (( = ) t) fe.edges;
        te.reverse_edges <- list_remove_first (( = ) f) te.reverse_edges;
        true)
      else true
  | _, _ -> false

let remove graph key =
  match Hashtbl.find_opt graph key with
  | None -> false
  | Some edges_obj ->
      if List.is_empty edges_obj.reverse_edges then (
        remove_from_reverse_edges graph key edges_obj.edges;
        Hashtbl.remove graph key;
        true)
      else false

let rec remove_from_edges graph key = function
  | [] -> ()
  | h :: t ->
      let node = Hashtbl.find graph h in
      node.edges <- list_remove_first (( = ) key) node.edges;
      remove_from_edges graph key t

let purge graph key =
  match Hashtbl.find_opt graph key with
  | None -> ()
  | Some node ->
      remove_from_reverse_edges graph key node.edges;
      remove_from_edges graph key node.reverse_edges;
      Hashtbl.remove graph key

let sort graph =
  let in_degrees = Hashtbl.create (Hashtbl.length graph) in
  Hashtbl.iter
    (fun node data ->
      Hashtbl.replace in_degrees node (List.length data.reverse_edges))
    graph;
  let queue = Queue.create () in
  Hashtbl.iter (fun node deg -> if deg = 0 then Queue.add node queue) in_degrees;
  let result = ref [] in
  while not (Queue.is_empty queue) do
    let node = Queue.take queue in
    result := node :: !result;
    match Hashtbl.find_opt graph node with
    | None -> ()
    | Some { edges } ->
        List.iter
          (fun neighbor ->
            match Hashtbl.find_opt in_degrees neighbor with
            | None -> ()
            | Some deg ->
                Hashtbl.replace in_degrees neighbor (deg - 1);
                if deg = 1 then Queue.add neighbor queue)
          edges
  done;
  List.rev !result
