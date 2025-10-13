type 'v edges = {
  mutable edges : 'v list;
  mutable reverse_edges : 'v list;
}

type 'v t = ('v, 'v edges) Hashtbl.t

(** [create ()] is a directed acyclic graph with no nodes *)
let create () = Hashtbl.create 100

(** [size graph] is the number of nodes in [graph] *)
let size graph = Hashtbl.length graph

(** [mem graph value] is [true] if [value] is a node in [graph], or [false]
    otherwise *)
let mem graph key = Hashtbl.mem graph key

(** [edges graph value] is a list of direct connections from [value] to other
    nodes in [graph] *)
let edges graph key =
  match Hashtbl.find_opt graph key with
  | Some { edges } -> edges
  | None -> []

(** [reverse_edges graph value] is a list of reverse connections from other
    nodes in [graph] to [value] *)
let reverse_edges graph key =
  match Hashtbl.find_opt graph key with
  | Some { reverse_edges } -> reverse_edges
  | None -> []

(** [recursive_collect f root] is a list of all node values obtained by
    recursively applying [f] on [root] and its results *)
let recursive_collect f root =
  let explored = Hashtbl.create 10 in
  (* Setup the queue with starting values [f root] *)
  let queue = Queue.create () in
  List.iter (fun v -> Queue.add v queue) (f root);
  (* Keep going until the queue is empty *)
  while not (Queue.is_empty queue) do
    let value = Queue.take queue in
    (* Skip if this value was already explored *)
    if Hashtbl.mem explored value then ()
    else (
      Hashtbl.add explored value ();
      List.iter (fun v -> Queue.add v queue) (f value))
  done;
  (* The keys are all that matter *)
  explored |> Hashtbl.to_seq_keys |> List.of_seq

(** [dependencies graph value] is a list of all nodes in [graph] which [value]
    is connected to (directly and indirectly) *)
let dependencies graph value = recursive_collect (edges graph) value

(** [dependents graph value] is a list of all nodes in [graph] which either
    directly or indirectly connect to [value] *)
let dependents graph value = recursive_collect (reverse_edges graph) value

(** [mems graph list] is [true] if every element in [list] corresponds to a node
    in [graph], or [false] otherwise *)
let rec mems graph = List.for_all (fun v -> mem graph v)

(** [add_to_reverse_edges graph value edges] adds [value] to the list of reverse
    edges in each node in [graph] referenced by [edges] *)
let rec add_to_reverse_edges graph value =
  List.iter (fun v ->
      let obj = Hashtbl.find graph v in
      obj.reverse_edges <- value :: obj.reverse_edges)

(** [list_find_first_remove f lst] is a 2-element tuple containing: [true] or
    [false] depending on if an element [v] such that [f v = true] was found in
    [lst] and removed, and the modified [lst] *)
let rec list_find_first_remove f = function
  | [] -> (false, [])
  | h :: t ->
      if f h then (true, t)
      else
        let b, t = list_find_first_remove f t in
        (b, h :: t)

(** [list_remove_first f lst] is a modification of [lst] where the first element
    [v] such that [f v = true] is removed *)
let list_remove_first f lst = snd (list_find_first_remove f lst)

(** [remove_from_reverse_edges graph value edges] removes [value] from the list
    of reverse edges in each node in [graph] referenced by [edges] *)
let rec remove_from_reverse_edges graph value =
  List.iter (fun v ->
      let obj = Hashtbl.find graph v in
      obj.reverse_edges <- list_remove_first (( = ) value) obj.reverse_edges)

(** [insert graph value edges] is [true] if a node with [value] and [edges] was
    successfully able to be inserted in [graph], or [false] if a node with the
    same value already exists. In both cases, the graph is left in a valid state
    post-operation *)
let insert graph key edges =
  if Hashtbl.mem graph key then false
  else if List.mem key edges then false
  else if not (mems graph edges) then false
  else (
    Hashtbl.add graph key { edges; reverse_edges = [] };
    add_to_reverse_edges graph key edges;
    true)

(** [dfs_search graph start edges] is [true] if a depth-first search for [goal]
    within [graph] and starting at [start] is successful, or [false] if it is
    unsuccessful.*)
let dfs_search graph goal start =
  let explored = Hashtbl.create 10 in
  let rec chain acc =
    match acc with
    | [] -> false
    | elt :: t ->
        if Hashtbl.mem explored elt then chain t
        else if elt = goal then true
        else chain ((Hashtbl.find graph elt).edges @ t)
  in
  chain start

(** [list_diff a b] is a 2-element tuple containing: a subset of [a] not
    contained in [b], and a subset of [b] not contained in [a] *)
let list_diff a b =
  let rec chain acc a b =
    match a with
    | [] -> (acc, b)
    | h :: t ->
        let found, b = list_find_first_remove (( = ) h) b in
        if found then chain acc t b else chain (h :: acc) t b
  in
  chain [] a b

(** [replace graph value edges] is [true] if the edge connections of node
    [value] was successfully changed to [edges], or [false] if this would
    introduce a cycle in the graph or if any of the referenced nodes do not
    exist. In both cases, the graph is left in a valid state post-operation *)
let replace graph key edges =
  if not (mems graph edges) then false
  else if dfs_search graph key edges then false
  else
    let edges_obj = Hashtbl.find graph key in
    let add_edges, remove_edges = list_diff edges edges_obj.edges in
    edges_obj.edges <- edges;
    add_to_reverse_edges graph key add_edges;
    remove_from_reverse_edges graph key remove_edges;
    true

(** [connect graph source dest] is [true] if a connection from [source] node to
    [dest] node in [graph] was successfully formed, or [false] if either
    [source] or [dest] didn't exist or if their connection would introduce a
    cycle in the graph. In both cases, the graph is left in a valid state
    post-operation *)
let connect graph source dest =
  match (Hashtbl.find_opt graph source, Hashtbl.find_opt graph dest) with
  | Some fe, Some te ->
      if List.mem dest fe.edges then true
      else if dfs_search graph source [ dest ] then false
      else (
        fe.edges <- dest :: fe.edges;
        te.reverse_edges <- source :: te.reverse_edges;
        true)
  | _, _ -> false

(** [disconnect graph source dest] is [true] if the connection from [source]
    node to [dest] node in [graph] was successfully broken, or [false] if either
    [source] or [dest] do not exist. In both cases, the graph is left in a valid
    state post-operation *)
let disconnect graph source dest =
  match (Hashtbl.find_opt graph source, Hashtbl.find_opt graph dest) with
  | Some fe, Some te ->
      if List.mem dest fe.edges then (
        fe.edges <- list_remove_first (( = ) dest) fe.edges;
        te.reverse_edges <- list_remove_first (( = ) source) te.reverse_edges;
        true)
      else true
  | _, _ -> false

(** [remove graph value] is [true] if the node [value] in [graph] is
    successfully able to be removed, or [false] if the node has connections from
    other nodes to itself. In both cases, the graph is left in a valid state
    post-operation *)
let remove graph key =
  match Hashtbl.find_opt graph key with
  | None -> false
  | Some edges_obj ->
      if List.is_empty edges_obj.reverse_edges then (
        remove_from_reverse_edges graph key edges_obj.edges;
        Hashtbl.remove graph key;
        true)
      else false

(** [remove_from_edges graph value reverse_edges] removes [value] from the list
    of edges in each node in [graph] referenced by [reverse_edges] *)
let rec remove_from_edges graph key = function
  | [] -> ()
  | h :: t ->
      let node = Hashtbl.find graph h in
      node.edges <- list_remove_first (( = ) key) node.edges;
      remove_from_edges graph key t

(** [purge graph value] forcefully removes node [value] from [graph] *)
let purge graph key =
  match Hashtbl.find_opt graph key with
  | None -> ()
  | Some node ->
      remove_from_reverse_edges graph key node.edges;
      remove_from_edges graph key node.reverse_edges;
      Hashtbl.remove graph key

(** [sort graph] is a list representing a topological sort of the nodes in
    [graph] *)
let sort graph =
  (* Kahn's Algorithm for Topological Sorting *)
  (* Setup in-degree table *)
  let in_degrees = Hashtbl.create (Hashtbl.length graph) in
  Hashtbl.iter
    (fun node data ->
      Hashtbl.replace in_degrees node (List.length data.reverse_edges))
    graph;
  (* Initialize queue with nodes with in-degree value 0 *)
  let queue = Queue.create () in
  Hashtbl.iter (fun node deg -> if deg = 0 then Queue.add node queue) in_degrees;
  let result = ref [] in
  while not (Queue.is_empty queue) do
    (* This node has an in-degree value of 0, meaning there are no preceding
       dependents left *)
    let node = Queue.take queue in
    result := node :: !result;
    (* Iterate over the edges of this node *)
    List.iter
      (fun neighbor ->
        (* Decrement the associated in-degree value *)
        (* If the in-degree value is 0, add it to the queue *)
        let deg = Hashtbl.find in_degrees neighbor - 1 in
        Hashtbl.replace in_degrees neighbor deg;
        if deg = 0 then Queue.add neighbor queue)
      (Hashtbl.find graph node).edges
  done;
  List.rev !result
