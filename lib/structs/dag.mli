type 'v t
(** The directed acyclic graph type *)

val create : unit -> 'v t
(** [create ()] is a directed acyclic graph with no nodes *)

val size : 'v t -> int
(** [size graph] is the number of nodes in [graph] *)

val mem : 'v t -> 'v -> bool
(** [mem graph value] is [true] if [value] is a node in [graph], or [false]
    otherwise *)

val edges : 'v t -> 'v -> 'v list
(** [edges graph value] is a list of direct connections from [value] to other
    nodes in [graph] *)

val reverse_edges : 'v t -> 'v -> 'v list
(** [reverse_edges graph value] is a list of reverse connections from other
    nodes in [graph] to [value] *)

val dependencies : 'v t -> 'v -> 'v list
(** [dependencies graph value] is a list of all nodes in [graph] which [value]
    is connected to (directly and indirectly) *)

val dependents : 'v t -> 'v -> 'v list
(** [dependents graph value] is a list of all nodes in [graph] which either
    directly or indirectly connect to [value] *)

val insert : 'v t -> 'v -> 'v list -> bool
(** [insert graph value edges] is [true] if a node with [value] and [edges] was
    successfully able to be inserted in [graph], or [false] if a node with the
    same value already exists. In both cases, the graph is left in a valid state
    post-operation *)

val replace : 'v t -> 'v -> 'v list -> bool
(** [replace graph value edges] is [true] if the edge connections of node
    [value] was successfully changed to [edges], or [false] if this would
    introduce a cycle in the graph or if any of the referenced nodes do not
    exist. In both cases, the graph is left in a valid state post-operation *)

val connect : 'v t -> 'v -> 'v -> bool
(** [connect graph source dest] is [true] if a connection from [source] node to
    [dest] node in [graph] was successfully formed, or [false] if either
    [source] or [dest] didn't exist or if their connection would introduce a
    cycle in the graph. In both cases, the graph is left in a valid state
    post-operation *)

val disconnect : 'v t -> 'v -> 'v -> bool
(** [disconnect graph source dest] is [true] if the connection from [source]
    node to [dest] node in [graph] was successfully broken, or [false] if either
    [source] or [dest] do not exist. In both cases, the graph is left in a valid
    state post-operation *)

val remove : 'v t -> 'v -> bool
(** [remove graph value] is [true] if the node [value] in [graph] is
    successfully able to be removed, or [false] if the node has connections from
    other nodes to itself. In both cases, the graph is left in a valid state
    post-operation *)

val purge : 'v t -> 'v -> unit
(** [purge graph value] forcefully removes node [value] from [graph] *)

val sort : 'v t -> 'v list
(** [sort graph] is a list representing a topological sort of the nodes in
    [graph] *)
