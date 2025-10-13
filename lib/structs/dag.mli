type 'k t

val create : int -> 'k t

val size : 'k t -> int

val mem : 'k t -> 'k -> bool

val edges : 'k t -> 'k -> 'k list

val reverse_edges : 'k t -> 'k -> 'k list

val dependencies : 'k t -> 'k -> 'k list

val dependents : 'k t -> 'k -> 'k list

val insert : 'k t -> 'k -> 'k list -> bool

val replace : 'k t -> 'k -> 'k list -> bool

val connect : 'k t -> 'k -> 'k -> bool

val disconnect : 'k t -> 'k -> 'k -> bool

val remove : 'k t -> 'k -> bool

val purge : 'k t -> 'k -> unit

val sort : 'k t -> 'k list