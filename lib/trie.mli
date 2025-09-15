type 'a t

val empty: 'a t

val mem: 'a t -> string -> bool

val get: 'a t -> string -> 'a option

val largest_prefix: 'a t -> string -> string option

val insert_or_replace: 'a t -> string -> 'a -> 'a option * 'a t

val remove: 'a t -> string -> 'a option * 'a t

val size: 'a t -> int

val nonredundancy_invar: 'a t -> bool