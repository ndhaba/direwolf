type 'a t

exception Already_exists
exception Key_not_found

val empty: 'a t

val mem: 'a t -> string -> bool

val get: 'a t -> string -> 'a

val get_opt: 'a t -> string -> 'a option

val largest_prefix: 'a t -> string -> string option

val insert: 'a t -> string -> 'a -> 'a t

val insert_or_replace: 'a t -> string -> 'a -> 'a option * 'a t

val remove: 'a t -> string -> 'a option * 'a t

val replace: 'a t -> string -> 'a -> 'a * 'a t

val set: 'a t -> string -> 'a -> 'a t

val size: 'a t -> int

val nonredundancy_invar: 'a t -> bool