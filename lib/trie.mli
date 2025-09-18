type 'a t

exception Already_exists
exception Key_not_found

val empty: 'a t

val mem: string -> 'a t -> bool

val get: string -> 'a t -> 'a

val get_opt: string -> 'a t -> 'a option

val largest_prefix: string -> 'a t -> string option

val insert: string -> 'a -> 'a t -> 'a t

val insert_or_replace: string -> 'a -> 'a t -> 'a option * 'a t

val remove: string -> 'a t -> 'a option * 'a t

val replace: string -> 'a -> 'a t -> 'a * 'a t

val set: string -> 'a -> 'a t -> 'a t

val size: 'a t -> int
