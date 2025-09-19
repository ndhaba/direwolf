type 'a t
(** The trie type *)

exception Already_exists
(** An exception raised when a key already has an association *)

exception Key_not_found
(** An exception raised when a key is not found within a trie *)

val empty: 'a t
(** [empty] is an empty trie *)

val mem: string -> 'a t -> bool
(** [mem k trie] is true if and only if [k] is a key stored in [tree] *)

val get: string -> 'a t -> 'a
(** [get k trie] is the value in [trie] associated with key [k].
    Raises [Key_not_found] if [k] does not exist in [trie] *)

val get_opt: string -> 'a t -> 'a option
(** [get_opt k trie] returns the value associated with [k] in [trie].
    Returns [Some v] if [k] exists in [trie] and [None] otherwise *)

val largest_prefix: string -> 'a t -> string option
(** [largest_prefix s trie] is the largest substring of [s] starting
    from the beginning that is contained within [trie] (if one exists) *)

val insert: string -> 'a -> 'a t -> 'a t
(** [insert k v trie] is a modified version of [trie] with [k] associated
    with [v]. Raises [Already_exists] if [k] is already associated with a value *)

val insert_or_replace: string -> 'a -> 'a t -> 'a option * 'a t
(** [insert_or_replace k v trie] is the previous value associated with [k] in
    [trie] (if there is one), and a modified version of [trie] with [k] associated
    with [v]. *)

val remove: string -> 'a t -> 'a option * 'a t
(** [remove k trie] is the value associated with [k] in [trie] (if there was one),
    and a modified version of [trie] with any associated with key [k] removed *)

val replace: string -> 'a -> 'a t -> 'a * 'a t
(** [replace k v trie] is the previous value associated with [k] in [trie], and a
    modified version of [trie] where [k] is associated with [v]. Raises [Key_not_found]
    if [k] is not already associated with a value *)

val set: string -> 'a -> 'a t -> 'a t
(** Same as [insert_or_replace] but does not return the previous value associated
    with [k] *)

val size: 'a t -> int
(** [size trie] is the number of keys stored in [trie] *)