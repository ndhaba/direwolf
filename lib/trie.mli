type 'a trie

val empty: 'a trie

val mem: 'a trie -> string -> bool

val get: 'a trie -> string -> 'a option

val largest_prefix: 'a trie -> string -> string option

val insert_or_replace: 'a trie -> string -> 'a -> 'a option * 'a trie

val remove: 'a trie -> string -> 'a option * 'a trie

val size: 'a trie -> int

val nonredundancy_invar: 'a trie -> bool