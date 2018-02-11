type 'a t
val empty : 'a t

(**
 * Add elements of list to heap; return new heap and
 * address within heap.
 *)
val alloc : 'a t -> 'a list -> int * 'a t

(**
 * lookup heap addr size
 * Look up size elements of heap starting at address addr.
 *)
val lookup : 'a t -> int -> int -> 'a list

val to_string : ('a -> string) -> 'a t -> string
