val concat_map : f:('a -> 'b list) -> 'a list -> 'b list
val tabulate : f:(int -> 'a) -> int -> 'a list
val range : lo:int -> hi:int -> int list
val map_filter : f:('a -> 'b option) -> 'a list -> 'b list
