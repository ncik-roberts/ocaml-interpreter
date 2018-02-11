type 'a t = 'a list

val split_n : 'a list -> int -> 'a list * 'a list
val drop_n : 'a list -> int -> 'a list
val take_n : 'a list -> int -> 'a list

(* Set nth element of list to provided value;
 * filling in default values as necessary *)
val set_n : 'a list -> default:'a -> 'a -> int -> 'a list

val to_string : ('a -> string) -> 'a list -> string
