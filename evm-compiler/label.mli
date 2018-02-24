type t

(** Create globally flesh label *)
val create : unit -> t

(** Convert label to int *)
val to_int : t -> int

module Map : Map.S with type key = t
