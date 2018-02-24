type t = int

let state = ref 0

let create () =
  let s = !state in
  state := s + 1;
  s

let to_int lbl = lbl

module Map = Map.Make (struct
  type t = int
  let compare = (compare : int -> int -> int)
end)
