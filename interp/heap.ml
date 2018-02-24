module M = Map.Make(struct
  type t = int
  let compare = (compare : int -> int -> int)
end)

(* max address used, heap *)
type 'a t = int * 'a M.t
let empty = (0, M.empty)

let alloc ((max_addr, _) as heap) fields =
  let h =
    List.fold_left (fun (max, h) field ->
      (max + 1, M.add max field h))
      heap fields
  in (max_addr, h)

let lookup (_, h) addr size =
  let rec loop size acc = match size with
    | 0 -> acc
    | _ -> let elem = M.find (addr+size-1) h in
           loop (size-1) (elem :: acc)
  in loop size []

let range =
  let rec loop acc n =
    if n <= 0 then acc
    else loop (n-1 :: acc) (n-1)
  in loop []

let to_string f (max_addr, h) =
  let to_string_at addr =
    Printf.sprintf "%s -> %s" (string_of_int addr) (f (M.find addr h)) in

  let body = range max_addr
    |> List.map to_string_at
    |> String.concat "; "
  in "{ " ^ body ^ " }"
