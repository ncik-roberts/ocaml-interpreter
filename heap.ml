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
