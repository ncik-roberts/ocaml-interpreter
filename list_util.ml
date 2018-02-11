type 'a t = 'a list

let split_n xs n =
  let rec loop hdRev tl n = match n, tl with
    | 0, _ -> (List.rev hdRev, tl)
    | _, x :: xs -> loop (x :: hdRev) xs (n-1)
    | _ -> assert false
  in loop [] xs n

let take_n xs n = fst (split_n xs n)
let drop_n xs n = snd (split_n xs n)

let set_n xs ~default x n =
  let rec loop hdRev tl n = match n, tl with
    | 0, _ -> List.rev hdRev @ x :: tl
    | _, [] -> loop (default :: hdRev) tl (n-1)
    | _, y :: ys -> loop (y :: hdRev) ys (n-1)
  in loop [] xs n

let to_string f xs =
  let result = List.fold_right (fun x str -> f x ^ ";" ^ str) xs "" in
  "[" ^ result ^ "]"
