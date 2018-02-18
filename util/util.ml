let concat_map ~f xs =
  let rec loop xs acc = match xs with
    | [] -> List.rev acc
    | y :: ys ->
        let hd = List.rev (f y) in
        loop ys (hd @ acc)
  in loop xs []

