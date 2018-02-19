let concat_map ~f xs =
  let rec loop xs acc = match xs with
    | [] -> List.rev acc
    | y :: ys ->
        let hd = List.rev (f y) in
        loop ys (hd @ acc)
  in loop xs []

let tabulate ~f n =
  let rec loop m acc = match m with
    | 0 -> acc
    | _ -> loop (m-1) (f (m-1) :: acc)
  in loop n []

let range ~lo ~hi = tabulate ~f:((+) lo) (hi - lo)

let map_filter ~f xs =
  let rec loop xs acc =
    match xs with
    | [] -> List.rev acc
    | y :: ys ->

    match f y with
    | None -> loop ys acc
    | Some y' -> loop ys (y' :: acc)
  in loop xs []
