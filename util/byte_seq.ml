(* Bytes are a reversed list of characters *)
type t = char list

let to_char x = Char.unsafe_chr (Int64.to_int x)

let of_int64 =
  let mask = 0xFF in
  let mask64 = Int64.of_int mask in
  let rec loop x ~acc =
    if x <= mask64 then List.rev (to_char x :: acc)
    else
      let curr_byte = Int64.logand x mask64 |> to_char in
      let rest_bytes = Int64.shift_right_logical x 8 in
      loop rest_bytes ~acc:(curr_byte :: acc)
  in loop ~acc:[]

let alphabet = "0123456789ABCDEF"
let byte_to_string c =
  let x = Char.code c in
  let fst = alphabet.[x / 16] in
  let snd = alphabet.[x mod 16] in
  String.init 2 (function
    | 0 -> fst
    | _ -> snd)

let length = List.length

let to_string bytes =
  let str = List.fold_left (fun acc x -> byte_to_string x ^ acc) "" bytes in
  "0x" ^ str

let to_int_list x = List.rev (List.map Char.code x)
