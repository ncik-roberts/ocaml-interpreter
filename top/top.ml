exception Exit

let ocamlc = "~/blockchain/ocaml-compiler/ocaml/ocamlc"

let cmo (ml : string) : string =
  let n = String.length ml in
  String.sub ml 0 (n-2) ^ "cmo"

let flip f x y = f y x

(* Call f four times *)
let repeat4 f =
  let x1 = f () in
  let x2 = f () in
  let x3 = f () in
  let x4 = f () in
  (x1, x2, x3, x4)

let main () =
  let filename = match Array.length Sys.argv with
    | 2 -> Sys.argv.(1)
    | _ -> prerr_endline "Expected: 1 file argument";
           raise Exit
  in

  Unix.system (ocamlc ^ " -dinstr " ^ filename) |> ignore;

  let cmo_filename = cmo filename in
  let cmo_file = open_in cmo_filename in

  (* Skip 16 bytes of file *)
  seek_in cmo_file 16;

  (* Read bytes until reaching the magic sequence *)
  let rec read acc =
    let (b1, b2, b3, b4) = repeat4 (fun () -> input_byte cmo_file) in
    let x = (b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1 in
    if x = 0xBEA69584 then acc
    else read (x :: acc)
  in

  let result = read []
    |> flip List_util.drop_n 2
    |> List.rev
  in
  close_in cmo_file;

  let output = result
    |> Array.of_list
    |> Grouper.convert
    |> (fun x -> Printf.printf "%s\n" (Grouper.to_string x); x)
    |> Jump_dests.process
    |> Copy_code.copy_code
    |> Copy_code.to_evm
  in

  Printf.printf "Program:\n";
  Printf.printf "%s\n\n" (Evm.to_string output);

  Printf.printf "Bytecode:\n";
  Printf.printf "0x%s\n" (Evm.to_hex_string output)

let _ = main ()
