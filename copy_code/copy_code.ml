type program = Evm.program

(* Number of bytes required to copy code *)
let offset = 12L

let copy_code program ~return_size =
  (* Copy return_size many fields from block at top of heap *)
  let teardown = Evm.[
    push (Int64.of_int (return_size * 0x20));
    SWAP 1;
    RETURN;
  ] in

  let program = program @ teardown in

  (* We must know how long the program is to copy it to the contract *)
  let len = List.length (Evm.to_bytes program)
    |> Int64.of_int

  in Evm.[
    push len;
    push offset;
    push 0L;
    CODECOPY;
    push len;
    push 0L;
    RETURN;
  ] @ program

let to_evm program = program
