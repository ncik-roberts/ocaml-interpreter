type program = Evm.program

(* Number of bytes required to copy code *)
let offset = 12L

let copy_code program =
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
