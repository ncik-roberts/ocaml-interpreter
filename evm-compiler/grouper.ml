(**
 * See documentation in mli file.
 *)
module I = Instruct
module E = Evm

type t =
  | Evm of E.instr
  | Const of int
  | Push_caml_code_offset of int

let stack_depth = 16

(* List of EVM instructions, and length of Caml bytecode
 * that generated those instructions *)
type group = {
  instrs : t list;
  caml_len : int
}

type program = group list

(*********
 * Convenience methods for common transformations of caml instructions
 * to evm instructions
 *)
let instrs (is : E.instr list) : group = {
  instrs = List.map (fun i -> Evm i) is;
  caml_len = 1;
}

let convert (p : int array) : program =
  (* Convert one group of instructions *)
  let consume (i : int) : group =
    match I.of_opcode p.(i) with

    (* Caml: Binary operations pop top of stack and add to the accumulator.
     * EVM: Binary operations add top two elements of stack.
     *)
    | I.ADDINT -> instrs [ E.ADD ]
    | I.SUBINT -> instrs [ E.SUB ]
    | I.MULINT -> instrs [ E.MUL ]
    | I.DIVINT -> instrs [ E.DIV ]
    | I.MODINT -> instrs [ E.MOD ]
    | I.ANDINT -> instrs [ E.AND ]
    | I.XORINT -> instrs [ E.XOR ]
    | I.ORINT  -> instrs [ E.OR ]
    | I.LSLINT -> instrs [
        E.SWAP 1;
        E.push 2L;
        E.EXP;
        E.MUL;
      ]
    (* TODO: Work out semantics *)
    | I.LSRINT -> assert false
    | I.ASRINT -> assert false

    (* Caml: Comparison operators pop top of stack and compare with the
     *   accumulator, storing the result value in accumulator.
     * EVM: Comparison operators compare top two elements on stack, storing
     *   0 or 1 at top of stack
     *)
    | I.EQ -> instrs E.[ EQ ]
    | I.NEQ -> instrs E.[ EQ; ISZERO; ]
    (* TODO: fixed signed operations. Hint: use SIGNEXTEND. *)
    | I.LEINT -> instrs E.[ GT; ISZERO; ]
    | I.LTINT -> instrs E.[ LT ]
    | I.GEINT -> instrs E.[ LT; ISZERO; ]
    | I.GTINT -> instrs E.[ GT ]

    (* Caml: Store a new value in accumulator, discarding old value.
     * EVM: Pop accumulator from stack and push constant.
     *)
    | I.CONST0 -> instrs E.[ POP; push 0L; ]
    | I.CONST1 -> instrs E.[ POP; push 1L; ]
    | I.CONST2 -> instrs E.[ POP; push 2L; ]
    | I.CONST3 -> instrs E.[ POP; push 3L; ]
    | I.CONSTINT ->
        let n = Int64.of_int p.(i+1) in
        { (instrs E.[ POP; push n; ]) with caml_len = 2 }

    (* Caml: Push the accumulator onto the stack and set the accumulator to
     *   a specified value.
     * EVM: Push specified value onto stack.
     *)
    | I.PUSHCONST0 -> instrs E.[ push 0L; ]
    | I.PUSHCONST1 -> instrs E.[ push 1L; ]
    | I.PUSHCONST2 -> instrs E.[ push 2L; ]
    | I.PUSHCONST3 -> instrs E.[ push 3L; ]
    | I.PUSHCONSTINT ->
        let n = Int64.of_int p.(i+1) in
        { (instrs E.[ push n; ]) with caml_len = 2 }

    (* Caml: Put accumulator onto stack and then peek (i+1)th element of stack
     *   into accumulator.
     * EVM: Duplicate (i+1)th element of stack.
     *)
    | I.PUSHACC0 -> instrs E.[ DUP 0 ]
    | I.PUSHACC1 -> instrs E.[ DUP 1 ]
    | I.PUSHACC2 -> instrs E.[ DUP 2 ]
    | I.PUSHACC3 -> instrs E.[ DUP 3 ]
    | I.PUSHACC4 -> instrs E.[ DUP 4 ]
    | I.PUSHACC5 -> instrs E.[ DUP 5 ]
    | I.PUSHACC6 -> instrs E.[ DUP 6 ]
    | I.PUSHACC7 -> instrs E.[ DUP 7 ]
    | I.PUSHACC ->
        let n = p.(i+1) + 1 in
        if n > stack_depth then failwith "Stack limit exceeded in PUSHACC instruction"
        else { (instrs E.[ DUP n ]) with caml_len = 2 }

    (* Caml: Replace accumulator with ith element of stack.
     * EVM: Pop top of stack and duplicate ith element of stack.
     *)
    | I.ACC0 -> instrs E.[ POP; DUP 0; ]
    | I.ACC1 -> instrs E.[ POP; DUP 1; ]
    | I.ACC2 -> instrs E.[ POP; DUP 2; ]
    | I.ACC3 -> instrs E.[ POP; DUP 3; ]
    | I.ACC4 -> instrs E.[ POP; DUP 4; ]
    | I.ACC5 -> instrs E.[ POP; DUP 5; ]
    | I.ACC6 -> instrs E.[ POP; DUP 6; ]
    | I.ACC7 -> instrs E.[ POP; DUP 7; ]
    | I.ACC ->
        let n = p.(i+1) in
        if n > stack_depth then failwith "Stack limit exceeded in ACC instruction"
        else { (instrs E.[ POP; DUP n; ]) with caml_len = 2 }
  in
  consume |> ignore;
  []
