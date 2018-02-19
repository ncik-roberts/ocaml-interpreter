(**
 * See documentation in mli file.
 *)
module I = Instruct
module E = Evm

type t =
  | Evm of E.instr
  | Push_caml_code_offset of int

(* Constants relevant to execution of EVM *)
module C = struct
  let stack_depth = 16 (* How deep instructions can index into the stack *)
  let acc_addr = 0L (* Address where acc is saved *)
  let heap_ctr_addr = 32L (* Address where heap counter is saved *)
  let first_free_addr = 64L (* Initial value of heap counter *)
end

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

(* Make header with given size and tag *)
let header ~size ~tag = Int64.of_int (size lsl 1 land tag)

let instr_makeblock ~size ~tag ~caml_len =
  let make_header = E.[
    (* Create and store tag *)
    push (header ~size ~tag);
    push C.heap_ctr_addr;
    MLOAD;
    MSTORE;
  ] in

  let range = Util.range ~lo:1 ~hi:(size+1) in
  let make_body = Util.concat_map range ~f:(fun i -> E.[
    push C.heap_ctr_addr;
    MLOAD;
    push (Int64.of_int (32 * i));
    ADD;
    MSTORE;
  ]) in

  (* Add size+1 to heap counter, and store old heap counter at top of stack *)
  let update_heap_ctr = E.[
    push C.heap_ctr_addr;
    DUP 1;
    MLOAD;
    push (Int64.of_int (32 * (size+1)));
    ADD;
    push C.heap_ctr_addr;
    MSTORE;

    (* Add 32 to old heap counter so that it points to the first entry
     * in the block (and not the tag) *)
    push 0x20L;
    ADD;
  ] in

  { (instrs (make_header @ make_body @ update_heap_ctr))
      with caml_len }

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

    | I.OFFSETINT ->
        let n = Int64.of_int p.(i + 1) in
        { (instrs E.[ push n; ADD; ]) with caml_len = 2 }

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

    (* Caml: Put accumulator onto stack and then peek (i+1)th element of stack
     *   into accumulator.
     * EVM: Duplicate (i+1)th element of stack.
     *)
    | I.PUSH | I.PUSHACC0 -> instrs E.[ DUP 1 ]
    | I.PUSHACC1 -> instrs E.[ DUP 2 ]
    | I.PUSHACC2 -> instrs E.[ DUP 3 ]
    | I.PUSHACC3 -> instrs E.[ DUP 4 ]
    | I.PUSHACC4 -> instrs E.[ DUP 5 ]
    | I.PUSHACC5 -> instrs E.[ DUP 6 ]
    | I.PUSHACC6 -> instrs E.[ DUP 7 ]
    | I.PUSHACC7 -> instrs E.[ DUP 8 ]
    | I.PUSHACC ->
        let n = p.(i+1) + 1 in
        if n > C.stack_depth
        then failwith "Stack limit exceeded in PUSHACC instruction"
        else { (instrs E.[ DUP n ]) with caml_len = 2 }

    (* Caml: Replace accumulator with ith element of stack.
     * EVM: Pop top of stack and duplicate ith element of stack.
     *)
    | I.ACC0 -> instrs E.[ POP; DUP 1; ]
    | I.ACC1 -> instrs E.[ POP; DUP 2; ]
    | I.ACC2 -> instrs E.[ POP; DUP 3; ]
    | I.ACC3 -> instrs E.[ POP; DUP 4; ]
    | I.ACC4 -> instrs E.[ POP; DUP 5; ]
    | I.ACC5 -> instrs E.[ POP; DUP 6; ]
    | I.ACC6 -> instrs E.[ POP; DUP 7; ]
    | I.ACC7 -> instrs E.[ POP; DUP 8; ]
    | I.ACC ->
        let n = p.(i+1) + 1 in
        if n > C.stack_depth
        then failwith "Stack limit exceeded in ACC instruction"
        else { (instrs E.[ POP; DUP n; ]) with caml_len = 2 }

    (* Caml: Pop n items from the stack.
     * EVM: Place top of stack in designated "save" location. Pop n items from stack.
     *   Restore top of stack from "save" location.
     *)
    | I.POP ->
        let n = p.(i+1) in
        let setup = E.[ push C.acc_addr; MSTORE; ] in
        let body = Util.tabulate ~f:(fun _ -> E.POP) n in
        let teardown = E.[ push C.acc_addr; MLOAD; ] in
        { (instrs (setup @ body @ teardown)) with caml_len = 2 }

   (* Caml: Branch to pc + offset (under certain conditions)
    * EVM: Branch to absolute pc (under certain conditions)
    *
    * We add 1 to offset because, in Caml, the offset is calculated with
    * respect to the byte after the branch instruction. For our purposes,
    * it will be easier to calculate the offset with respect to the
    * branch instruction.
    *)
    | I.BRANCH ->
        let ofs = p.(i+1) in
        { instrs = E.[ Push_caml_code_offset (ofs+1);
                       Evm JUMP; ];
          caml_len = 2; }
    | I.BRANCHIF ->
        let ofs = p.(i+1) in
        { instrs = E.[ Push_caml_code_offset (ofs+1);
                       Evm JUMPI; ];
          caml_len = 2; }
    | I.BRANCHIFNOT ->
        let ofs = p.(i+1) in
        { instrs = E.[ Evm ISZERO;
                       Push_caml_code_offset (ofs+1);
                       Evm JUMPI; ];
          caml_len = 2; }

    (* Caml: Allocate block on heap with given size and tag, and place
     *   elements popped off stack into heap.
     * EVM: Same semantics (modulo the accumulator).
     *)
    | I.MAKEBLOCK ->
        instr_makeblock ~size:p.(i+1) ~tag:p.(i+2) ~caml_len:3
    | I.MAKEBLOCK1 ->
        instr_makeblock ~size:1 ~tag:p.(i+1) ~caml_len:2
    | I.MAKEBLOCK2 ->
        instr_makeblock ~size:2 ~tag:p.(i+1) ~caml_len:2
    | I.MAKEBLOCK3 ->
        instr_makeblock ~size:3 ~tag:p.(i+1) ~caml_len:2
  in

  let rec loop (i : int) (acc : program) : program =
    if i >= Array.length p then List.rev acc
    else
      let group = consume i in
      loop (i + group.caml_len) (group :: acc)

  in

  (* - Initialize accumulator to 0;
   * - Initialize heap counter to first free element;
   *)
  let setup_instrs = [
    Evm (E.push 0L);
    Evm (E.push C.first_free_addr);
    Evm (E.push C.heap_ctr_addr);
    Evm (E.MSTORE);
  ] in

  (* Add initial setup code: need to push 0 to accumulator at first *)
  match loop 0 [] with
  | [] -> [ { caml_len = 0; instrs = setup_instrs } ]
  | g :: gs -> { g with instrs = setup_instrs @ g.instrs } :: gs
