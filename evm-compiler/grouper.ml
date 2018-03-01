(**
 * See documentation in mli file.
 *)
module I = Instruct
module E = Evm

type t =
  | Evm of E.instr
  | Push_caml_code_offset of int
  | Goto of Label.t
  | Label of Label.t

(* Constants relevant to execution of EVM *)
module C = struct
  let word_size = 0x20L
  let stack_depth = 16 (* How deep instructions can index into the stack *)
  let acc_addr = 0L (* Address where acc is saved *)
  let heap_ctr_addr = 0x20L (* Address where heap counter is saved *)
  let extra_args_addr = 0x40L (* Address where extra args is saved *)
  let env_addr = 0x60L (* Address where env is saved *)
  let first_free_addr = 0x80L (* Initial value of heap counter *)

  let closure_tag = 1
end

(* List of EVM instructions, and length of Caml bytecode
 * that generated those instructions *)
type 'a group = {
  instrs : 'a list;
  caml_len : int;
}

(* Int represents the total caml length of all previous groups *)
type program = (t group * int) list

(*********
 * Convenience methods for common transformations of caml instructions
 * to evm instructions
 *)
let instrs (is : E.instr list) : t group = {
  instrs = List.map (fun i -> Evm i) is;
  caml_len = 1;
}

let to_string (p : program) : string =
  let instr_to_string = function
    | Evm e -> "\t" ^ E.instr_to_string e
    | Push_caml_code_offset i -> "\tofs " ^ string_of_int i
    | Goto lbl -> Printf.sprintf "\tgoto %d" (Label.to_int lbl)
    | Label lbl -> Printf.sprintf "\tlabel %d" (Label.to_int lbl)
  in
  let group_to_string (g, total_caml_len) : string =
    Printf.sprintf "Group (%d; total: %d):\n%s"
      g.caml_len total_caml_len
      (List.map instr_to_string g.instrs
        |> String.concat "\n")
  in
  String.concat "\n" (List.map group_to_string p)

(* Make header with given size and tag *)
let header ~size ~tag = Int64.of_int (size lsl 1 lor tag)

(* Conditional branches *)
let branch_cond vl ofs cond =
  let vl = Int64.of_int vl in
  E.{ instrs = [
      Evm (DUP 1);
      Evm (push vl);
    ] @ cond @ [
      Push_caml_code_offset (ofs-1);
      Evm JUMPI;
    ]; caml_len = 3; }

(* Allocate block of size given at top of stack.
 * Update heap ctr, and leave address at top of stack.
 *)
let alloc_dyn ~tag =
  let make_header = E.[
    Evm (DUP 1);
    Evm (push 2L);
    Evm MUL;
    Evm (push (Int64.of_int tag));
    Evm OR;
    Evm (push C.heap_ctr_addr);
    Evm MLOAD;
    Evm MSTORE;
  ] in

  let update_heap_ctr = E.[
    (* Calculate word*(n+1) for new heap ctr. *)
    Evm (push 1L);
    Evm ADD;
    Evm (push C.word_size);
    Evm MUL;

    (* Push heap_ctr onto stack twice, and add word*(n+1) to one of them *)
    Evm (push C.heap_ctr_addr);
    Evm MLOAD;
    Evm (DUP 1);
    Evm (SWAP 2);
    Evm ADD;

    Evm (push C.heap_ctr_addr);
    Evm MSTORE;

    (* Leave allocated address at top of stack *)
    Evm (push C.word_size);
    Evm ADD;
  ]

  in make_header @ update_heap_ctr

(* Allocate block of given size and tag, update heap ctr,
 * and leave address to block at top of stack *)
let alloc ~size ~tag =
  let make_header = E.[
    (* Create and store tag *)
    push (header ~size ~tag);
    push C.heap_ctr_addr;
    MLOAD;
    MSTORE;
  ] in

  (* Set heap ctr to new value, and leave ptr at top of stack *)
  let update_heap_ctr = E.[
    push C.heap_ctr_addr;
    MLOAD;
    DUP 1;
    push (Int64.of_int (32 * (size+1)));
    ADD;
    push C.heap_ctr_addr;
    MSTORE;

    (* Add word size to old heap counter so that it points to the first entry
     * in the block (and not the tag) *)
    push C.word_size;
    ADD;
  ] in

  make_header @ update_heap_ctr

(* Makeblock instruction with various arguments *)
let instr_makeblock ~size ~tag ~caml_len : t group =
  let range = Util.range ~lo:0 ~hi:size in
  let make_body = Util.concat_map range ~f:(fun i -> E.[
    SWAP 1;
    DUP 2;
    push (Int64.of_int (32 * i));
    ADD;
    MSTORE;
  ]) in

  { (instrs (alloc ~size ~tag @ make_body))
      with caml_len }

(* Apply instruction with various arguments *)
let instr_apply (n : int) : t group =
  let lbl = Label.create () in

  (* Figure out where to store arguments on heap *)
  let setup = E.[
    (* Save acc on heap for now, it simplifies the logic *)
    Evm (push C.acc_addr);
    Evm MSTORE;
    Evm (push C.heap_ctr_addr);
    Evm MLOAD;
  ] in

  (* Store all n arguments on the heap *)
  let store =
    Util.range ~lo:0 ~hi:n
    |> Util.concat_map ~f:(fun i -> E.[
        Evm (SWAP 1); (* Get top item of stack (skipping address) *)
        Evm (DUP 2); (* Clone previous address *)
        Evm MSTORE;

        (* Increase address at top of stack by 32 *)
        Evm (push C.word_size);
        Evm ADD;
      ])
  in

  (* Push extra_args, environment, and pc *)
  let push_instrs = E.[
    Evm POP;

    Evm (push C.extra_args_addr);
    Evm MLOAD;

    Evm (push C.env_addr);
    Evm MLOAD;

    Goto lbl; (* Push address of label onto stack *)
  ] in

  (* For restoring heap items to stack, we start from highest address *)
  let restore_setup =
    let x = 0x20 * (n-1) |> Int64.of_int in E.[
      Evm (push C.heap_ctr_addr);
      Evm MLOAD;
      Evm (push x);
      Evm ADD;
    ] in

  let restore_stack =
    Util.range ~lo:0 ~hi:n
    |> Util.concat_map ~f:(fun i -> E.[
        Evm (DUP 1);
        Evm MLOAD;
        Evm (SWAP 1);

        (* Decrease address at top of stack by word size *)
        Evm (push C.word_size);
        Evm (SWAP 1);
        Evm SUB;
      ])
  in

  let restore_acc = E.[
    Evm POP; (* Remove heap ctr from top of stack *)
    Evm (push C.acc_addr);
    Evm MLOAD;
  ] in

  (* We now set up values of env and extra_args *)
  let enter_function_call = E.[

    (* Set new value of env and extra_args *)
    Evm (DUP 1);
    Evm (push C.env_addr);
    Evm MSTORE;
    Evm (push (Int64.of_int (n-1)));
    Evm (push C.extra_args_addr);
    Evm MSTORE;

    (* JUMP *)
    Evm (DUP 1);
    Evm MLOAD; (* access code value of accumulator *)
    Evm JUMP;

    (* We need to store the pc for this instruction *)
    Label lbl;
  ] in {
    instrs = setup
           @ store
           @ push_instrs
           @ restore_setup
           @ restore_stack
           @ restore_acc
           @ enter_function_call;
    caml_len = 1 }

let convert (p : int array) : program =
  (* Convert one group of instructions *)
  let consume (i : int) : t group =
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

    (* Caml: (params ofs, val) increment pc by ofs-1 if val is not equal to the
     *  accumulator.
     * EVM: Same thing.
     *)
    | I.BEQ ->
        branch_cond p.(i+1) p.(i+2) E.[ Evm EQ; ]
    | I.BNEQ ->
        branch_cond p.(i+1) p.(i+2) E.[ Evm EQ; Evm ISZERO; ]
    | I.BLTINT ->
        branch_cond p.(i+1) p.(i+2) E.[ Evm LT; ]
    | I.BGEINT ->
        branch_cond p.(i+1) p.(i+2) E.[ Evm LT; Evm ISZERO; ]
    | I.BGTINT ->
        branch_cond p.(i+1) p.(i+2) E.[ Evm GT; ]
    | I.BLEINT ->
        branch_cond p.(i+1) p.(i+2) E.[ Evm GT; Evm ISZERO; ]

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
        let is = match p.(i+1) with
          | 1 -> E.[ SWAP 1; POP; ]
          | n ->
            let setup = E.[ push C.acc_addr; MSTORE; ] in
            let body = Util.tabulate ~f:(fun _ -> E.POP) n in
            let teardown = E.[ push C.acc_addr; MLOAD; ] in
            setup @ body @ teardown
        in
        { (instrs is) with caml_len = 2 }

   (* Caml: Branch to pc + offset (under certain conditions)
    * EVM: Branch to absolute pc (under certain conditions)
    *)
    | I.BRANCH ->
        let ofs = p.(i+1) in
        { instrs = E.[ Push_caml_code_offset (ofs-1);
                       Evm JUMP; ];
          caml_len = 2; }
    | I.BRANCHIF ->
        let ofs = p.(i+1) in
        { instrs = E.[ Evm (DUP 1);
                       Push_caml_code_offset (ofs-1);
                       Evm JUMPI; ];
          caml_len = 2; }
    | I.BRANCHIFNOT ->
        let ofs = p.(i+1) in
        { instrs = E.[ Evm (DUP 1);
                       Evm ISZERO;
                       Push_caml_code_offset (ofs-1);
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

    (* Caml: Pop n elements from the stack.
     *       Check value of extraArgs. If it's positive, then the
     *       value is decremnted, and pc jumps to the value of
     *       the accumulator. Otherwise, three values from the
     *       stack are used for pc, environment, and extraArgs.
     *)
    | I.RETURN ->
        let n = p.(i+1) in

        (* Store acc on heap for now... *)
        let store_acc = E.[
          Evm (push C.acc_addr);
          Evm MSTORE;
        ] in

        let pops = Util.tabulate ~f:(fun _ -> Evm E.POP) n in
        let label = Label.create () in
        let instrs = E.[
          Evm (push C.extra_args_addr);
          Evm MLOAD;

          Goto label;
          Evm JUMPI;

          (* Here, extra_args was 0 *)
          (* Stack:  pc :: env :: extraArgs :: ... *)
          Evm (SWAP 2);
          Evm (push C.extra_args_addr);
          Evm MSTORE;

          (* Stack: env :: pc :: ... *)
          Evm (push C.env_addr);
          Evm MSTORE;

          (* Restore acc behind pc *)
          Evm (push C.acc_addr);
          Evm MLOAD;
          Evm (SWAP 1);

          (* Stack: pc :: ... *)
          Evm JUMP;

          (* Here, the extra_args > 0 *)
          Label label;

          (* Restore acc *)
          Evm (push C.acc_addr);
          Evm MLOAD;

          (* Decrement extra_args *)
          Evm (push 1L);
          Evm (push C.extra_args_addr);
          Evm MLOAD;
          Evm SUB;
          Evm (push C.extra_args_addr);
          Evm MSTORE;

          (* Store acc in env *)
          Evm (DUP 1);
          Evm (push C.env_addr);
          Evm MLOAD;

          (* Jump to code value of acc *)
          Evm (DUP 1);
          Evm MLOAD;
          Evm JUMP;
        ] in

        { instrs = store_acc
                 @ pops
                 @ instrs;
                 caml_len = 2; }


    (* Caml: Check value of n. If it's greater than 0, then the
     *   accumulator is put onto the stack. A closure of n+1 elements
     *   is created in the accumulator. The code value is set to
     *   pc + ofs, and the other elements are set to values popped
     *   from the stack.
     *
     * EVM: Same, except we can just push the caml code offset to
     * the stack.
     *)
    | I.CLOSURE ->
        let n = p.(i+1) in
        let ofs = p.(i+2) in

        (* Optionally keep acc *)
        let setup =
          if n > 0 then [] else E.[ Evm POP ]
        in

        (* Perform allocation of closure *)
        let do_alloc =
          alloc ~size:(n+1) ~tag:C.closure_tag
            |> List.map (fun i -> Evm i)
        in

        (* Store code_val in memory *)
        let code_val = E.[
          Push_caml_code_offset (ofs-1);
          Evm (DUP 2);
          Evm MSTORE;
        ] in

        (* Store each member of stack in memory *)
        let move_from_stack =
          Util.range ~lo:1 ~hi:(n+1)
            |> Util.concat_map ~f:(fun i -> E.[
              Evm (SWAP 1);
              Evm (DUP 2);
              Evm (push (Int64.of_int (0x20 * i)));
              Evm ADD;
              Evm MSTORE;
            ]) in

        { instrs = setup @ do_alloc @ code_val @ move_from_stack;
          caml_len = 3; }

    | I.APPLY ->
        let args = p.(i+1) in
        { caml_len = 2;
          instrs = E.[
            (* Store args-1 as extra_args *)
            Evm (push (args - 1 |> Int64.of_int));
            Evm (push C.extra_args_addr);
            Evm MSTORE;

            (* Set environment to the value of the accumulator *)
            Evm (DUP 1);
            Evm (push C.acc_addr);
            Evm MSTORE;

            (* Set pc to code value of accumulator *)
            Evm (DUP 1);
            Evm MLOAD;
            Evm JUMP;
          ]; }

    | I.APPLY1 -> instr_apply 1
    | I.APPLY2 -> instr_apply 2
    | I.APPLY3 -> instr_apply 3

    (* Used in closures *)
    | I.RESTART ->
        let exit = Label.create () in
        let loop = Label.create () in {
          caml_len = 1;
          instrs = E.[
            (* Save acc *)
            Evm (push C.acc_addr);
            Evm MSTORE;

            Evm (push 2L); (* Subtract 2 later *)
            Evm (push 2L); (* Divide by 2 later *)

            (* Compute size of environment, which is stored in the header
             * of the environment. *)
            Evm (push C.word_size);
            Evm (push C.env_addr);
            Evm MLOAD;
            Evm SUB;
            Evm MLOAD;

            (* Shift by 1 to get the size bits of the header *)
            Evm DIV;
            (* Calculate n as |env| - 2 *)
            Evm SUB;

            (* Increase extra_args by n *)
            Evm (DUP 1);
            Evm (push C.extra_args_addr);
            Evm MLOAD;
            Evm ADD;
            Evm (push C.extra_args_addr);
            Evm MSTORE;

            (* Place word_size*(n+1) at top of stack *)
            Evm (push 1L);
            Evm ADD;
            Evm (push C.word_size);
            Evm MUL;

            (* Create a loop to place elements n+1 to 2 on stack *)
            Label loop;

            (* Exit loop if we are below 2 elements on stack *)
            Evm (DUP 1);
            Evm (push Int64.(mul C.word_size 2L));
            Evm GT;
            Goto exit;
            Evm JUMPI;

            (* Push heap element onto stack and update index *)
            Evm (DUP 1);
            Evm (push C.word_size);
            Evm (SWAP 1);
            Evm (push C.env_addr);
            Evm MLOAD;
            Evm ADD;
            Evm MLOAD;
            Evm (SWAP 2);
            Evm SUB;

            Goto loop;
            Evm JUMP;
            Label exit;

            (* We use last index left on the stack to load the new environment *)
            Evm MLOAD;
            Evm (push C.env_addr);
            Evm MSTORE;

            (* Restore acc *)
            Evm (push C.acc_addr);
            Evm MLOAD;
          ];
        }

    | I.GRAB ->
        let n = p.(i+1) in
        let lbl = Label.create () in
        let exit = Label.create () in

        (* Loop for iterating extra_args many times *)
        let loop = Label.create () in

        let instrs = E.[
          (* If extra_args >= n, *)
          Evm (push C.extra_args_addr);
          Evm MLOAD;
          Evm (DUP 1); (* Place an extra extra_args on the stack
                        * to be consumed after the jump *)
          Evm (push (Int64.of_int n));
          Evm (SWAP 1);
          Evm LT;
          Goto lbl;
          Evm JUMPI;

          (* then decrement extra_args by n, and that's it. *)
          Evm (push (Int64.of_int n));
          Evm (SWAP 1);
          Evm SUB;
          Evm (push C.extra_args_addr);
          Evm MSTORE;
          Goto exit;
          Evm JUMP;

          (* Otherwise, create a closure of extra_args + 3 elements,
           * after popping the former acc value. *)
          Label lbl;
          Evm (SWAP 1);
          Evm POP;
          Evm (push 3L);
          Evm ADD;

        (* Allocate a closure *)
        ] @ alloc_dyn ~tag:C.closure_tag @ E.[
          (* Code value is PC - 3 *)
          Push_caml_code_offset (-3);
          Evm (DUP 2);
          Evm MSTORE;

          (* index 1 is env *)
          Evm (push C.env_addr);
          Evm MLOAD;
          Evm (DUP 2);
          Evm (push C.word_size);
          Evm ADD;
          Evm MSTORE;

          (* Increment original address by 2 words to start storing stack items *)
          Evm (push Int64.(mul 2L C.word_size));
          Evm ADD;

          (* Put extra_args+1 on the stack as a loop counter *)
          Evm (push C.extra_args_addr);
          Evm MLOAD;
          Evm (push 1L);
          Evm ADD;

          (* It's a do-while loop, since extra_args+1 > 0 *)
          Label loop;
          (* loop_ctr :: addr :: item :: ... *)
          Evm (SWAP 2);
          (* item :: addr :: loop_ctr :: ... *)
          Evm (DUP 2);
          (* addr :: item :: addr :: loop_ctr :: ... *)
          Evm MSTORE;
          (* addr :: loop_ctr :: ... *)

          Evm (push C.word_size);
          Evm ADD;
          (* addr' :: loop_ctr :: ... *)
          Evm (SWAP 1);
          Evm (push 1L);
          Evm (SWAP 1);
          Evm SUB;
          (* loop_ctr' :: addr' :: ... *)

          (* Goto loop if loop_ctr' is not 0 *)
          Evm (DUP 1);
          Goto loop;
          Evm JUMPI;

          Evm POP;
          (* addr :: ... *)

          (* We now subtract word_size*(extra_args+3) from the addr on the
           * top of the stack *)
          Evm (push 3L);
          Evm (push C.extra_args_addr);
          Evm MLOAD;
          Evm ADD;
          Evm (push C.word_size);
          Evm MUL;

          Evm (SWAP 1);
          Evm SUB;

          (* Finally, pop pc, environment, and extra_args from top of stack *)
          (* addr :: pc :: env :: extra_args *)
          Evm (SWAP 3);
          (* extra_args :: pc :: env :: addr *)
          Evm (push C.extra_args_addr);
          Evm MSTORE;
          (* pc :: env :: addr *)
          Evm (SWAP 1);
          Evm (push C.env_addr);
          Evm MSTORE;
          (* pc :: addr *)
          Evm JUMP;

          (* Exit label from the if branch a long time ago... *)
          Label exit;
        ]

        in { instrs; caml_len = 2; }

    | i -> Printf.printf "Unimplemented opcode: %d\n" (I.to_opcode i);
           assert false
  in

  let rec loop (i : int) (acc : program) : program =
    if i >= Array.length p then List.rev acc
    else
      let group = consume i in
      loop (i + group.caml_len) ((group, i) :: acc)
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
  | [] -> [
    ({ caml_len = 0; instrs = setup_instrs }, 0) ]
  | [ (g, i) ] ->
      [ ({ g with instrs = setup_instrs @ g.instrs }, i) ]
  | (g, i) :: gs ->
      ({ g with instrs = setup_instrs @ g.instrs  }, i) :: gs
