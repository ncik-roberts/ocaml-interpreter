open Instruct

module Block_tag = struct
  type tag =
    | Closure

  type t = tag option

  let from_int = function
    | 0 -> None
    | 247 -> Some Closure
    | _ -> assert false

  let to_string = function
    | None -> "none"
    | Some Closure -> "closure"
end

module Value = struct
  type block = {
    size : int; (* Number of fields *)
    tag : Block_tag.t; (* Type of block *)
    addr : int; (* Index into heap *)
  }

  type t =
    | Long of int
    | Block of block

  let to_string = function
    | Long i -> Printf.sprintf "Long %d" i
    | Block { size; tag; addr } ->
        Printf.sprintf "Block size=%d tag=%s addr=%d"
          size (Block_tag.to_string tag) addr

  let extract_long = function
    | Long i -> i
    | _ -> assert false
end

open Value

type state = {
  stack : Value.t list;
  acc : Value.t;
  pc : int;
  heap : Value.t Heap.t;
  globals : Value.t list;
  extra_args : int;
  env : Value.block;
}

(* 4 byte opcodes/values *)
type program = int array

(* Change return type of a function *)
let lift_bool f x y = if f x y then 1 else 0

let run (p : program) : state =

  (* The execution is finished only when the pc has reached the
   * end of the program. *)
  let is_final (st : state) : bool =
    st.pc >= Array.length p || p.(st.pc) = to_opcode STOP
  in

  (* Steps from one state to the next.
   *   Note: mutable fields of state are shared between the argument
   *   and the return value.
   *)
  let step (st : state) : state =

    (* De-block value *)
    let block (v : Value.t) : Value.block = match v with
      | Block block -> block
      | _ -> assert false in

    (* Code value of the given value using the current heap *)
    let code_value (v : Value.t) : int =
      let b = block v in
      match Heap.lookup st.heap b.addr b.size with
        | Long code_value :: _ -> code_value
        | _ -> assert false in

    (* CONST<i> *)
    let step_const (i : int) : state =
      { st with pc = st.pc + 1; acc = Long i; } in

    (* PUSHACC<i> *)
    let step_pushacc (i : int) : state =
      let stack' = st.acc :: st.stack in
      { st with pc = st.pc + 1;
                acc = List.nth stack' i;
                stack = stack'; } in

    (* ACC<i> *)
    let step_acc (i : int) : state =
      { st with pc = st.pc + 1;
                acc = List.nth st.stack i; } in

    (* APPLY<i> *)
    let step_apply (i : int) : state =
      let (args, stack') = List_util.split_n st.stack i in
      let stack'' = args
        @ [ Long (st.pc + 1); Block st.env; Long st.extra_args; ]
        @ stack' in

      { st with pc = code_value st.acc;
                env = block st.acc;
                extra_args = 0;
                stack = stack''; } in

    (* MAKEBLOCK<i> with tag <t> *)
    let step_makeblock (i : int) (tag_int : int) : state =
      (* Only remove i-1 elements from stack since acc is first element
       * of block. *)
      let (fields, stack') = List_util.split_n st.stack (i-1) in
      let tag = Block_tag.from_int tag_int in
      let (addr, heap') = Heap.alloc st.heap (st.acc :: fields) in
      let block = Block { size = i; tag; addr; } in
      { st with pc = st.pc + 2;
                acc = block;
                stack = stack';
                heap = heap'; } in

    (* Apply the binop to the acc and the first element of the stack, pushing
     * result onto stack. *)
    let step_binop (binop : int -> int -> int) : state =
      let op1 = extract_long st.acc in
      let op2 = match st.stack with
        | v :: _ -> extract_long v
        | _ -> assert false in
      { st with pc = st.pc + 1;
                acc = Long (binop op1 op2);
                stack = List_util.drop_n st.stack 1; } in

    (* Apply unop to member of stack *)
    let step_unop (unop : int -> int) : state =
      let i = extract_long st.acc in
      { st with pc = st.pc + 1;
                acc = Long (unop i); } in

    (* Jump by the parameter at pc + 1 *)
    let step_branch (cond : bool) : state =
      if cond then
        let ofs = p.(st.pc + 1) in
        { st with pc = st.pc + ofs }
      else { st with pc = st.pc + 2 }
    in

    (* PUSHCONST<i> *)
    let step_pushconst (i : int) : state =
      { st with pc = st.pc + 1;
                acc = Long i;
                stack = st.acc :: st.stack; } in

    match of_opcode p.(st.pc) with
    (* Push constant item onto stack *)
    | CONST0 -> step_const 0
    | CONST1 -> step_const 1
    | CONST2 -> step_const 2
    | CONST3 -> step_const 3
    | CONSTINT ->
        let i = p.(st.pc + 1) in
        { (step_const i) with pc = st.pc + 2 }

    (* Push acc onto stack *)
    | PUSH | PUSHACC0 -> step_pushacc 0
    | PUSHACC1 -> step_pushacc 1
    | PUSHACC2 -> step_pushacc 2
    | PUSHACC3 -> step_pushacc 3
    | PUSHACC4 -> step_pushacc 4
    | PUSHACC5 -> step_pushacc 5
    | PUSHACC6 -> step_pushacc 6
    | PUSHACC7 -> step_pushacc 7
    | PUSHACC ->
        let i = p.(st.pc + 1) in
        { (step_pushacc i) with pc = st.pc + 2 }

    (* Peek the stack into acc *)
    | ACC0 -> step_acc 0
    | ACC1 -> step_acc 1
    | ACC2 -> step_acc 2
    | ACC3 -> step_acc 3
    | ACC4 -> step_acc 4
    | ACC5 -> step_acc 5
    | ACC6 -> step_acc 6
    | ACC7 -> step_acc 7
    | ACC ->
        let i = p.(st.pc + 1) in
        { (step_acc i) with pc = st.pc + 2 }

    (* PUSH followed by CONST *)
    | PUSHCONST0 -> step_pushconst 0
    | PUSHCONST1 -> step_pushconst 1
    | PUSHCONST2 -> step_pushconst 2
    | PUSHCONST3 -> step_pushconst 3
    | PUSHCONSTINT ->
        let i = p.(st.pc + 1) in
        { (step_pushconst i) with pc = st.pc + 2 }

    (* Allocate a block *)
    | MAKEBLOCK1 -> step_makeblock 1 p.(st.pc + 1)
    | MAKEBLOCK2 -> step_makeblock 2 p.(st.pc + 1)
    | MAKEBLOCK3 -> step_makeblock 3 p.(st.pc + 1)
    | MAKEBLOCK ->
        let n = p.(st.pc + 1) in
        let t = p.(st.pc + 2) in
        { (step_makeblock n t) with pc = st.pc + 3 }

    | POP ->
        let n = p.(st.pc + 1) in
        let stack' = List_util.drop_n st.stack n in
        { st with pc = st.pc + 2;
                  stack = stack'; }

    (* Perform arithmetic on int *)
    | ADDINT -> step_binop (+)
    | SUBINT -> step_binop (-)
    | MULINT -> step_binop ( * )
    | DIVINT -> step_binop (/)
    | MODINT -> step_binop (mod)
    | NEGINT -> step_unop (~-)
    | ANDINT -> step_binop (land)
    | ORINT -> step_binop (lor)
    | XORINT -> step_binop (lxor)
    | LSLINT -> step_binop (lsl)
    | LSRINT -> step_binop (lsr)
    | ASRINT -> step_binop (asr)
    | OFFSETINT ->
        let ofs = p.(st.pc + 1) in
        { (step_unop (fun i -> i + ofs))
            with pc = st.pc + 2 }

    (* Perform comparisons on int *)
    | EQ -> step_binop (lift_bool (=))
    | LTINT -> step_binop (lift_bool (<))
    | LEINT -> step_binop (lift_bool (<=))
    | GTINT -> step_binop (lift_bool (>))
    | GEINT -> step_binop (lift_bool (>=))

    (* Jumps, conditional or otherwise *)
    | BRANCH -> step_branch true
    | BRANCHIF -> step_branch (st.acc <> Long 0)
    | BRANCHIFNOT -> step_branch (st.acc = Long 0)

    (* Create closure *)
    | CLOSURE ->
        let n = p.(st.pc + 1) in
        let ofs = p.(st.pc + 2) in
        let stack' = if n > 0 then st.acc :: st.stack
                     else st.stack in
        let (fields, stack'') = List_util.split_n stack' n in

        (* Push code value onto fields *)
        let fields' = Long (st.pc + ofs) :: fields in
        let (addr, heap') = Heap.alloc st.heap fields' in

        { st with
            acc = Block { size = n+1;
                          tag = Some Block_tag.Closure;
                          addr; };
            stack = stack'';
            heap = heap';
            pc = st.pc + 3; }

    (* Use return address *)
    | RETURN ->
        let n = p.(st.pc + 1) in
        let stack' = List_util.drop_n st.stack n in begin
          match st.extra_args with
          | 0 ->
              let (pc, env, extra_args, stack'') = match stack' with
                | Long pc :: Block env :: Long extra_args :: stack'' ->
                    (pc, env, extra_args, stack'')
                | _ -> assert false in

              { st with pc; env; extra_args; stack = stack''; }
          | _ ->
              { st with pc = code_value st.acc;
                        env = block st.acc; }
        end

    (* Apply function to a certain number of args *)
    | APPLY1 -> step_apply 1
    | APPLY2 -> step_apply 2
    | APPLY3 -> step_apply 3
    | APPLY ->
        let args = p.(st.pc + 1) in
        { st with extra_args = args - 1;
                  pc = code_value st.acc;
                  env = block st.acc; }

    (* Set global value *)
    | SETGLOBAL ->
        let n = p.(st.pc + 1) in

        (* Set nth element of globals to acc *)
        let globals' = List_util.set_n st.globals st.acc n
                        ~default:(Long 0) in

        { st with pc = st.pc + 2;
                  globals = globals';
                  acc = Long 0; }

    | _ -> assert false
  in

  let print_state st =
    Printf.printf "pc: %d\n" st.pc;
    Printf.printf "acc: %s\n" (Value.to_string st.acc);
    Printf.printf "stack: %s\n" (List_util.to_string Value.to_string st.stack);
  in

  print_state |> ignore;

  (* Run from st to the final state *)
  let rec loop (st : state) : state =
    if is_final st then st
    else loop (step st) in

  loop {
    stack = [];
    pc = 0;
    acc = Long 0;
    heap = Heap.empty;
    globals = [];
    extra_args = 0;
    env = {
      size = 0;
      tag = None;
      addr = 0;
    };
  }
