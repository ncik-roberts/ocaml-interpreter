open OUnit2
open Interp
open Value

(* useful data type for writing tests *)
module Mock_instr = struct
  type t =
    | Op of Instruct.op
    | Imm of int
  let to_opcode = function
    | Imm i -> i
    | Op op -> Instruct.to_opcode op
end

open Mock_instr

type test = {
  program : Mock_instr.t list; (* The program to run *)
  tests : (state Lazy.t -> OUnit2.test) list;
  name : string; (* Name of test suite *)
}

(* Various ways of constructing a test *)
let acc_equals acc st =
  "acc_equals" >:: fun _ ->
    let st = Lazy.force st in
    assert_equal ~printer:Value.to_string st.acc acc

let runs st = "runs" >:: fun _ -> Lazy.force st |> ignore

let stack_equals stack st =
  "stack_equals" >:: fun _ ->
    let st = Lazy.force st in
    assert_equal ~printer:(List_util.to_string Value.to_string) st.stack stack

let mk_test ?(name="test") k st =
  name >:: fun _ ->
    let st = Lazy.force st in
    k st

let block_is tag size expected =
  mk_test ~name:"block_is" (fun st -> match st.acc with
    | Block b ->
        assert_equal ~printer:Block_tag.to_string b.tag tag;
        assert_equal ~printer:string_of_int b.size size;
        let fields = Heap.lookup st.heap b.addr b.size in
        assert_equal ~printer:(List_util.to_string Value.to_string)
           fields expected
    | _ -> assert_failure "not a block")

let block_contains_in_order tag size expected =
  mk_test ~name:"block_contains" (fun st -> match st.acc with
    | Block b ->
        assert_equal ~printer:Block_tag.to_string b.tag tag;
        assert_equal ~printer:string_of_int b.size size;
        let fields = Heap.lookup st.heap b.addr b.size in
        List.iter2 (fun field exp -> match exp with
          | `Long i -> assert_equal ~printer:Value.to_string (Long i) field
          | `Closure ->
              assert_bool (Value.to_string field ^ "not function")
                (match field with Block { tag = Some Block_tag.Closure } -> true
                             | _ -> false)
        ) fields expected
    | _ -> assert_failure "not a block")

(* All the tests we wish to run *)
let tests : test list = Instruct.[
  { program = [ Op CONSTINT; Imm 0x4321; ];
    tests = [
      runs;
      acc_equals (Long 0x4321);
    ];
    name = "CONSTINT0";
  };

  { program = [ Op CONSTINT; Imm 0x4321;
                Op PUSHCONSTINT; Imm 0x8765;
                Op PUSH; ];
    tests = [
      runs;
      stack_equals [ Long 0x8765; Long 0x4321 ];
      acc_equals (Long 0x8765);
    ];
    name = "PUSHCONSTINT";
  };

  { program = [ Op CONSTINT; Imm 0x4321;
                Op PUSHCONSTINT; Imm 0x8765;
                Op MAKEBLOCK2; Imm 0x0; ];
    tests = [
      runs;
      stack_equals [];
      block_is None 2 [ Long 0x8765; Long 0x4321; ];
    ];
    name = "MAKEBLOCK2";
  };

  { program = [ Op CONSTINT; Imm 0x4321;
                Op PUSHCONSTINT; Imm 0x8765;
                Op PUSH;
                Op ACC0;
                Op PUSH;
                Op ACC2;
                Op MAKEBLOCK2; Imm 0;
                Op POP; Imm 2; ];
    tests = [
      runs;
      stack_equals [];
      block_is None 2 [ Long 0x4321; Long 0x8765; ];
    ];
    name = "1.ml";
  };

  { program = [
      Op CONST3;
      Op OFFSETINT; Imm 4;
      Op PUSH; Op ACC0;
      Op OFFSETINT; Imm 8;
      Op PUSH; Op ACC0;
      Op OFFSETINT; Imm 9;
      Op PUSH; Op ACC0;
      Op PUSH; Op ACC2;
      Op PUSH; Op ACC4;
      Op MAKEBLOCK3; Imm 0;
      Op POP; Imm 3;
    ];
    tests = [
      runs;
      stack_equals [];
      block_is None 3 [ Long 7; Long 15; Long 24; ];
    ];
    name = "2.ml";
  };

  { program = [
      Op CONST3; Op PUSH;
      Op ACC0; Op PUSH;
      Op ACC1; Op DIVINT;
      Op PUSH; Op ACC1;
      Op PUSH; Op ACC2;
      Op MULINT; Op SUBINT;
      Op PUSH; Op ACC1;
      Op PUSH; Op ACC2;
      Op PUSH; Op ACC2;
      Op DIVINT; Op MULINT;
      Op PUSH; Op ACC0;
      Op PUSH; Op ACC2;
      Op PUSH; Op ACC4;
      Op MAKEBLOCK3; Imm 0;
      Op POP; Imm 3;
    ];
    tests = [
      runs;
      stack_equals [];
      block_is None 3 [ Long 3; Long 8; Long 6; ];
    ];
    name = "3.ml";
  };

  { program = [
      Op CONST3; Op PUSH;
      Op ACC0; Op PUSH;
      Op CONST3; Op EQ;
      Op BRANCHIFNOT; Imm 5;
      Op CONSTINT; Imm 6;
      Op BRANCH; Imm 2;
      Op CONST0; Op PUSH;
      Op ACC0; Op PUSH; Op ACC2;
      Op ADDINT; Op PUSH; Op ACC1;
      Op GTINT;
      Op BRANCHIFNOT; Imm 19;
      Op ACC1; Op PUSH;
      Op ACC1; Op LEINT;
      Op BRANCHIFNOT; Imm 7;
      Op ACC1; Op PUSH;
      Op ACC1; Op ADDINT;
      Op BRANCH; Imm 21;
      Op ACC1; Op PUSH;
      Op ACC2; Op MULINT;
      Op BRANCH; Imm 15;
      Op ACC1; Op PUSH;
      Op ACC1; Op SUBINT;
      Op PUSH; Op ACC2;
      Op EQ;
      Op BRANCHIFNOT; Imm 3;
      Op CONST0;
      Op BRANCH; Imm 3;
      Op CONSTINT; Imm 100;
      Op PUSH;
      Op ACC0;
      Op PUSH;
      Op ACC2;
      Op PUSH;
      Op ACC4;
      Op MAKEBLOCK3; Imm 0;
      Op POP; Imm 3;
    ];
    tests = [
      runs;
      stack_equals [];
      block_is None 3 [ Long 3; Long 6; Long 0; ];
    ];
    name = "4.ml";
  };

  { program = [
      Op BRANCH; Imm 8;
      Op ACC0; Op PUSH;
      Op CONSTINT; Imm 17185;
      Op MULINT;
      Op RETURN; Imm 1;
      Op CLOSURE; Imm 0; Imm (-7);
      Op PUSH;
      Op CONSTINT; Imm 34661;
      Op PUSH; Op ACC1;
      Op APPLY1;
      Op PUSH; Op ACC0;
      Op PUSH; Op ACC2;
      Op MAKEBLOCK2; Imm 0;
      Op POP; Imm 2;
    ];
    tests = [
      runs;
      stack_equals [];
    ];
    name = "5.ml";
  };

  { program = [
      Op BRANCH; Imm 10;
      Op RESTART;
      Op GRAB; Imm 1;
      Op ACC1;
      Op PUSH;
      Op ACC1;
      Op ADDINT;
      Op RETURN; Imm 2;
      Op CLOSURE; Imm 0; Imm (-8);
      Op PUSH;
      Op CONSTINT; Imm 5;
      Op OFFSETINT; Imm 7;
      Op PUSH;
      Op CONST3;
      Op OFFSETINT; Imm 4;
      Op PUSH;
      Op ACC; Imm 2;
      Op APPLY2;
      Op PUSH;
      Op CONSTINT; Imm 9;
      Op PUSH;
      Op ACC2;
      Op APPLY1;
      Op PUSH;
      Op CONSTINT; Imm 11;
      Op PUSH;
      Op ACC1;
      Op APPLY1;
      Op PUSH;
      Op ACC0;
      Op PUSH;
      Op ACC2;
      Op PUSH;
      Op ACC4;
      Op PUSH;
      Op ACC6;
      Op MAKEBLOCK; Imm 4; Imm 0;
      Op POP; Imm 4;
    ];
    tests = [
      runs;
      stack_equals [];
      block_contains_in_order None 4 [
        `Closure;
        `Long 19;
        `Closure;
        `Long 20;
      ];
    ];
    name = "6.ml";
  };
]

let ounit_tests = List.map (fun test ->
    let program = Array.of_list (List.map to_opcode test.program) in
    let final = lazy (run program) in
    test.name >::: List.map ((|>) final) test.tests)
  tests

let ounit_test = "all" >::: ounit_tests

let _ = begin
  try run_test_tt_main ounit_test with Unix.Unix_error _ -> ();
  print_string "\n\nAll tests run!";
end
