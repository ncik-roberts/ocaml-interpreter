open OUnit2

module E = Evm
module I = Instruct
module G = Grouper
module J = Jump_dests

let op = I.to_opcode

type test = {
  input : int array;
  output : E.program;
  name : string;
}

let tests = [
  { input = I.[| op CONSTINT; 0x4321; |];
    output = E.[ POP; push 0x4321L ];
    name = "const";
  };

  { input = I.[| op CONSTINT; 0x4321;
                 op PUSHCONSTINT; 0x8765;
                 op PUSH; |];
    output = E.[ POP; push 0x4321L; push 0x8765L; DUP 1 ];
    name = "push";
  };

  { input = I.[| op CONSTINT; 0x4321;
               op PUSHCONSTINT; 0x8765;
               op MAKEBLOCK2; 0x0; |];
    output = E.[
      POP;
      push 0x4321L;
      push 0x8765L;

      (* Store tag at heap counter *)
      push 0x00L;
      push 0x20L;
      MLOAD;
      MSTORE;

      (* Store 0x8765 at next location *)
      push 0x20L;
      MLOAD;
      push 0x20L;
      ADD;
      MSTORE;

      (* Store 0x4321 at next location *)
      push 0x20L;
      MLOAD;
      push 0x40L;
      ADD;
      MSTORE;

      (* Update heap counter *)
      push 0x20L;
      DUP 1; (* Leave heap counter on top of stack as block *)
      MLOAD;
      push 0x60L;
      ADD;
      push 0x20L;
      MSTORE;

      (* Update address on top of stack to represent first entry in block *)
      push 0x20L;
      ADD;
    ];
    name = "makeblock";
  };
]

let setup_instrs = [
  E.push 0L;
  E.push 0x40L;
  E.push 0x20L;
  E.MSTORE;
]

let mk_test (t : test) : OUnit2.test =
  t.name >:: fun _ ->
    assert_equal ~printer:(List_util.to_string E.instr_to_string)
      (G.convert t.input |> J.process) (setup_instrs @ t.output)

let ounit_test =
  "evm" >::: List.map mk_test tests
