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
    output = E.[ push 0x0L; POP; push 0x4321L ];
    name = "const";
  };

  { input = I.[| op CONSTINT; 0x4321;
                 op PUSHCONSTINT; 0x8765;
                 op PUSH; |];
    output = E.[ push 0x0L; POP; push 0x4321L; push 0x8765L; DUP 1 ];
    name = "PUSHCONSTINT";
  };
]

let mk_test (t : test) : OUnit2.test =
  t.name >:: fun _ ->
    assert_equal ~printer:(List_util.to_string E.instr_to_string)
      (G.convert t.input |> J.process) t.output

let ounit_test =
  "evm" >::: List.map mk_test tests
