open OUnit2

let ounit_test = "all" >::: [
  Test_interp.ounit_test;
  Test_evm.ounit_test;
]

let _ = begin
  try run_test_tt_main ounit_test with Unix.Unix_error _ -> ();
  print_string "\n\nAll tests run!";
end
