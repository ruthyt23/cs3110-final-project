open OUnit2
open Project3110.Deck

let tests = "test suite" >::: [ 
  
("Trivial test" >:: fun _ -> assert_equal 0 0);

 ]
let _ = run_test_tt_main tests
