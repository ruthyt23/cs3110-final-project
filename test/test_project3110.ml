open OUnit2
open Project3110.Deck

let test_deck = Project3110.Deck.shuffle_deck (Project3110.Deck.init_deck ())

let tests =
  "test suite"
  >::: [
         ("Trivial test" >:: fun _ -> assert_equal 0 0);
         ("Deck test" >:: fun _ -> assert_equal (List.length test_deck) 110);
       ]

let _ = run_test_tt_main tests
