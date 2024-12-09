open OUnit2
open Project3110.Deck

let test_deck = Project3110.Deck.shuffle_deck (Project3110.Deck.init_deck ())

let test_card_action test_name input output =
  test_name >:: fun _ -> assert_equal input output

let tests =
  "test suite"
  >::: [
         ("Trivial test" >:: fun _ -> assert_equal 0 0);
         ("Deck test" >:: fun _ -> assert_equal (List.length test_deck) 89);
       ]

let _ = run_test_tt_main tests
