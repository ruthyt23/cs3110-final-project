open OUnit2
open Project3110.Deck

let test_deck = Project3110.Deck.shuffle_deck (Project3110.Deck.init_deck ())
let player1_init = Project3110.Player.init_player "player1"

let rec add_property player deck =
  match deck with
  | [] -> player
  | card :: rest ->
      let updated_player = Project3110.Player.add_property player card in
      add_property updated_player rest

let player1 =
  add_property player1_init
    [
      Property ("Light Blue", "Oriental Avenue");
      Property ("Light Blue", "Vermont Avenue");
      Property ("Light Blue", "Connecticut Avenue");
      Property ("Orange", "St. James Place");
      Property ("Orange", "Tennessee Avenue");
    ]

(* Player hand can have no more than 8 cards *)

let test_player test_name input output =
  test_name >:: fun _ -> assert_equal input output

let tests =
  "test suite"
  >::: [
         ("Trivial test" >:: fun _ -> assert_equal 0 0);
         ("Deck test" >:: fun _ -> assert_equal (List.length test_deck) 89);
         test_player "Testing Player - init name"
           (Project3110.Player.get_name player1_init)
           "player1";
         test_player "Testing Player - init properties"
           (Project3110.Player.get_properties player1_init)
           [];
         test_player "Testing Player - init bank"
           (Project3110.Player.get_bank player1_init)
           0;
         test_player "Testing Player - init deck"
           (Project3110.Player.get_hand player1_init)
           [];
         test_player "Testing Player - hand"
           (Project3110.Player.get_property_sets player1)
           1;
       ]

let _ = run_test_tt_main tests
