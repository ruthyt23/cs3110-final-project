open OUnit2
open Project3110.Deck

(*************** Helpers for Testing Deck.mli *********************************)
let test_deck = Project3110.Deck.shuffle_deck (Project3110.Deck.init_deck ())

let rec draw_card deck int =
  match int with
  | 0 -> deck
  | _ -> draw_card (snd (Project3110.Deck.draw_card deck)) (int - 1)

let new_deck = draw_card test_deck 7

(*************** Helpers for Testing Player.mli *******************************)
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
      ("Light Blue", "Oriental Avenue");
      ("Light Blue", "Vermont Avenue");
      ("Light Blue", "Connecticut Avenue");
      ("Orange", "St. James Place");
      ("Orange", "Tennessee Avenue");
    ]

let player1 = Project3110.Player.remove_from_bank player1 8

let create_test test_name input output =
  test_name >:: fun _ -> assert_equal input output

let tests =
  "test suite"
  >::: [
         ("Trivial test" >:: fun _ -> assert_equal 0 0);
         (* Checks deck size is 89 *)
         ("Deck test" >:: fun _ -> assert_equal (List.length test_deck) 89);
         (* Player initialization tests *)
         (* 1. Player name is player1 *)
         create_test "Testing Player - init name"
           (Project3110.Player.get_name player1_init)
           "player1";
         (* 2. Player is initialized with 0 properties *)
         create_test "Testing Player - init properties"
           (Project3110.Player.get_properties player1_init)
           [];
         (* 3. Player is initialized with no money in the bank *)
         create_test "Testing Player - init bank"
           (Project3110.Player.get_bank player1_init)
           0;
         (* 4. Player has no money in their bank *)
         create_test "Testing Player - init deck"
           (Project3110.Player.get_hand player1_init)
           [];
         (* 5. Checks if player has a full set in their hand *)
         create_test "Testing Player - hand"
           (Project3110.Player.get_property_sets player1)
           1;
         (* 6. Tests that a player cannot have negative in their bank *)
         create_test "Testing Player - add_bank"
           (Project3110.Player.get_bank player1)
           0;
         (* 7. Tests that the deck is removing cards after they've been drawn *)
         create_test "Testing - draw_card" (List.length new_deck) 82;
       ]

let _ = run_test_tt_main tests
