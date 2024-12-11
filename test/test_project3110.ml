open OUnit2
open Project3110.Deck
open Project3110.Player

let test_deck = shuffle_deck (Project3110.Deck.init_deck ())
let player1_init = init_player "player1"

let rec add_properties player deck =
  match deck with
  | [] -> player
  | card :: rest ->
      let updated_player = Project3110.Player.add_property player card in
      add_properties updated_player rest

let player1 =
  add_properties player1_init
    [
      ("Light Blue", "Oriental Avenue");
      ("Light Blue", "Vermont Avenue");
      ("Light Blue", "Connecticut Avenue");
      ("Orange", "St. James Place");
      ("Orange", "Tennessee Avenue");
    ]

(* Player hand can have no more than 8 cards *)

(* Helper functions *)

let init_test_players () = (init_player "p1", init_player "p2")

let add_test_properties player =
  add_properties player
    [
      ("Brown", "Mediterranean Avenue");
      ("Light Blue", "Oriental Avenue");
      ("Light Blue", "Vermont Avenue");
      ("Light Blue", "Connecticut Avenue");
    ]

let test_player test_name input output =
  test_name >:: fun _ -> assert_equal input output

let test_has_property test_name player property expected =
  test_player test_name
    (List.mem property (Project3110.Player.get_properties player))
    expected

(* Card Action Tests *)

let card_action_tests =
  let p1, p2 = init_test_players () in
  let p1_with_prop, p2_with_prop =
    (add_test_properties p1, add_test_properties p2)
  in
  [
    (* Test: Forced Deal - checks if p1 receives p2's property in exchange *)
    test_has_property "Forced Deal - p1 gets new property"
      (fst
         (Project3110.CardAction.forced_deal p1_with_prop p2_with_prop
            ("Brown", "Mediterranean Avenue")
            ("Light Blue", "Oriental Avenue")))
      ("Light Blue", "Oriental Avenue")
      true;
    (* Test: Forced Deal - verifies p2 no longer has their original property *)
    test_has_property "Forced Deal - p2 loses property"
      (snd
         (Project3110.CardAction.forced_deal p1_with_prop p2_with_prop
            ("Brown", "Mediterranean Avenue")
            ("Light Blue", "Oriental Avenue")))
      ("Light Blue", "Oriental Avenue")
      false;
    (* Test: Sly Deal - verifies p1 successfully steals p2's property *)
    test_has_property "Sly Deal - p1 gets property"
      (fst
         (Project3110.CardAction.sly_deal p1_with_prop p2_with_prop
            ("Light Blue", "Oriental Avenue")))
      ("Light Blue", "Oriental Avenue")
      true;
  ]

(* Game State Tests *)

let tests =
  [
    ("Trivial test" >:: fun _ -> assert_equal 0 0);
    ("Deck test" >:: fun _ -> assert_equal (List.length test_deck) 89);
    test_player "Testing Player - init name" (get_name player1_init) "player1";
    test_player "Testing Player - init properties"
      (get_properties player1_init)
      [];
    test_player "Testing Player - init bank" (get_bank player1_init) 0;
    test_player "Testing Player - init deck" (get_hand player1_init) [];
    test_player "Testing Player - hand" (get_property_sets player1) 1;
  ]

let run_tests = "test suite" >::: List.flatten [ tests; card_action_tests ]
let _ = run_test_tt_main run_tests
