open OUnit2
open Project3110.Deck
open Project3110.Player

(*************** Helpers for Testing Deck.mli *********************************)
let test_deck = Project3110.Deck.shuffle_deck (Project3110.Deck.init_deck ())

let rec draw_card deck int =
  match int with
  | 0 -> deck
  | _ -> draw_card (snd (Project3110.Deck.draw_card deck)) (int - 1)

let new_deck = draw_card test_deck 7

(*************** Helpers for Testing Player.mli *******************************)
let player1_init = Project3110.Player.init_player "player1"

let rec add_hand player card_lst =
  match card_lst with
  | [] -> player
  | h :: t ->
      let updated_player = Project3110.Player.add_to_hand player h in
      add_hand updated_player t

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

let player1 =
  add_hand player1
    [
      Property ("Railroad", "Reading Railroad");
      Property ("Railroad", "Pennsylvania Railroad");
      Money 4;
      Money 1;
      Action "Debt Collector";
    ]

let player1_remove_sets =
  Project3110.Player.remove_property player1 ("Light Blue", "Connecticut Avenue")

(* bank_money *)
let player1 = Project3110.Player.remove_from_bank player1 8
let player2 = Project3110.Player.bank_money player1 6

(*************** Helpers for Testing CardAction.mli ***************************)

(* let test_player test_name input output = test_name >:: fun _ -> assert_equal
   input output *)

let init_test_players () = (init_player "p1", init_player "p2", init_player "p3")

let add_test_properties player =
  add_property player
    [
      ("Brown", "Mediterranean Avenue");
      ("Light Blue", "Oriental Avenue");
      ("Light Blue", "Vermont Avenue");
      ("Light Blue", "Connecticut Avenue");
    ]

(************************* Beginning of Test Suite ****************************)
(* let create_test test_name input output = test_name >:: fun _ -> assert_equal
   input output *)

let bool_printer = function
  | true -> "true"
  | false -> "false"

let int_printer x = string_of_int x
let string_printer x = x

(* Additional printer functions *)
let card_printer = function
  | Money n -> "Money " ^ string_of_int n
  | Property (color, name) -> "Property (" ^ color ^ ", " ^ name ^ ")"
  | Action name -> "Action " ^ name

let property_printer (color, name) = "(" ^ color ^ ", " ^ name ^ ")"

let list_printer element_printer lst =
  "[" ^ String.concat "; " (List.map element_printer lst) ^ "]"

let card_list_printer = list_printer card_printer
let property_list_printer = list_printer property_printer

let create_test test_name input output ?printer () =
  match printer with
  | Some p -> test_name >:: fun _ -> assert_equal input output ~printer:p
  | None -> test_name >:: fun _ -> assert_equal input output

let create_bool_test test_name input output =
  create_test test_name input output ~printer:bool_printer ()

let create_int_test test_name input output =
  create_test test_name input output ~printer:int_printer ()

let create_string_test test_name input output =
  create_test test_name input output ~printer:string_printer ()

let create_card_test test_name input output =
  create_test test_name input output ~printer:card_printer ()

let create_card_list_test test_name input output =
  create_test test_name input output ~printer:card_list_printer ()

let create_property_test test_name input output =
  create_test test_name input output ~printer:property_printer ()

let create_property_list_test test_name input output =
  create_test test_name input output ~printer:property_list_printer ()

let test_has_property test_name player property expected =
  create_test test_name
    (List.mem property (Project3110.Player.get_properties player))
    expected

(************************* Card Action Tests ****************************)

let card_action_tests =
  let p1, p2, p3 = init_test_players () in
  let p1_with_prop, p2_with_prop, p3_with_prop =
    (add_test_properties p1, add_test_properties p2, add_test_properties p3)
  in
  let p1_with_money = bank_money p1_with_prop 10 in
  let p2_with_money = bank_money p2_with_prop 10 in
  let p3_with_money = bank_money p2_with_prop 10 in
  [
    (* Forced Deal Tests*)
    create_bool_test "Forced Deal - p1 gets new property"
      (List.mem
         ("Light Blue", "Oriental Avenue")
         (Project3110.Player.get_properties
            (fst
               (Project3110.CardAction.forced_deal p1_with_prop p2_with_prop
                  ("Brown", "Mediterranean Avenue")
                  ("Light Blue", "Oriental Avenue")))))
      true;
    create_bool_test "Forced Deal - p2 loses property"
      (List.mem
         ("Light Blue", "Oriental Avenue")
         (Project3110.Player.get_properties
            (snd
               (Project3110.CardAction.forced_deal p1_with_prop p2_with_prop
                  ("Brown", "Mediterranean Avenue")
                  ("Light Blue", "Oriental Avenue")))))
      false;
    (* Sly Deal Tests*)
    create_bool_test "Sly Deal - p1 gets property"
      (List.mem
         ("Light Blue", "Oriental Avenue")
         (Project3110.Player.get_properties
            (fst
               (Project3110.CardAction.sly_deal p1_with_prop p2_with_prop
                  ("Light Blue", "Oriental Avenue")))))
      true;
    (* Debt Collector Tests *)
    create_int_test "Debt Collector - p1 receives money"
      (let new_p1, _ =
         Project3110.CardAction.debt_collector p1_with_money p2_with_money 5
       in
       Project3110.Player.get_bank new_p1)
      15;
    create_int_test "Debt Collector - p2 loses money"
      (let _, new_p2 =
         Project3110.CardAction.debt_collector p1_with_money p2_with_money 5
       in
       Project3110.Player.get_bank new_p2)
      5;
    (* Pass Go Tests *)
    create_int_test "Pass Go - player receives two cards"
      (let new_p1, _ =
         Project3110.CardAction.pass_go p1 [ Money 1; Money 2; Money 3 ]
       in
       List.length (Project3110.Player.get_hand new_p1))
      2;
    create_int_test "Pass Go - deck loses two cards"
      (let _, new_deck =
         Project3110.CardAction.pass_go p1 [ Money 1; Money 2; Money 3 ]
       in
       List.length new_deck)
      1;
    (* Its My Birthday Tests *)
    create_int_test "Birthday - birthday player gets money another player"
      (let players =
         Project3110.CardAction.its_my_birthday p1_with_money
           [ p1_with_money; p2_with_money ]
       in
       Project3110.Player.get_bank (List.hd players))
      12;
    create_int_test
      "Birthday - birthday player gets money from many other players"
      (let players =
         Project3110.CardAction.its_my_birthday p1_with_money
           [ p1_with_money; p2_with_money; p3_with_money ]
       in
       Project3110.Player.get_bank (List.hd players))
      14;
    create_bool_test "Birthday - non-birthday players lose money"
      (let players =
         Project3110.CardAction.its_my_birthday p1_with_money
           [ p1_with_money; p2_with_money; p3_with_money ]
       in
       get_bank (List.nth players 1) == 8 && get_bank (List.nth players 2) == 8)
      true;
    (* Add hand management tests *)
    create_int_test "Hand Size - after multiple draws"
      (let player = init_player "test" in
       let updated_player =
         List.fold_left
           (fun p _ -> Project3110.Player.add_to_hand p (Money 1))
           player
           (List.init 5 (fun _ -> ()))
       in
       List.length (Project3110.Player.get_hand updated_player))
      5;
    create_bool_test "Hand Contains - specific card check"
      (let player =
         add_hand (init_player "test")
           [
             Money 1; Property ("Brown", "Baltic Avenue"); Action "Deal Breaker";
           ]
       in
       Project3110.Player.card_check player (Action "Deal Breaker"))
      true;
    create_bool_test "Sly Deal - valid property exists"
      (let p1 = init_player "p1" in
       let p2 =
         add_property (init_player "p2") [ ("Brown", "Mediterranean Avenue") ]
       in
       try
         let _, _ =
           Project3110.CardAction.sly_deal p1 p2
             ("Brown", "Mediterranean Avenue")
         in
         true
       with _ -> false)
      true;
    create_bool_test "Deal Breaker - complete set required"
      (let p1 = init_player "p1" in
       let p2 =
         add_property (init_player "p2")
           [
             ("Light Blue", "Oriental Avenue");
             ("Light Blue", "Vermont Avenue");
             ("Light Blue", "Connecticut Avenue");
           ]
       in
       try
         let new_p1, _ =
           Project3110.CardAction.deal_breaker p1 p2
             [ "Oriental Avenue"; "Vermont Avenue"; "Connecticut Avenue" ]
             "Light Blue"
         in
         List.length (Project3110.Player.get_properties new_p1) = 3
       with _ -> false)
      true;
    (* Add money management tests *)
    create_int_test "Bank Operations - complex transaction"
      (let player = bank_money (init_player "test") 10 in
       let p1 = Project3110.Player.remove_from_bank player 3 in
       let p2 = Project3110.Player.bank_money p1 5 in
       let p3 = Project3110.Player.remove_from_bank p2 8 in
       Project3110.Player.get_bank p3)
      4;
    (* Add deck manipulation tests *)
    create_bool_test "Deck Drawing - maintains unique cards"
      (let deck = Project3110.Deck.init_deck () in
       let drawn_card, remaining_deck = Project3110.Deck.draw_card deck in
       not (List.mem drawn_card remaining_deck))
      true;
    create_int_test "Deck Distribution - correct card type counts"
      (let deck = Project3110.Deck.init_deck () in
       let money_cards =
         List.filter
           (fun card ->
             match card with
             | Money _ -> true
             | _ -> false)
           deck
       in
       List.length money_cards)
      20;
    create_int_test "Property Count - mixed color sets"
      (let player =
         add_property (init_player "test")
           [
             ("Brown", "Mediterranean Avenue");
             ("Brown", "Baltic Avenue");
             ("Light Blue", "Oriental Avenue");
             ("Railroad", "Reading Railroad");
             ("Utility", "Electric Company");
           ]
       in
       List.length (Project3110.Player.get_properties player))
      5;
    create_bool_test "Property Removal - specific property"
      (let player =
         add_property (init_player "test")
           [ ("Brown", "Mediterranean Avenue"); ("Brown", "Baltic Avenue") ]
       in
       let updated_player =
         Project3110.Player.remove_property player
           ("Brown", "Mediterranean Avenue")
       in
       not
         (List.mem
            ("Brown", "Mediterranean Avenue")
            (Project3110.Player.get_properties updated_player)))
      true;
  ]

(************************* Game State Tests ****************************)

let game_state_tests =
  let p1, p2, p3 = init_test_players () in
  let p1_with_sets =
    add_property p1
      [
        (* Light Blue Set *)
        ("Light Blue", "Oriental Avenue");
        ("Light Blue", "Vermont Avenue");
        ("Light Blue", "Connecticut Avenue");
        (* Brown Set *)
        ("Brown", "Mediterranean Avenue");
        ("Brown", "Baltic Avenue");
        (* Orange Set *)
        ("Orange", "St. James Place");
        ("Orange", "Tennessee Avenue");
        ("Orange", "New York Avenue");
      ]
  in
  let initial_game = Project3110.GameState.init_game [ p1; p2; p3 ] in
  [
    (* Init Game Tests *)
    create_int_test "Init game - correct number of players"
      (List.length (Project3110.GameState.get_players initial_game))
      3;
    create_int_test "Init game - starts with player 1"
      (Project3110.GameState.get_current_player_index initial_game)
      0;
    create_int_test "Init game - each player gets 5 cards"
      (List.length
         (Project3110.Player.get_hand
            (Project3110.GameState.get_current_player initial_game)))
      5;
    (* Next Turn Tests *)
    create_int_test "Next turn - advances to next player"
      (Project3110.GameState.get_current_player_index
         (Project3110.GameState.next_turn initial_game))
      1;
    create_int_test "Next turn - wraps around to first player"
      (Project3110.GameState.get_current_player_index
         (Project3110.GameState.next_turn
            (Project3110.GameState.next_turn
               (Project3110.GameState.next_turn initial_game))))
      0;
    (* Win Condition Tests *)
    create_bool_test "Win condition - three sets wins"
      (Project3110.GameState.check_win_condition p1_with_sets)
      true;
    create_bool_test "Win condition - no sets doesn't win"
      (Project3110.GameState.check_win_condition p1)
      false;
    create_bool_test "Win Condition - two sets not enough"
      (let player =
         add_property (init_player "almost")
           [
             ("Brown", "Mediterranean Avenue");
             ("Brown", "Baltic Avenue");
             ("Light Blue", "Oriental Avenue");
             ("Light Blue", "Vermont Avenue");
             ("Light Blue", "Connecticut Avenue");
           ]
       in
       Project3110.GameState.check_win_condition player)
      false;
    (* Play Card Tests *)
    create_int_test "Play money card - increases bank"
      (let game_with_money, _ =
         Project3110.GameState.play_card initial_game
           (Project3110.GameState.get_current_player initial_game)
           (Money 5) true
       in
       Project3110.Player.get_bank
         (Project3110.GameState.get_current_player game_with_money))
      5;
    create_bool_test "Play property card - adds to properties"
      (let game_with_property, _ =
         Project3110.GameState.play_card initial_game
           (Project3110.GameState.get_current_player initial_game)
           (Property ("Brown", "Mediterranean Avenue"))
           true
       in
       List.mem
         ("Brown", "Mediterranean Avenue")
         (Project3110.Player.get_properties
            (Project3110.GameState.get_current_player game_with_property)))
      true;
    (* Draw Card Tests *)
    create_int_test "Draw card - increases hand size"
      (let game_after_draw = Project3110.GameState.draw_card initial_game in
       List.length
         (Project3110.Player.get_hand
            (Project3110.GameState.get_current_player game_after_draw)))
      6;
    create_int_test "Draw card - decreases deck size"
      (let initial_deck_size =
         List.length
           (Project3110.Player.get_hand
              (Project3110.GameState.get_current_player initial_game))
       in
       let game_after_draw = Project3110.GameState.draw_card initial_game in
       List.length
         (Project3110.Player.get_hand
            (Project3110.GameState.get_current_player game_after_draw))
       - initial_deck_size)
      1;
    create_bool_test "Play action card - Forced Deal"
      (let p1_with_props = add_test_properties p1 in
       let p2_with_props = add_test_properties p2 in
       let game_with_props =
         Project3110.GameState.init_game [ p1_with_props; p2_with_props ]
       in
       let game_after_action, _ =
         Project3110.GameState.play_card game_with_props p1_with_props
           (Action "Forced Deal") true
       in
       List.mem
         ("Light Blue", "Oriental Avenue")
         (Project3110.Player.get_properties
            (Project3110.GameState.get_current_player game_after_action)))
      true;
    create_bool_test "Play action card - Sly Deal"
      (let p1_with_props = add_test_properties p1 in
       let p2_with_props = add_test_properties p2 in
       let game_with_props =
         Project3110.GameState.init_game [ p1_with_props; p2_with_props ]
       in
       let game_after_action, _ =
         Project3110.GameState.play_card game_with_props p1_with_props
           (Action "Sly Deal") true
       in
       List.mem
         ("Light Blue", "Oriental Avenue")
         (Project3110.Player.get_properties
            (Project3110.GameState.get_current_player game_after_action)))
      true;
    create_int_test "Play action card - Debt Collector"
      (let p1_with_money = bank_money p1 10 in
       let p2_with_money = bank_money p2 10 in
       let game_with_money =
         Project3110.GameState.init_game [ p1_with_money; p2_with_money ]
       in
       let game_after_action, _ =
         Project3110.GameState.play_card game_with_money p1_with_money
           (Action "Debt Collector") true
       in
       Project3110.Player.get_bank
         (Project3110.GameState.get_current_player game_after_action))
      15;
    create_int_test "Play action card - It's My Birthday"
      (let p1_with_money = bank_money p1 10 in
       let p2_with_money = bank_money p2 10 in
       let p3_with_money = bank_money p3 10 in
       let game_with_money =
         Project3110.GameState.init_game
           [ p1_with_money; p2_with_money; p3_with_money ]
       in
       let game_after_action, _ =
         Project3110.GameState.play_card game_with_money
           (Project3110.GameState.get_current_player game_with_money)
           (Action "It's My Birthday") true
       in
       Project3110.Player.get_bank
         (Project3110.GameState.get_current_player game_after_action))
      14;
    create_bool_test "Play action card - Pass Go"
      (let game_after_action, _ =
         Project3110.GameState.play_card initial_game
           (Project3110.GameState.get_current_player initial_game)
           (Action "Pass Go") true
       in
       let hand_size =
         List.length
           (Project3110.Player.get_hand
              (Project3110.GameState.get_current_player game_after_action))
       in
       hand_size = 6 || hand_size = 7)
      true;
    create_bool_test "Play action card - Invalid Action"
      (try
         let _ =
           Project3110.GameState.play_card initial_game p1
             (Action "Invalid Action") true
         in
         false
       with Failure _ -> true)
      true;
    (* Add game state validation tests *)
    create_int_test "Initial Deal - correct card distribution"
      (let game =
         Project3110.GameState.init_game
           [ init_player "p1"; init_player "p2"; init_player "p3" ]
       in
       let current_player = Project3110.GameState.get_current_player game in
       List.length (Project3110.Player.get_hand current_player))
      5;
  ]

let tests =
  [
    (******************** Deck.mli tests ********************************)
    create_int_test "Deck test" (List.length test_deck) 89;
    create_int_test "Testing - draw_card" (List.length new_deck) 82;
    (******************** Player.mli tests ********************************)
    (******************** init_player tests *****************************)
    create_string_test "Testing Player - init name"
      (Project3110.Player.get_name player1_init)
      "player1";
    create_property_list_test "Testing Player - init properties"
      (Project3110.Player.get_properties player1_init)
      [];
    create_int_test "Testing Player - init bank: 0"
      (Project3110.Player.get_bank player1_init)
      0;
    create_card_list_test "Testing Player - init deck: empty list"
      (Project3110.Player.get_hand player1_init)
      [];
    (********************* add/remove_from_hand tests *******************)
    create_card_list_test "Testing Player - add cards to hand "
      (List.rev (Project3110.Player.get_hand player1))
      [
        Property ("Railroad", "Reading Railroad");
        Property ("Railroad", "Pennsylvania Railroad");
        Money 4;
        Money 1;
        Action "Debt Collector";
      ];
    create_test "Testing Player - remove card from empty hand"
      (Project3110.Player.remove_from_hand player1_init (Money 5))
      player1_init ();
    create_card_list_test
      "Testing Player - Remove card from hand that isn't there"
      (List.rev
         (Project3110.Player.get_hand
            (Project3110.Player.remove_from_hand player1 (Money 5))))
      [
        Property ("Railroad", "Reading Railroad");
        Property ("Railroad", "Pennsylvania Railroad");
        Money 4;
        Money 1;
        Action "Debt Collector";
      ];
    create_card_list_test "Testing Player - removing a card from a player"
      (List.rev
         (Project3110.Player.get_hand
            (Project3110.Player.remove_from_hand player1 (Money 4))))
      [
        Property ("Railroad", "Reading Railroad");
        Property ("Railroad", "Pennsylvania Railroad");
        Money 1;
        Action "Debt Collector";
      ];
    (******** add/remove properites/ get_property_sets test *************)
    create_int_test "Testing Player - 1 full set in properties"
      (Project3110.Player.get_property_sets player1)
      1;
    (***************** bank_money/remove_from_bank tests *****************)
    create_int_test "Testing Player - Removing money from 0 bank "
      (Project3110.Player.get_bank player1)
      0;
    create_int_test "Testing Player - Adding money to non-zero bank"
      (Project3110.Player.get_bank (Project3110.Player.bank_money player2 5))
      11;
    create_int_test "Testing Player - Removing from non-zero bank"
      (Project3110.Player.get_bank
         (Project3110.Player.remove_from_bank player2 4))
      2;
    create_int_test "Testing - Drawing multiple cards from deck "
      (List.length new_deck) 82;
    create_int_test "Deck shuffling maintains card count"
      (let deck = Project3110.Deck.init_deck () in
       let shuffled = Project3110.Deck.shuffle_deck deck in
       List.length shuffled)
      89;
    create_bool_test "Deck contains required action cards"
      (let deck = Project3110.Deck.init_deck () in
       let has_deal_breaker =
         List.exists
           (fun card ->
             match card with
             | Action "Deal Breaker" -> true
             | _ -> false)
           deck
       in
       let has_forced_deal =
         List.exists
           (fun card ->
             match card with
             | Action "Forced Deal" -> true
             | _ -> false)
           deck
       in
       has_deal_breaker && has_forced_deal)
      true;
    (* Add property set validation tests *)
    create_bool_test "Property Set - complete light blue set"
      (let player =
         add_property (init_player "test")
           [
             ("Light Blue", "Oriental Avenue");
             ("Light Blue", "Vermont Avenue");
             ("Light Blue", "Connecticut Avenue");
           ]
       in
       Project3110.Player.get_property_sets player = 1)
      true;
    create_bool_test "Property Set - incomplete set doesn't count"
      (let player =
         add_property (init_player "test")
           [
             ("Light Blue", "Oriental Avenue"); ("Light Blue", "Vermont Avenue");
           ]
       in
       Project3110.Player.get_property_sets player = 0)
      true;
  ]

let run_tests =
  "test suite" >::: List.flatten [ tests; card_action_tests; game_state_tests ]

let _ = run_test_tt_main run_tests
