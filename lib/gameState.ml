open Deck
open Player
open CardAction

type game_state = {
  players : Player.player list;
  curr_player_index : int;
  deck : Deck.card list;
  discard_pile : Deck.card list;
}
(** [game_state] represents the current state of a Monopoly Deal game. It
    contains:
    - A list of all players
    - The index of the current player's turn
    - The remaining deck of cards
    - A discard pile of played cards *)

let next_turn game_state =
  let num_players = List.length game_state.players in
  let next_index = (game_state.curr_player_index + 1) mod num_players in
  { game_state with curr_player_index = next_index }

let check_win_condition player =
  let properties_count = get_property_sets player in
  properties_count >= 3

let get_discard game_state = game_state.discard_pile

(* Simple mock inputs for testing *)
let mock_target_index = ref 1
let mock_property_index = ref 0

let get_target_player players current_player test_flag =
  match test_flag with
  | true ->
      (* In test mode, just use the mock index *)
      List.nth players !mock_target_index
  | false ->
      (* Normal gameplay with I/O *)
      print_endline "\nPlayers:";
      List.iteri
        (fun i p ->
          if get_name p <> get_name current_player then
            Printf.printf "%d: %s\n" i (get_name p))
        players;
      let rec get_target () =
        print_string "\nSelect a target player: ";
        try
          let target_index = int_of_string (read_line ()) in
          if target_index >= 0 && target_index < List.length players then
            List.nth players target_index
          else (
            print_endline "Invalid player number. Please try again.";
            get_target ())
        with
        | Failure _ ->
            print_endline "Please enter a valid number";
            get_target ()
        | End_of_file ->
            print_endline "\nGame terminated.";
            exit 0
      in
      get_target ()

let select_property player prompt test_flag =
  match test_flag with
  | true ->
      (* In test mode, just use the mock index *)
      List.nth (Player.get_properties player) !mock_property_index
  | false ->
      (* Normal gameplay with I/O *)
      print_endline ("\n" ^ get_name player ^ "'s Properties:");
      let properties = Player.get_properties player in
      List.iteri
        (fun i (color, name) -> Printf.printf "%d: %s %s\n" i color name)
        properties;
      print_string ("\nSelect a property to " ^ prompt ^ ": ");
      let property_index = int_of_string (read_line ()) in
      List.nth properties property_index

let select_color player =
  let properties = Player.get_properties player in
  if List.is_empty properties then ""
  else (
    print_endline ("\n" ^ get_name player ^ "'s Properties:");
    List.iteri
      (fun i (color, name) -> Printf.printf "%d: %s %s\n" i color name)
      properties;
    print_string "\nSelect a property with the wanted color: ";
    let property_index = int_of_string (read_line ()) in
    let color, _ = List.nth properties property_index in
    color)

let rec just_say_no_check player1 player2 player1_wants =
  if Player.card_check player2 Deck.just_say_no then (
    print_endline
      ("\n" ^ get_name player2
     ^ " has a Just Say No card. Would you like to use it? (y/n)");
    match read_line () with
    | "y" ->
        let new_player2 = remove_from_hand player2 Deck.just_say_no in
        just_say_no_check new_player2 player1 (not player1_wants)
    | "n" -> (player1, player2, player1_wants)
    | _ ->
        print_endline "Invalid input entered. Try again!";
        just_say_no_check player1 player2 player1_wants)
  else (player1, player2, player1_wants)

let play_card game_state player card test_flag =
  let player_without_card = remove_from_hand player card in

  match card with
  | Money amount ->
      let updated_player = bank_money player_without_card amount in
      let updated_players =
        List.map
          (fun p -> if get_name p = get_name player then updated_player else p)
          game_state.players
      in
      ({ game_state with players = updated_players }, 0)
  | Property (color, property_name) ->
      let updated_player =
        add_property player_without_card (color, property_name)
      in
      let updated_players =
        List.map
          (fun p -> if get_name p = get_name player then updated_player else p)
          game_state.players
      in
      ({ game_state with players = updated_players }, 0)
  | Action action -> (
      let updated_discard_pile = card :: game_state.discard_pile in
      match action with
      | "Forced Deal" ->
          let target_player =
            get_target_player game_state.players player test_flag
          in
          let new_player1, new_player2, check =
            just_say_no_check player_without_card target_player false
          in
          let updated_player, updated_target_player =
            if check then (
              print_endline
                ("\n" ^ get_name target_player
               ^ " used a Just Say No card - tough luck!");
              (new_player1, new_player2))
            else
              let card_to_receive =
                select_property target_player "steal" test_flag
              in
              let card_to_give = select_property player "give" test_flag in
              let new_player1, new_player2 =
                forced_deal player_without_card target_player card_to_give
                  card_to_receive
              in
              (new_player1, new_player2)
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player
                else if get_name p = get_name target_player then
                  updated_target_player
                else p)
              game_state.players
          in
          ( {
              game_state with
              players = updated_players;
              discard_pile = updated_discard_pile;
            },
            0 )
      | "Sly Deal" ->
          let target_player =
            get_target_player game_state.players player test_flag
          in
          let card_to_receive =
            select_property target_player "steal" test_flag
          in
          let updated_player, updated_target_player =
            sly_deal player_without_card target_player card_to_receive
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player
                else if get_name p = get_name target_player then
                  updated_target_player
                else p)
              game_state.players
          in
          ( {
              game_state with
              players = updated_players;
              discard_pile = updated_discard_pile;
            },
            0 )
      | "Debt Collector" ->
          let target_player =
            get_target_player game_state.players player test_flag
          in
          let updated_player, updated_target_player =
            debt_collector player_without_card target_player 5
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player
                else if get_name p = get_name target_player then
                  updated_target_player
                else p)
              game_state.players
          in
          ( {
              game_state with
              players = updated_players;
              discard_pile = updated_discard_pile;
            },
            0 )
      | "It's My Birthday" ->
          let updated_players =
            its_my_birthday player_without_card game_state.players
          in
          ( {
              game_state with
              players = updated_players;
              discard_pile = updated_discard_pile;
            },
            0 )
      | "Pass Go" ->
          let updated_player, updated_deck =
            pass_go player_without_card game_state.deck
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player else p)
              game_state.players
          in
          ( {
              game_state with
              players = updated_players;
              deck = updated_deck;
              discard_pile = updated_discard_pile;
            },
            0 )
      | "Deal Breaker" ->
          let target_player =
            get_target_player game_state.players player false
          in
          let target_color = select_color target_player in
          let updated_player, updated_target_player =
            if target_color = "" then (
              print_string "The chosen player has no properties. Sorry!\n";
              (player_without_card, target_player))
            else if
              Deck.property_count
                (Player.get_properties target_player)
                target_color
              != Deck.full_property_count target_color
            then (
              print_string
                ("The " ^ target_color
               ^ " set hasn't been complete yet. Sorry!\n");
              (player_without_card, target_player))
            else
              let _, prop_list =
                List.hd
                  (List.filter
                     (fun (color, _) -> color = target_color)
                     Player.property_sets)
              in
              deal_breaker player_without_card target_player prop_list
                target_color
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player
                else if get_name p = get_name target_player then
                  updated_target_player
                else p)
              game_state.players
          in
          ({ game_state with players = updated_players }, 0)
      | "Wild Rent Card" ->
          let updated_player, updated_target_player, mult =
            if List.length (get_properties player_without_card) = 0 then (
              print_string
                "You currently don't have any properties to charge rent on. \
                 Sorry!\n";
              (player_without_card, None, 1))
            else
              let rec double_rent_check player loop_num =
                let double_rent_card_check =
                  card_count player Deck.double_the_rent
                in
                if double_rent_card_check = 0 then (player, 1)
                else (
                  print_endline
                    "You have a Double the Rent card. Would you like to use \
                     it? (y/n)";
                  match read_line () with
                  | "y" ->
                      let new_player1 =
                        remove_from_hand player Deck.double_the_rent
                      in
                      if double_rent_card_check = 2 then
                        double_rent_check new_player1 2
                      else (new_player1, 2 * loop_num)
                  | "n" -> (player, 1)
                  | _ ->
                      print_endline "Invalid input entered. Try again!";
                      double_rent_check player 1)
              in
              let updated_player1, mult =
                double_rent_check player_without_card 1
              in
              let color = select_color updated_player1 in
              let target_player =
                get_target_player game_state.players updated_player1 false
              in
              let updated_pl1, updated_pl2 =
                charge_rent updated_player1 target_player color mult
              in
              (updated_pl1, Some updated_pl2, mult)
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player
                else
                  match updated_target_player with
                  | Some name -> if get_name p = get_name name then name else p
                  | None -> p)
              game_state.players
          in
          ( { game_state with players = updated_players },
            if mult > 1 then mult / 2 else 0 )
      | "House" ->
          let updated_player =
            if get_property_sets player_without_card = 0 then (
              print_string
                "You currently don't have a complete property set to place a \
                 house on. Sorry!\n";
              player_without_card)
            else
              let target_color = select_color player_without_card in
              add_house player_without_card target_color
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player else p)
              game_state.players
          in
          ( {
              game_state with
              players = updated_players;
              discard_pile = updated_discard_pile;
            },
            0 )
      | "Hotel" ->
          let updated_player =
            if get_property_sets player_without_card = 0 then (
              print_string
                "You currently don't have a complete property set to place a \
                 hotel on. Sorry!\n";
              player_without_card)
            else
              let house_and_hotel =
                Player.get_house_and_hotel player_without_card
              in
              let curr_houses =
                List.filter
                  (fun (color, typ) ->
                    typ = "House"
                    && not (List.mem (color, "Hotel") house_and_hotel))
                  house_and_hotel
              in
              if List.length curr_houses = 0 then (
                print_string
                  "You currently don't have any houses to place a hotel on. \
                   Sorry!\n";
                player_without_card)
              else
                let target_color =
                  print_endline "Properties with a House:";
                  List.iteri
                    (fun i (color, _) -> Printf.printf "%d: %s\n" i color)
                    curr_houses;
                  print_string "\nSelect the wanted color: ";
                  let property_index = int_of_string (read_line ()) in
                  let color, _ = List.nth curr_houses property_index in
                  color
                in
                add_hotel player_without_card target_color
          in
          let updated_players =
            List.map
              (fun p ->
                if get_name p = get_name player then updated_player else p)
              game_state.players
          in
          ( {
              game_state with
              players = updated_players;
              discard_pile = updated_discard_pile;
            },
            0 )
      | "Double The Rent" ->
          print_string
            "Please wait to play this card until you play a Rent card!";
          (game_state, 0)
      | _ -> failwith "not yet implemented")

let draw_card game_state =
  try
    let card, new_deck = draw_card game_state.deck in
    let current_player =
      List.nth game_state.players game_state.curr_player_index
    in
    let updated_player = add_to_hand current_player card in
    let updated_players =
      List.map
        (fun player ->
          if get_name player = get_name current_player then updated_player
          else player)
        game_state.players
    in
    { game_state with players = updated_players; deck = new_deck }
  with Failure _ ->
    let discard_deck = shuffle_deck game_state.discard_pile in
    let new_discard_pile = [] in
    let card, new_deck = draw_card discard_deck in
    let current_player =
      List.nth game_state.players game_state.curr_player_index
    in
    let updated_player = add_to_hand current_player card in
    let updated_players =
      List.map
        (fun player ->
          if get_name player = get_name current_player then updated_player
          else player)
        game_state.players
    in
    {
      game_state with
      players = updated_players;
      deck = new_deck;
      discard_pile = new_discard_pile;
    }

let discard_card game_state current_player card =
  let updated_player = remove_from_hand current_player card in
  let updated_players =
    List.map
      (fun player ->
        if get_name player = get_name current_player then updated_player
        else player)
      game_state.players
  in
  let updated_discard_pile = card :: game_state.discard_pile in
  {
    game_state with
    players = updated_players;
    discard_pile = updated_discard_pile;
  }

let rec deal_initial_cards game_state num_cards =
  if num_cards = 0 then game_state
  else deal_initial_cards (draw_card game_state) (num_cards - 1)

let init_game starting_players =
  match starting_players with
  | [] -> failwith "at least 2 players are required"
  | first_player :: rest ->
      let initial_state =
        {
          players = starting_players;
          curr_player_index = 0;
          deck = shuffle_deck (init_deck ());
          discard_pile = [];
        }
      in
      (* Deal 5 cards to each player *)
      let rec deal_to_all_players state players =
        match players with
        | [] -> state
        | player :: rest ->
            (* Deal 5 cards to current player *)
            let state_after_cards = deal_initial_cards state 5 in
            (* Move to next player *)
            let next_player_state =
              {
                state_after_cards with
                curr_player_index =
                  (state_after_cards.curr_player_index + 1)
                  mod List.length starting_players;
              }
            in
            deal_to_all_players next_player_state rest
      in
      (* Deal cards and reset to first player *)
      let final_state = deal_to_all_players initial_state starting_players in
      { final_state with curr_player_index = 0 }

(* Implement the accessor functions *)
let get_current_player game_state =
  List.nth game_state.players game_state.curr_player_index

let get_players game_state = game_state.players
let get_current_player_index game_state = game_state.curr_player_index
