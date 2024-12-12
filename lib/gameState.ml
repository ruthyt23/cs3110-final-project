open Deck
open Player
open CardAction

type game_state = {
  players : Player.player list;
  curr_player_index : int;
  deck : Deck.card list;
  discard_pile : Deck.card list;
}

let next_turn game_state =
  let num_players = List.length game_state.players in
  let next_index = (game_state.curr_player_index + 1) mod num_players in
  { game_state with curr_player_index = next_index }

let check_win_condition player =
  let properties_count = get_property_sets player in
  properties_count >= 3

let get_target_player players current_player =
  print_endline "\nPlayers:";
  List.iteri
    (fun i p ->
      if get_name p <> get_name current_player then
        Printf.printf "%d: %s\n" i (get_name p))
    players;
  print_string "\nSelect a target player: ";
  let target_index = int_of_string (read_line ()) in
  List.nth players target_index

let get_discard game_state = game_state.discard_pile

let select_property player prompt =
  print_endline ("\n" ^ get_name player ^ "'s Properties:");
  let properties = Player.get_properties player in
  List.iteri
    (fun i (color, name) -> Printf.printf "%d: %s %s\n" i color name)
    properties;
  print_string ("\nSelect a property to " ^ prompt ^ ": ");
  let property_index = int_of_string (read_line ()) in
  List.nth properties property_index

let play_card game_state player card =
  let player_without_card = remove_from_hand player card in

  match card with
  | Money amount ->
      let updated_player = bank_money player_without_card amount in
      let updated_players =
        List.map
          (fun p -> if get_name p = get_name player then updated_player else p)
          game_state.players
      in
      { game_state with players = updated_players }
  | Property (color, property_name) ->
      let updated_player =
        add_property player_without_card (color, property_name)
      in
      let updated_players =
        List.map
          (fun p -> if get_name p = get_name player then updated_player else p)
          game_state.players
      in
      { game_state with players = updated_players }
  | Action action -> (
      let updated_discard_pile = card :: game_state.discard_pile in
      match action with
      | "Forced Deal" ->
          let target_player = get_target_player game_state.players player in
          let card_to_receive = select_property target_player "steal" in
          let card_to_give = select_property player "give" in
          let updated_player, updated_target_player =
            forced_deal player_without_card target_player card_to_give
              card_to_receive
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
          {
            game_state with
            players = updated_players;
            discard_pile = updated_discard_pile;
          }
      | "Sly Deal" ->
          let target_player = get_target_player game_state.players player in
          let card_to_receive = select_property target_player "steal" in
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
          {
            game_state with
            players = updated_players;
            discard_pile = updated_discard_pile;
          }
      | "Debt Collector" ->
          let target_player = get_target_player game_state.players player in
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
          {
            game_state with
            players = updated_players;
            discard_pile = updated_discard_pile;
          }
      | "It's My Birthday" ->
          let updated_players =
            its_my_birthday player_without_card game_state.players
          in
          {
            game_state with
            players = updated_players;
            discard_pile = updated_discard_pile;
          }
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
          {
            game_state with
            players = updated_players;
            deck = updated_deck;
            discard_pile = updated_discard_pile;
          }
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
