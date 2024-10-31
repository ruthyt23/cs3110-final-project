open Deck
open Player

type game_state = {
  players : Player.player list;
  curr_player_index : int;
  deck : Deck.card list;
}

let next_turn game_state =
  let num_players = List.length game_state.players in
  let next_index = (game_state.curr_player_index + 1) mod num_players in
  { game_state with curr_player_index = next_index }

let check_win_condition player =
  let properties_count = get_property_sets player in
  properties_count >= 3

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
  | _ -> failwith "Action card handling not yet implemented"

let draw_card game_state =
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
