open Deck
open Player

type game_state = {
  players : Player.player list;
  curr_player_index : int;
  deck : Deck.card list;
}

let init_game starting_players =
  match starting_players with
  | [] -> failwith "at least 2 players are required"
  | first_player :: rest ->
      {
        players = starting_players;
        curr_player_index = 0;
        deck = shuffle_deck (init_deck ());
      }

let next_turn game_state =
  let num_players = List.length game_state.players in
  let next_index = (game_state.curr_player_index + 1) mod num_players in
  { game_state with curr_player_index = next_index }

let check_win_condition player =
  let properties_count = get_property_sets player in
  properties_count >= 3

let play_card game_state player card =
  match card with
  | Money amount ->
      let updated_player = bank_money player amount in
      let updated_players =
        List.map
          (fun p -> if get_name p = get_name player then updated_player else p)
          game_state.players
      in
      { game_state with players = updated_players }
  | Property (color, property_name) ->
      let updated_player = add_property player (color, property_name) in
      let updated_players =
        List.map
          (fun p -> if get_name p = get_name player then updated_player else p)
          game_state.players
      in
      { game_state with players = updated_players }
  | _ -> failwith "havent done that card type yet"

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
