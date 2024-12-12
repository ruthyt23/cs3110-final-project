open Project3110
open Player
open GameState
open Deck

let rec print_hand cards =
  match cards with
  | [] -> ()
  | card :: rest ->
      (match card with
      | Money amount -> Printf.printf "Money card: $%dM\n" amount
      | Property (color, name) ->
          Printf.printf "Property card: %s - %s\n" color name
      | Action name -> Printf.printf "Action card: %s\n" name);
      print_hand rest

let get_card_choice hand =
  print_endline "\nYour hand:";
  List.iteri
    (fun i card ->
      Printf.printf "%d: " i;
      match card with
      | Money amount -> Printf.printf "Money card: $%dM\n" amount
      | Property (color, name) ->
          Printf.printf "Property card: %s - %s\n" color name
      | Action name -> Printf.printf "Action card: %s\n" name)
    hand;
  print_string "Choose a card to play (enter number) or -1 to skip: ";
  read_int ()

let print_game_state game_state =
  let players = GameState.get_players game_state in
  List.iter
    (fun player ->
      Printf.printf "\n=== %s's Table ===\n" (Player.get_name player);
      Printf.printf "💰 Bank: $%dM\n" (Player.get_bank player);
      Printf.printf "🏠 Properties:\n";
      List.iter
        (fun (color, name) -> Printf.printf "  %s - %s\n" color name)
        (Player.get_properties player))
    players;
  print_endline ""

let rec play_cards_phase game_state cards_played =
  (* Check if player has already played maximum number of cards *)
  if cards_played >= 3 then (
    print_endline "\nYou've played the maximum of 3 cards.";
    game_state)
  else
    (* Get current player and their hand *)
    let current_player = GameState.get_current_player game_state in
    let current_hand = Player.get_hand current_player in

    (* Display current game state *)
    print_game_state game_state;

    (* Display current hand *)
    print_endline "\nYour hand:";
    print_hand current_hand;
    Printf.printf "\nYou can play %d more cards this turn.\n" (3 - cards_played);
    print_endline "Would you like to play a card? (y/n)";

    (* Get player's choice to play a card or not *)
    match read_line () with
    | "y" | "Y" ->
        let card_index = get_card_choice current_hand in

        (* Validate card choice and play the card *)
        if card_index >= 0 && card_index < List.length current_hand then
          let chosen_card = List.nth current_hand card_index in
          match chosen_card with
          | Money _ | Property _ | Action _ ->
              (* Play the card and recursively continue the play phase *)
              let updated_state =
                play_card game_state current_player chosen_card false
              in
              play_cards_phase updated_state (cards_played + 1)
        else
          (* Invalid card index, retry without counting as a play *)
          play_cards_phase game_state cards_played
    (* If player doesn't want to play a card, end the play phase *)
    | _ -> game_state

let create_box message =
  let box_width = 40 in
  let emoji_width = 2 in
  (* Assume each emoji is 2 characters wide *)
  let msg_len = String.length message - (2 * emoji_width) in
  let padding = (box_width - msg_len) / 2 in
  let left_padding = String.make padding ' ' in
  let right_padding = String.make (box_width - msg_len - padding) ' ' in

  Printf.printf "\n\n+%s+\n" (String.make box_width '-');
  Printf.printf "|%s%s%s|\n" left_padding message right_padding;
  Printf.printf "+%s+\n\n" (String.make box_width '-')

let rec discard_excess_cards game_state cards_discarded =
  let current_player = GameState.get_current_player game_state in
  let current_hand = Player.get_hand current_player in
  let hand_size = List.length current_hand in
  if hand_size <= 7 then game_state
  else (
    print_string "Choose a card to discard (enter number): ";
    match read_int_opt () with
    | Some discard_index when discard_index >= 0 && discard_index < hand_size ->
        let card_to_remove =
          List.nth current_hand (discard_index - cards_discarded)
        in

        let updated_state =
          discard_card game_state current_player card_to_remove
        in
        let new_cards_discarded = cards_discarded + 1 in
        discard_excess_cards updated_state new_cards_discarded
    | _ ->
        print_endline "Invalid choice. Please try again.";
        discard_excess_cards game_state cards_discarded)

let play_turn game_state =
  let current_player = GameState.get_current_player game_state in
  let name = get_name current_player in
  let message = Printf.sprintf "🎮 %s's Turn 🎮" name in
  create_box message;

  (* Draw two cards *)
  print_endline "\nDrawing 2 cards...";
  let state_after_draws = GameState.draw_card game_state in
  let final_draw_state = GameState.draw_card state_after_draws in

  (* (* Show current hand *) let current_player_updated =
     GameState.get_current_player final_draw_state in print_endline "\nYour
     current hand:"; print_hand (get_hand current_player_updated); *)

  (* Play up to 3 cards phase *)
  let new_state = play_cards_phase final_draw_state 0 in

  let new_current_player = GameState.get_current_player new_state in
  let current_hand = Player.get_hand new_current_player in
  let hand_size = List.length current_hand in
  if hand_size <= 7 then next_turn new_state
  else (
    Printf.printf "\nYou have %d cards in your hand. Please discard %d cards.\n"
      hand_size (hand_size - 7);
    List.iteri
      (fun i card ->
        Printf.printf "%d: " i;
        match card with
        | Money amount -> Printf.printf "Money card: $%dM\n" amount
        | Property (color, name) ->
            Printf.printf "Property card: %s - %s\n" color name
        | Action name -> Printf.printf "Action card: %s\n" name)
      current_hand;

    let final_state = discard_excess_cards new_state 0 in

    (* End turn *)
    next_turn final_state)

let rec game_loop game_state =
  (* Get current player *)
  let current_player = GameState.get_current_player game_state in

  (* Check win condition before the turn *)
  if GameState.check_win_condition current_player then (
    Printf.printf "\n🎉 %s wins! 🎉\n" (Player.get_name current_player);
    Printf.printf "They collected 3 full property sets!\n")
  else
    (* Play the turn *)
    let updated_state = play_turn game_state in

    (* Print a divider between turns *)
    print_endline "\n----------------------------------------\n";

    (* Continue the game loop with the updated state *)
    game_loop updated_state

let main () =
  print_endline "Welcome to Monopoly Deal!\n";

  print_string "Enter number of players (2-5): ";
  let num_players = read_int () in
  if num_players < 2 || num_players > 5 then
    failwith "Number of players must be between 2 and 5"
  else
    (* Initialize players *)
    let rec init_players n acc =
      if n = 0 then acc
      else (
        Printf.printf "Enter name for Player %d: " (num_players - n + 1);
        let player_name = read_line () in
        let player = Player.init_player player_name in
        init_players (n - 1) (player :: acc))
    in
    let players = init_players num_players [] in

    let initial_state = GameState.init_game players in

    (* Start game loop *)
    print_endline "\nGame starting...\n";
    game_loop initial_state
;;

main ()
