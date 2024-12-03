open Project3110
open Player
open Game_state
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
  let players = Game_state.get_players game_state in
  List.iter
    (fun player ->
      Printf.printf "\n=== %s's Table ===\n" (Player.get_name player);
      Printf.printf "Bank: $%dM\n" (Player.get_bank player);
      Printf.printf "Properties:\n";
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
    let current_player = Game_state.get_current_player game_state in
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
                play_card game_state current_player chosen_card
              in
              play_cards_phase updated_state (cards_played + 1)
        else
          (* Invalid card index, retry without counting as a play *)
          play_cards_phase game_state cards_played
    (* If player doesn't want to play a card, end the play phase *)
    | _ -> game_state

let play_turn game_state =
  let current_player = Game_state.get_current_player game_state in
  Printf.printf "\n=== %s's turn ===\n" (get_name current_player);

  (* Draw two cards *)
  print_endline "\nDrawing 2 cards...";
  let state_after_draws = Game_state.draw_card game_state in
  let final_draw_state = Game_state.draw_card state_after_draws in

  (* (* Show current hand *) let current_player_updated =
     Game_state.get_current_player final_draw_state in print_endline "\nYour
     current hand:"; print_hand (get_hand current_player_updated); *)

  (* Play up to 3 cards phase *)
  let final_state = play_cards_phase final_draw_state 0 in

  (* End turn *)
  next_turn final_state

let rec game_loop game_state =
  (* Get current player *)
  let current_player = Game_state.get_current_player game_state in

  (* Check win condition before the turn *)
  if Game_state.check_win_condition current_player then (
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

  (* Initialize players *)
  print_string "Enter name for Player 1: ";
  let player1_name = read_line () in
  print_string "Enter name for Player 2: ";
  let player2_name = read_line () in

  let player1 = Player.init_player player1_name in
  let player2 = Player.init_player player2_name in

  (* Initialize game state *)
  let initial_state = Game_state.init_game [ player1; player2 ] in

  (* Start game loop *)
  print_endline "\nGame starting...\n";
  game_loop initial_state
;;

main ()
