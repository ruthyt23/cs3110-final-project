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

let main () =
  print_endline "Welcome to Monopoly Deal!\n";
  let player1 = Player.init_player "player 1" in
  let player2 = Player.init_player "player 2" in
  let players = [ player1; player2 ] in
  let _ = Game_state.init_game players in
  while List.length (List.filter check_win_condition players) = 0 do
    print_endline "something"
  done;
  print_endline "WINNERS: ";
  List.filter check_win_condition players
  |> List.iter (fun winner -> print_endline (Player.get_name winner));
  print_endline "Thanks for playing!"
;;

main ()

(* 

   print_hand

   get_card_choice

   play_turn

   game_loop

   () - continously calls next turn until *)
