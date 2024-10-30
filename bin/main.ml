open Project3110
open Player
open Game_state
open Deck

(* 

   print_hand

   get_card_choice

   play_turn

   game_loop

   () - continously calls next turn until *)

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

open Project3110

let main () =
  print_endline "Welcome to Monopoly Deal!\n";
  let player1 = Player.init_player "player 1" in
  let player2 = Player.init_player "player 2" in
  let players = [ player1; player2 ] in
  let _ = Game_state.init_game players in
  print_endline "Thanks for playing!"
;;

main ()
