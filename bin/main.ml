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

(* 

   print_hand

   get_card_choice

   play_turn

   game_loop

   () - continously calls next turn until *)
