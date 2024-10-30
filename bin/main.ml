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
