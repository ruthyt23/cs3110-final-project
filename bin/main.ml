open Project3110
open Player
open GameState
open Deck
open Scene
open Bogue
open Lwt.Infix

let () =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         print_endline "\n Qutting Program ...";
         exit 0))

let card_border =
  Style.(mk_border ~radius:8 (mk_line ~width:20 ~color:(255, 255, 255, 255) ()))

let name_border =
  Style.(mk_border ~radius:8 (mk_line ~width:2 ~color:(0, 0, 0, 255) ()))

let property_border =
  Style.(mk_border ~radius:100 (mk_line ~width:2 ~color:(0, 0, 0, 255) ()))

let white_bg = Style.(color_bg (255, 255, 255, 255))

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
  let rec get_choice () =
    print_string "Choose a card to play (enter number) or -1 to skip: ";
    try
      let choice = read_int () in
      if choice = -1 || (choice >= 0 && choice < List.length hand) then choice
      else (
        print_endline "Invalid card number. Please try again.";
        get_choice ())
    with
    | Failure _ ->
        print_endline "Please enter a valid number";
        get_choice ()
    | End_of_file ->
        print_endline "\nGame terminated.";
        exit 0
  in
  get_choice ()

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
              let updated_state, additional_cards_played =
                play_card game_state current_player chosen_card false
              in
              play_cards_phase updated_state
                (cards_played + 1 + additional_cards_played)
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

let alt_main () =
  print_endline "Welcome to Monopoly Deal!\n";

  let rec get_num_players () =
    print_string "Enter number of players (2-5): ";
    try
      let num = read_int () in
      if num < 2 || num > 5 then (
        print_endline "Number of players must be between 2 and 5";
        get_num_players ())
      else num
    with
    | Failure _ ->
        print_endline "Please enter a valid number";
        print_endline "";
        get_num_players ()
    | End_of_file ->
        print_endline "\nGame terminated.";
        exit 0
  in

  let num_players = get_num_players () in

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

let gui_main () =
  let left_cards = ref (create_deck ()) in
  let right_cards = ref [] in

  (* Reduce overall container size *)
  let container_width = 1200 in
  let container_height = 600 in

  let main_container =
    W.box ~w:container_width ~h:container_height
      ~style:
        Style.(
          create
            ~background:(color_bg (100, 100, 100, 255))
            ~border:
              (mk_border ~radius:12 (mk_line ~width:3 ~color:(0, 0, 0, 255) ()))
            ())
      ()
  in

  let left_section =
    W.box
      ~w:(container_width * 50 / 100)
      ~h:container_height
      ~style:
        Style.(
          create
            ~background:(color_bg (80, 80, 80, 255))
            ~border:
              (mk_border ~radius:8 (mk_line ~width:2 ~color:(0, 0, 0, 255) ()))
            ())
      ()
  in

  let table_section =
    W.box
      ~w:(container_width * 50 / 100)
      ~h:container_height
      ~style:
        Style.(
          create
            ~background:(color_bg (120, 120, 120, 255))
            ~border:
              (mk_border ~radius:8 (mk_line ~width:2 ~color:(0, 0, 0, 255) ()))
            ())
      ()
  in

  let left_content =
    L.superpose
      [ L.resident left_section; create_overlapping_cards !left_cards ]
  in

  (* Create the cards layout with size constraints *)
  let right_cards_layout = organize_table_cards !right_cards in
  L.set_width right_cards_layout (container_width * 45 / 100);
  L.set_height right_cards_layout container_height;

  (* Create a scrollable view with smaller dimensions *)
  let scrollable_view =
    L.make_clip
      ~w:(container_width * 45 / 100)
      ~h:(container_height - 20) (* Leave room for scrollbar *)
      right_cards_layout
  in

  (* Position the scrollable view *)
  L.setx scrollable_view 10;
  (* Add some padding from the left *)
  L.sety scrollable_view 10;

  (* Add some padding from the top *)

  (* Combine the background and scrollable content *)
  let right_content =
    L.superpose [ L.resident table_section; scrollable_view ]
  in

  let final_layout =
    L.superpose ~center:true
      [ L.resident main_container; L.flat [ left_content; right_content ] ]
  in

  let board = Bogue.of_layout final_layout in

  (* Listen for card clicks and show popup *)
  let _ =
    Lwt.async (fun () ->
        let rec loop () =
          print_endline "\n=== Async Loop Start ===";
          print_endline "Waiting for card click...";
          flush stdout;

          !card_promise >>= fun (name, bg_color) ->
          (* Modified to receive just name and color *)
          print_endline ("Received click for: " ^ name);
          flush stdout;

          let popup_card =
            create_property_card ~name ~bg_color ~is_popup:true ~clickable:false
          in
          print_endline "Created popup card";
          flush stdout;

          (* Create a new promise that will be resolved when the popup is
             closed *)
          let close_promise, close_resolver = Lwt.task () in
          print_endline "Created close promise";
          flush stdout;

          (* Create buttons *)
          let play_button =
            W.button ~border_radius:3
              ~border_color:Draw.(opaque green)
              "Play Card"
          in
          let cancel_button =
            W.button ~border_radius:3 ~border_color:Draw.(opaque red) "Cancel"
          in

          let button_box =
            W.box ~w:100 ~h:40
              ~style:
                Style.(
                  create
                    ~background:(color_bg (200, 200, 200, 255))
                    ~border:
                      (mk_border ~radius:4
                         (mk_line ~width:2 ~color:(0, 0, 0, 255) ()))
                    ())
              ()
          in

          let buttons_layout =
            L.flat ~sep:20
              [
                L.superpose ~center:true
                  [ L.resident button_box; L.resident play_button ];
                L.superpose ~center:true
                  [ L.resident button_box; L.resident cancel_button ];
              ]
          in

          (* Combine card and buttons *)
          let popup_content =
            L.tower ~align:Center ~sep:10 [ popup_card; buttons_layout ]
          in

          (* Create final popup layout *)

          (* Create screen with background and attach popup *)
          let screen =
            Popup.attach
              ~bg:Draw.(set_alpha 180 black)
                (* Semi-transparent black background *)
              final_layout popup_content
          in

          (* Handle cancel button *)
          let handle_cancel _b =
            L.set_show screen false;
            L.set_show popup_content false;
            popup_active := false;
            let new_promise, new_resolver = Lwt.task () in
            card_promise := new_promise;
            card_resolver := new_resolver;
            Lwt.wakeup close_resolver ()
          in

          (* Handle play button *)
          let update_layouts () =
            print_endline "\n=== update_layouts start ===";
            print_endline
              ("Right cards count: " ^ string_of_int (List.length !right_cards));

            let left_layout = create_overlapping_cards !left_cards in

            (* List.iter (fun widget -> L.set_width widget (int_of_float(400.0
               *. 0.8)); L.set_height widget (int_of_float(628.0 *. 0.8)) )
               !right_cards; *)
            let right_cards_layout = organize_table_cards !right_cards in

            L.set_width right_cards_layout (container_width * 45 / 100);

            let scrollable_view =
              L.make_clip
                ~w:(container_width * 45 / 100)
                ~h:(container_height - 20) right_cards_layout
            in

            (* L.setx scrollable_view 10; L.sety scrollable_view 10; *)
            L.set_rooms left_content [ L.resident left_section; left_layout ];

            L.set_rooms right_content
              [ L.resident table_section; scrollable_view ];

            print_endline "=== update_layouts end ===\n"
          in

          let handle_play _b =
            print_endline ("Trying to remove card: " ^ name);

            (* Filter out only the clicked card, keeping all others *)
            left_cards :=
              List.filter
                (fun layout ->
                  let keep =
                    Hashtbl.fold
                      (fun _id (card_name, card_layout, _) acc ->
                        if layout == card_layout then card_name <> name else acc)
                      card_info true
                  in
                  keep)
                !left_cards;

            print_endline
              ("Number of cards after filtering: "
              ^ string_of_int (List.length !left_cards));

            let right_card =
              create_property_card ~name ~bg_color ~is_popup:false
                ~clickable:false
            in

            (* Store the new card's information in card_info *)
            let card_id = !next_card_id in
            incr next_card_id;
            Hashtbl.add card_info card_id (name, right_card, bg_color);

            (* Append to right_cards instead of replacing *)
            right_cards := !right_cards @ [ right_card ];

            print_endline
              ("Number of right cards: "
              ^ string_of_int (List.length !right_cards));

            update_layouts ();
            handle_cancel _b
          in

          (* Initial layout setup *)
          update_layouts ();

          W.on_button_release ~release:handle_cancel cancel_button;
          W.on_button_release ~release:handle_play play_button;

          (* Wait for popup to be closed before continuing the loop *)
          close_promise >>= fun () ->
          print_endline "=== Async Loop End ===\n";
          flush stdout;
          loop ()
        in
        loop ())
  in

  Bogue.run board

let display_welcome_screen () =
  let message =
    {|
  ╔════════════════════════════════════════════════╗
  ║             Welcome to Monopoly Deal            ║
  ║                                                ║
  ║  🎮  A fast-paced card game of property trading ║
  ║  💰  Collect properties, charge rent, and win!  ║
  ║  🏆  Be the first to collect 3 full sets       ║
  ║                                                ║
  ║         Press Enter to start the game...       ║
  ╚════════════════════════════════════════════════╝
  |}
  in
  print_endline message;
  let _ = read_line () in
  ()

let () =
  display_welcome_screen ();
  (* can also run gui_main here, gui version*)
  alt_main ()
