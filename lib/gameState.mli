type game_state

val init_game : Player.player list -> game_state
(** [init_game starting_players] initializes the game with a list of
    [starting_players], with [curr_player_index] at 0 (starting with the first
    player) and initializes and shuffles the [deck] of cards *)

val next_turn : game_state -> game_state
(** [next_turn game_state] advances the game state by returning a new
    [game_state] with the [curr_player_index] updated to the next player *)

val check_win_condition : Player.player -> bool
(** [check_win_condition player] checks if the given [player] has met the win
    condition, returns [true] if the player has achieved 3 full property sets,
    [false] otherwise *)

val play_card : game_state -> Player.player -> Deck.card -> bool -> game_state
(** [play_card game_state card player] plays the given [card] for the specified
    [player], it returns a new [game_state] with any updates resulting from the
    card effect (e.g., adding properties or banking money), the [card] is
    removed from the player's hand in the updated game state. *)

val draw_card : game_state -> game_state
(** [draw_card game_state] allows the current player to draw a card from the
    [deck], returns a new [game_state] with the drawn card added to the player's
    hand and the [deck] updated accordingly *)

val get_current_player : game_state -> Player.player
val get_players : game_state -> Player.player list
val get_current_player_index : game_state -> int
