type player

val init_player : string -> player

val add_to_hand : player -> Deck.card -> player

(* val play_card : player -> card -> game_state -> game_state *)

val bank_money : player -> int -> player

