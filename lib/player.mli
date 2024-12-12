type player

val init_player : string -> player
(** [init_player name] is a player with [name] name, an empty list as their
    hand, 0 as their starting [bank], and an empty list as their [properties]*)

val add_to_hand : player -> Deck.card -> player
(** [add_to_hand player card] is a player with an updated [hand] thats has card*)

val remove_from_hand : player -> Deck.card -> player
(** [remove_from_hand player card] is a player with an updated [hand] that has
    card removed. If the card is not inside the deck the original deck is
    returned.*)

val bank_money : player -> int -> player
(** [bank_money player amount] returns a new player with the [amount] added to
    their [bank] *)

val remove_from_bank : player -> int -> player
(** [remove_from_bank player amount] returns a new player with [amount] removed
    from their bank*)

val add_property : player -> string * string -> player
(** [add_property player property] returns a new player with the given
    [property] added to their [properties] *)

val remove_property : player -> string * string -> player
(** [remove_property player property] returns a new player with the given
    [property] removed from their [properties] *)

val get_name : player -> string
(** [get_name player] returns the name of the player*)

val property_sets : (string * string list) list
(** The property sets separated by color/function. *)

val get_property_sets : player -> int
(** [get_property_sets player] returns the number of full property sets the
    player has *)

val get_hand : player -> Deck.card list
(** [get_hand player] returns the list of cards currently in the player's hand *)

val get_bank : player -> int
(** [get_bank player] returns the amount of money in the player's bank *)

val get_properties : player -> (string * string) list
(** [get_properties player] returns the list of properties the player owns *)

val card_count : player -> Deck.card -> int
(** [card_count player card] returns the number of [card]s that [player] has. *)

val card_check : player -> Deck.card -> bool
(** [card_check player card] checks if [player] has at least one of [card]. *)
