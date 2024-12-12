type card =
  | Action of string
  | Property of string * string
  | Money of int

val init_deck : unit -> card list
(** [init_deck ()] creates a new deck of cards, using the default property,
    action and bank cards. *)

val draw_card : card list -> card * card list
(** [draw_card deck] draws a card from the deck, returning the card and updated
    deck. *)

val shuffle_deck : card list -> card list
(** [shuffle_deck deck] returns a new shuffled deck. *)
