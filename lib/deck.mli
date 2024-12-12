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

val full_property_count : string -> int
(** [full_property_count color] returns the number of cards in a full set of
    [color] cards.*)

val property_count : (string * string) list -> string -> int
(** [property_count prop_list color] returns the number of [color] cards in the
    given [prop_list]. *)

val property_rent : string -> int list
(** [property_rent color] gives the list of rent prices for a given [color].*)

val just_say_no : card
(** The "Just Say No" action card - used to facilitate action card exchanges. *)

val double_the_rent : card
(** The "Double the Rent" action card - used to check during rent charges. *)
