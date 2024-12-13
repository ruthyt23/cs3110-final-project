open Bogue
module L = Layout
module W = Widget

val create_property_card :
  name:string -> bg_color:Draw.color -> is_popup:bool -> clickable:bool -> L.t
(** [create_property_card name color is_vertical is_face_up] creates a property
    card with the given [name], [color], orientation ([is_vertical]), and face
    status ([is_face_up]). Returns a Layout.t representing the card. *)

val create_vertical_cards : L.t list -> L.t
(** [create_vertical_cards cards] arranges a list of card layouts vertically.
    Returns a single Layout.t containing all cards stacked top to bottom. *)

val create_horizontal_cards : L.t list -> L.t
(** [create_horizontal_cards cards] arranges a list of card layouts
    horizontally. Returns a single Layout.t containing all cards arranged left
    to right. *)

val organize_table_cards : L.t list -> L.t
(** [organize_table_cards cards] arranges a list of card layouts in a grid
    pattern suitable for table display. Returns a single Layout.t containing the
    organized cards. *)

val card_info : (int, string * Layout.t * (int * int * int * int)) Hashtbl.t
(** [card_info ()] returns a hashtable mapping card IDs to tuples containing the
    card's name, layout, and color. Used for managing card state and properties.
*)

val create_overlapping_cards : Layout.t list -> L.t
(** [create_overlapping_cards cards] creates a layout where cards partially
    overlap each other, typically used for displaying cards in hand. Returns a
    single Layout.t. *)

val create_deck : unit -> Layout.t list
(** [create_deck ()] creates a new deck of property cards. Returns a list of
    Layout.t representing all cards in the deck. *)

val card_promise : (string * (int * int * int * int)) Lwt.t ref
(** [card_promise] is a reference to an Lwt promise that will resolve to a tuple
    containing a card name and its RGBA color values. Used for handling
    asynchronous card selection events. *)

val card_resolver : (string * (int * int * int * int)) Lwt.u ref
(** [card_resolver] is a reference to the resolver for [card_promise]. Used to
    fulfill the promise when a card selection is made. *)

val popup_active : bool ref
(** [popup_active] is a reference tracking whether a card selection popup is
    currently being displayed. Used to prevent multiple popups from appearing
    simultaneously. *)

val next_card_id : int ref
(** [next_card_id] is a reference maintaining the next available unique
    identifier for cards. Incremented each time a new card is created. *)
