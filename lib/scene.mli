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

val card_info : (int, string * Layout.t * Draw.color) Hashtbl.t
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
