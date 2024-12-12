open Bogue
module L = Layout
module W = Widget

val create_property_card : string -> Draw.color -> bool -> bool -> L.t
val create_vertical_cards : L.t list -> L.t
val create_horizontal_cards : L.t list -> L.t
val organize_table_cards : L.t list -> L.t
val card_info : unit -> (int, string * Layout.t * Draw.color) Hashtbl.t
val create_overlapping_cards : Layout.t list -> Layout.t
val create_deck : unit -> Layout.t list
