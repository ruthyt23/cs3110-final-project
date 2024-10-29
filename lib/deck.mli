type card = 
  | Action of string
  | Property of string * string
  | Money of int

val init_deck : unit -> card list
val draw_card : card list -> (card * card list)
val shuffle_deck : card list -> card list