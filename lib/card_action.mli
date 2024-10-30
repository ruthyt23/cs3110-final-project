type card =
  | Action of string
  | Property of string * string
  | Money of int

type player

val deal_breaker :
  player -> player -> (string * string) list -> string -> player * player
(** [deal_breaker player1 player2 properties color] steals a full [color] set of
    [properties] from [player2] and gives it to [player1]. *)

val forced_deal :
  player -> player -> string * string -> string * string -> player * player
(** [forced_deal player1 player2 property1 property2] forces a property swap of
    [property1] and [property2] between [player1] and [player2]. *)

val sly_deal : player -> player -> string * string -> player * player
(** [sly_deal player1 player2 card] forces [player2] to give [card] to
    [player2]. *)

val debt_collector : player -> player -> int -> player * player
(** [debt_collector player1 player2 debt] forces [player1] to pay [debt] to
    [player2]. *)

val pass_go : player -> Deck.card list -> player
(** [pass_go player1 deck] allows [player1] to draw two cards from [deck]. *)

val its_my_birthday : player -> player list -> player list
(** [its_my_birthday birthday_player players] forces each player from [players]
    to pay the [birthday_player]. *)
