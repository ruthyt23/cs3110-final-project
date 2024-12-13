open Player
open Deck

val deal_breaker : player -> player -> string list -> string -> player * player
(** [deal_breaker player1 player2 properties color] steals a full [color] set of
    [properties] from [player2] and gives it to [player1]. *)

val forced_deal :
  player -> player -> string * string -> string * string -> player * player
(** [forced_deal player1 player2 property1 property2] forces a property swap of
    [property1] and [property2] between [player1] and [player2]. *)

val sly_deal : player -> player -> string * string -> player * player
(** [sly_deal player1 player2 card] forces [player2] to give [card] to
    [player1]. *)

val debt_collector : player -> player -> int -> player * player
(** [debt_collector player1 player2 debt] forces [player2] to pay [debt] to
    [player1]. *)

val pass_go : player -> card list -> player * card list
(** [pass_go player1 deck] allows [player1] to draw two cards from [deck]. *)

val its_my_birthday : player -> player list -> player list
(** [its_my_birthday birthday_player players] forces each player from [players]
    to pay the [birthday_player]. *)

val charge_rent : player -> player -> string -> int -> player * player
(** [charge_rent player1 player2 card mult] forces [player2] to pay [player1]
    rent for [card], accounting for raised prices due to any present houses and
    hotels on the property - if the player utilizes "Double the Rent" action
    cards, the rent is multiplied by the given [mult]. *)

val add_house : player -> string -> player
(** [add_house player color] adds a house to [player]'s full [color] property
    set. *)

val add_hotel : player -> string -> player
(** [add_hotel player color] adds a hotel to [player]'s full [color] property
    set. Requires that [color] already has a house present. *)
