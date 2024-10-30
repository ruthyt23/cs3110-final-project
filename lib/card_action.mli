open Player
open Deck

val deal_breaker :
  player -> player -> (string * string) list -> string -> player * player
(** Steal full set of properties from another player *)

val forced_deal :
  player -> player -> string * string -> string * string -> player * player
(** Force property swap between two players *)

val sly_deal : player -> player -> string * string -> player * player
(** Forces a player to give a card to another player. *)

val debt_collector : player -> player -> int -> player * player
(** Forces a player to pay a debt *)

val pass_go : player -> card list -> player
(** Draw two cards *)

val its_my_birthday : player -> player list -> player list
(** Forces all players to pay the current player. *)
