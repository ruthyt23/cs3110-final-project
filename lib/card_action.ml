open Deck
include Player

(** Steal full set of properties from another player based on the color they say
    they want *)
let deal_breaker (pl1 : player) (pl2 : player) prop_lst (color : string) =
  failwith "todo"

(** [forced_deal pl1 pl2 card1 card2] Forces pl2 to give pl1 the given [card]
    and returns the tuple (pl1, pl2) with updated properties for each player.
    Pl2 must have the card given in order for the action.*)
let forced_deal (pl1 : player) (pl2 : player) (card1 : string * string)
    (card2 : string * string) : player * player =
  let pl1_props = Player.add_property pl1 card1 in
  let pl2_props = Player.add_property pl2 card2 in
  let new_pl1 = Player.remove_property pl1_props card2 in
  let new_pl2 = Player.remove_property pl2_props card1 in
  (new_pl1, new_pl2)

(** [sly_deal pl1 pl2 card] Forces pl2 to give pl1 the given [card] and returns
    the tuple (pl1, pl2) with updated properties for each player. Pl2 must have
    the card given in order for the action.*)
let sly_deal pl1 pl2 card =
  let new_pl1 = Player.add_property pl1 card in
  let new_pl2 = Player.remove_property pl2 card in
  (new_pl1, new_pl2)

(** [debt_collector pl1 pl2 amount] Forces pl2 to give pl1 the given [amount].
    pl2 must have a bank amount > 0 and returns the pair (pl1, pl2) with the
    updated bank amounts for each player *)
let debt_collector pl1 pl2 amount =
  let upd_pl1 = Player.bank_money pl1 amount in
  let upd_pl2 = Player.remove_from_bank pl2 amount in
  (upd_pl1, upd_pl2)

(** [pass_go player card_lst ] Gives two cards to the player and removes 2 cards
    from the deck*)
let pass_go player card_lst =
  let add_fst_card_player = Player.add_to_hand player (List.hd card_lst) in
  let snd_card_lst = List.tl card_lst in
  let add_snd_card_player =
    Player.add_to_hand add_fst_card_player (List.hd snd_card_lst)
  in
  add_snd_card_player

(** [its_my_birthday] player pl_lst Gives [player] two dollars from everyone's
    deck who's amount is >=0 and then removes 2 dollars from each player in
    [pl_lst] *)
let its_my_birthday player pl_lst =
  let remv_pl = List.filter (fun elem -> elem <> player) pl_lst in
  let upd_player = Player.bank_money player (List.length pl_lst * 2) in
  let new_pl_lst =
    List.map (fun player -> Player.remove_from_bank player 2) remv_pl
  in
  List.cons upd_player new_pl_lst
