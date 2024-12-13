open Deck
include Player

(** [forced_deal pl1 pl2 p1_gives p2_gives] Forces a property swap:
    - pl1 gives up [p1_gives] and receives [p2_gives]
    - pl2 gives up [p2_gives] and receives [p1_gives] Returns (pl1, pl2) with
      their updated properties. *)
let forced_deal (pl1 : player) (pl2 : player) (p1_gives : string * string)
    (p2_gives : string * string) : player * player =
  (* Remove properties from their original owners *)
  let pl1_without_property = Player.remove_property pl1 p1_gives in
  let pl2_without_property = Player.remove_property pl2 p2_gives in

  (* Give each player the property from the other player *)
  let pl1_with_new_property =
    Player.add_property pl1_without_property p2_gives
  in
  let pl2_with_new_property =
    Player.add_property pl2_without_property p1_gives
  in

  (pl1_with_new_property, pl2_with_new_property)

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
  let actual_amount = min (Player.get_bank pl2) amount in
  let upd_pl2 = Player.remove_from_bank pl2 actual_amount in
  let upd_pl1 = Player.bank_money pl1 actual_amount in
  (upd_pl1, upd_pl2)

(** [pass_go player card_lst ] Gives two cards to the player and removes 2 cards
    from the deck*)
let pass_go player card_lst =
  let add_fst_card_player = Player.add_to_hand player (List.hd card_lst) in
  let snd_card_lst = List.tl card_lst in
  let add_snd_card_player =
    Player.add_to_hand add_fst_card_player (List.hd snd_card_lst)
  in
  (add_snd_card_player, List.tl snd_card_lst)

(** [its_my_birthday] player pl_lst Gives [player] two dollars from everyone's
    deck who's amount is >=0 and then removes 2 dollars from each player in
    [pl_lst] *)

let its_my_birthday player pl_lst =
  let total_collected, updated_other_players =
    List.fold_left
      (fun (total, acc) p ->
        if Player.get_name p <> Player.get_name player then
          let actual_amount = min (Player.get_bank p) 2 in
          let updated_player = Player.remove_from_bank p actual_amount in
          (total + actual_amount, updated_player :: acc)
        else (total, p :: acc))
      (0, []) pl_lst
  in
  let updated_player = Player.bank_money player total_collected in
  List.map
    (fun p ->
      if Player.get_name p = Player.get_name player then updated_player else p)
    (List.rev updated_other_players)

(** Steal full set of properties from another player based on the color they say
    they want. *)
let deal_breaker (pl1 : player) (pl2 : player) prop_lst (color : string) =
  List.fold_left
    (fun (curr_pl1, curr_pl2) curr_prop ->
      sly_deal curr_pl1 curr_pl2 (color, curr_prop))
    (pl1, pl2) prop_lst

let charge_rent pl1 pl2 color mult =
  let count = property_count (get_properties pl1) color in
  let rent_charges = Deck.property_rent color in
  let house_and_hotel =
    let house_and_hotel_count =
      List.fold_left
        (fun acc (curr_color, _) -> if color = curr_color then acc + 1 else acc)
        0
        (Player.get_house_and_hotel pl1)
    in
    if house_and_hotel_count = 1 then 3
    else if house_and_hotel_count = 2 then 7
    else 0
  in
  let rent_amt = (List.nth rent_charges (count - 1) * mult) + house_and_hotel in
  print_string
    ("Rent of $" ^ string_of_int rent_amt ^ "M charged to " ^ get_name pl2);
  debt_collector pl1 pl2 rent_amt

let add_house = Player.add_house
let add_hotel = Player.add_hotel
