type player = {
  name : string;
  hand : Deck.card list;
  bank : int;
  properties : (string * string) list;
}

let init_player name = { name; hand = []; bank = 0; properties = [] }
let add_to_hand player card = { player with hand = card :: player.hand }
let bank_money player amount = { player with bank = player.bank + amount }
let remove_from_bank player amount = { player with bank = player.bank - amount }

let add_property player property =
  { player with properties = property :: player.properties }

let remove_property player property =
  {
    player with
    properties = List.filter (fun elem -> elem <> property) player.properties;
  }

let get_name player = player.name
let get_hand player = player.hand
let get_bank player = player.bank
let get_properties player = player.properties

let remove_from_hand player card =
  { player with hand = List.filter (( <> ) card) player.hand }

let get_property_sets player =
  let property_sets =
    [
      ("Brown", [ "Mediterranean Avenue"; "Baltic Avenue" ]);
      ( "Light Blue",
        [ "Oriental Avenue"; "Vermont Avenue"; "Connecticut Avenue" ] );
      ("Pink", [ "St. Charles Place"; "States Avenue"; "Virginia Avenue" ]);
      ("Orange", [ "St. James Place"; "Tennessee Avenue"; "New York Avenue" ]);
      ("Red", [ "Kentucky Avenue"; "Indiana Avenue"; "Illinois Avenue" ]);
      ("Yellow", [ "Atlantic Avenue"; "Ventnor Avenue"; "Marvin Gardens" ]);
      ( "Green",
        [ "Pacific Avenue"; "North Carolina Avenue"; "Pennsylvania Avenue" ] );
      ("Dark Blue", [ "Park Place"; "Boardwalk" ]);
    ]
  in

  List.fold_left
    (fun count (color, props) ->
      if
        List.for_all
          (fun property -> List.mem (color, property) player.properties)
          props
      then count + 1
      else count)
    0 property_sets
