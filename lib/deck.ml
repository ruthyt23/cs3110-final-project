type card =
  | Action of string
  | Property of string * string
  | Money of int

let init_deck () =
  let property_cards =
    [
      (* brown properties *)
      Property ("Brown", "Mediterranean Avenue");
      Property ("Brown", "Baltic Avenue");
      (* light blue properties *)
      Property ("Light Blue", "Oriental Avenue");
      Property ("Light Blue", "Vermont Avenue");
      Property ("Light Blue", "Connecticut Avenue");
      (* pink properties *)
      Property ("Pink", "St. Charles Place");
      Property ("Pink", "States Avenue");
      Property ("Pink", "Virginia Avenue");
      (* orange properties *)
      Property ("Orange", "St. James Place");
      Property ("Orange", "Tennessee Avenue");
      Property ("Orange", "New York Avenue");
      (* red properties *)
      Property ("Red", "Kentucky Avenue");
      Property ("Red", "Indiana Avenue");
      Property ("Red", "Illinois Avenue");
      (* yellow properties *)
      Property ("Yellow", "Atlantic Avenue");
      Property ("Yellow", "Ventnor Avenue");
      Property ("Yellow", "Marvin Gardens");
      (* green properties *)
      Property ("Green", "Pacific Avenue");
      Property ("Green", "North Carolina Avenue");
      Property ("Green", "Pennsylvania Avenue");
      (* dark blue properties *)
      Property ("Dark Blue", "Park Place");
      Property ("Dark Blue", "Boardwalk");
      (* railroads *)
      Property ("Railroad", "Reading Railroad");
      Property ("Railroad", "Pennsylvania Railroad");
      Property ("Railroad", "B&O Railroad");
      Property ("Railroad", "Short Line");
      (* util *)
      Property ("Utility", "Electric Company");
      Property ("Utility", "Water Works");
    ]
  in
  let money_cards =
    [
      (* 1 $10M money card *)
      Money 10;
      (* 2 $5M money cards *)
      Money 5;
      Money 5;
      (* 3 $4M money cards *)
      Money 4;
      Money 4;
      Money 4;
      (* 3 $3M money cards *)
      Money 3;
      Money 3;
      Money 3;
      (* 5 $2M money cards *)
      Money 2;
      Money 2;
      Money 2;
      Money 2;
      Money 2;
      (* 6 $1M money cards *)
      Money 1;
      Money 1;
      Money 1;
      Money 1;
      Money 1;
      Money 1;
    ]
  in
  let action_cards =
    [
      (* 2 Deal Breaker cards *)
      Action "Deal Breaker";
      Action "Deal Breaker";
      (* 3 Just Say No cards *)
      Action "Just Say No";
      Action "Just Say No";
      Action "Just Say No";
      (* 3 Sly Deal cards *)
      Action "Sly Deal";
      Action "Sly Deal";
      Action "Sly Deal";
      (* 4 Forced Deal cards *)
      Action "Forced Deal";
      Action "Forced Deal";
      Action "Forced Deal";
      Action "Forced Deal";
      (* 3 Debt Collector cards *)
      Action "Debt Collector";
      Action "Debt Collector";
      Action "Debt Collector";
      (* 3 It's My Birthday cards *)
      Action "It's My Birthday";
      Action "It's My Birthday";
      Action "It's My Birthday";
      (* 10 Pass Go cards *)
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      Action "Pass Go";
      (* 3 House cards *)
      Action "House";
      Action "House";
      Action "House";
      (* 3 Hotel cards *)
      Action "Hotel";
      Action "Hotel";
      Action "Hotel";
      (* 2 Double the Rent cards *)
      Action "Double The Rent";
      Action "Double The Rent";
    ]
  in
  property_cards @ money_cards @ action_cards

let draw_card deck =
  match deck with
  | [] -> failwith "deck is empty"
  | card :: rest -> (card, rest)

let shuffle_deck deck =
  Random.self_init ();
  (* Initialize random number generator *)
  let new_deck = List.map (fun c -> (Random.bits (), c)) deck in
  let sorted_deck = List.sort compare new_deck in
  List.map (fun (_, card) -> card) sorted_deck

let discard_pile deck = failwith "unimplemented"
