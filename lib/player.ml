type player = {
  name : string;
  hand : Deck.card list;
  bank : int;
  properties: (string * string list) list;
}

let init_player name = {
  name = name;
  hand = [];
  bank = 0;
  properties = [];
}

let add_to_hand player card = {
  player with hand = card :: player.hand
}

let bank_money player amount = {
  player with bank = player.bank + amount
}

(* 
let play_card player card game_state = {
  game_state with next_turn = player 
} *)