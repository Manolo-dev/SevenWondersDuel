type ressource =
  | Wood
  | Stone
  | Clay
  | Glass
  | Paper
  | Choice of ressource list
  | Payment of ressource * int

and cost =
  | CostResource of string
  | CostMoney of int

and sciences =
  | Wheel
  | Compass
  | Mortar
  | Feather
  | Clock
  | Planetarium
  | Law

and player = {
  cards: card list;
  ressources: ressource list;
  success: int;
  money: int;
  military: int;
  wonders: card list;
  sciences: sciences list;
  effects_when_played: (color * effect) list;
}

and effect =
  | Instant   of (party -> party)
  | Permanent of (party -> party)
  | Final     of (party -> party)

and color =
  | Ressource
  | RareRessource
  | Success
  | Military
  | Sciences
  | Economy
  | Guild
  | Wonder
  | Token

and card = {
  id: int;
  color: color;
  name: string;
  costs: cost list;
  effects: effect list;
  age: string;
}

and board = {
  age1: card list;
  age2: card list;
  age3: card list;
  tokens: card list;
}

and party = {
  p1: player;
  p2: player;
  board: board;
  discard: card list;
  replaying: bool;
  bank: int;
}

let new_player () = {
  cards = [];
  ressources = [];
  success = 0;
  money = 0;
  military = 0;
  wonders = [];
  sciences = [];
  effects_when_played = [];
}

let new_party () = {
  p1 = new_player ();
  p2 = new_player ();
  board = {
    age1 = [];
    age2 = [];
    age3 = [];
    tokens = [];
  };
  discard = [];
  replaying = false;
  bank = 0;
}

let new_card () = {
  id = 0;
  color = Ressource;
  name = "";
  costs = [];
  effects = [];
  age = "";
}

let is_color = function
  |     "Ressource"
  | "RareRessource"
  |       "Success"
  |      "Military"
  |      "Sciences"
  |       "Economy"
  |         "Guild"
  |        "Wonder"
  |         "Token" -> true
  | _ -> false
;;

let new_color = function
  |     "Ressource" -> Ressource
  | "RareRessource" -> RareRessource
  |       "Success" -> Success
  |      "Military" -> Military
  |      "Sciences" -> Sciences
  |       "Economy" -> Economy
  |         "Guild" -> Guild
  |        "Wonder" -> Wonder
  |         "Token" -> Token
  | t -> failwith ("Unknown type : " ^ t)
;;

let rec parse_list f = function
  |     ["]"] -> []
  | str::rest -> (f str)::(parse_list f rest)
  | _ -> failwith "Invalid list"
;;

let rec new_ressource = function
  |           ["wood"] -> Wood
  |          ["stone"] -> Stone
  |           ["clay"] -> Clay
  |          ["glass"] -> Glass
  |          ["paper"] -> Paper
  |          "["::rest -> Choice (parse_list (fun str -> new_ressource [str]) rest)
  |  ["wood"; str_num] -> Payment (Wood, int_of_string str_num)
  | ["stone"; str_num] -> Payment (Stone, int_of_string str_num)
  |  ["clay"; str_num] -> Payment (Clay, int_of_string str_num)
  | ["glass"; str_num] -> Payment (Glass, int_of_string str_num)
  | ["paper"; str_num] -> Payment (Paper, int_of_string str_num)
  | llll -> failwith ("Invalid ressource : " ^ (String.concat " " llll))
;;

let new_science = function
  |       "wheel" -> Wheel
  |     "compass" -> Compass
  |      "mortar" -> Mortar
  |     "feather" -> Feather
  |       "clock" -> Clock
  | "planetarium" -> Planetarium
  |         "law" -> Law
  | s -> failwith ("Unknown science : " ^ s)
;;