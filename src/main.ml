open Utils;;

type ressource =
  | Wood
  | Stone
  | Clay
  | Glass
  | Paper
  | Choice of ressource list
  | Payment of ressource * int

and cost =
  | CostWood
  | CostStone
  | CostClay
  | CostGlass
  | CostPaper
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
  wonder: int;
  wonders: card list;
  sciences: sciences list;
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
  token: card list;
}

and party = {
  p1: player;
  p2: player;
  board: board;
  discard: card list;
  replaying: bool;
  to_bank: bool;
  bank: int;
}

let new_card () = {
  id = 0;
  color = Ressource;
  name = "";
  costs = [];
  effects = [];
  age = "";
}

let color_of_string = function
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

let ressource_of_string = function
  |  "wood" -> Wood
  | "stone" -> Stone
  |  "clay" -> Clay
  | "glass" -> Glass
  | "paper" -> Paper
  | r -> failwith ("Unknown ressource : " ^ r)
;;

let science_of_string = function
  |       "wheel" -> Wheel
  |     "compass" -> Compass
  |      "mortar" -> Mortar
  |     "feather" -> Feather
  |       "clock" -> Clock
  | "planetarium" -> Planetarium
  |         "law" -> Law
  | s -> failwith ("Unknown science : " ^ s)
;;

let parse_effect current_card = function
  | [_; effect; duration] -> begin
    (*let rob_money str_num cp =
      {cp with
        p2 = {cp.p2 with
          money = if cp.p2.money > (int_of_string str_num) then
            cp.p2.money - (int_of_string str_num)
          else 0
        }
      }
    in
    let rob_card card_type cp =
      let cards = filter (fun card -> card.color = card_type) cp.p2.cards in
      let card = choose_an_id cards in
      {cp with p2 = {cp.p2 with cards = filter (fun c -> c.id <> card) cp.p2.cards}}
    in
    let payment_ressource r str_num cp =
      {cp with
        p1 = {cp.p1 with
          ressources = (Payment (ressource_of_string r, (int_of_string str_num)))::(cp.p1.ressources)
        }
      }
    in
    let replay cp = {cp with replaying = true} in
    let wonder_to_replay cp =
      {cp with
        p1 = {cp.p1
          with wonders = (
            foreach (
              fun c ->
                if exists ((=) (Instant replay)) c.effects then
                  c
                else
                  {c with effects = ((Instant replay)::c.effects)}
            ) cp.p1.wonders
          )
        }
      }
    in*)
    let effect = split ' ' effect in
    let effect = match effect with
      |       ["wood"] -> fun cp -> {cp with p1 = {cp.p1 with ressources =                        Wood::(cp.p1.ressources)}}
      |      ["stone"] -> fun cp -> {cp with p1 = {cp.p1 with ressources =                       Stone::(cp.p1.ressources)}}
      |       ["clay"] -> fun cp -> {cp with p1 = {cp.p1 with ressources =                        Clay::(cp.p1.ressources)}}
      |      ["glass"] -> fun cp -> {cp with p1 = {cp.p1 with ressources =                       Glass::(cp.p1.ressources)}}
      |      ["paper"] -> fun cp -> {cp with p1 = {cp.p1 with ressources =                       Paper::(cp.p1.ressources)}}
      (*| _::"|"::_ as l -> fun cp -> {cp with p1 = {cp.p1 with ressources = (Choice (ressource_list l))::(cp.p1.ressources)}}
      | ["="; science] -> fun cp -> {cp with p1 = {cp.p1 with   sciences = (science_of_string science)::(cp.p1.sciences  )}}
      |         "$"::l -> fun cp -> {cp with p1 = {cp.p1 with      money =             cp.p1.money    + (parse_value cp l)}}
      |         "!"::l -> fun cp -> {cp with p1 = {cp.p1 with   military =             cp.p1.military + (parse_value cp l)}}
      |         "#"::l -> fun cp -> {cp with p1 = {cp.p1 with    success =             cp.p1.success  + (parse_value cp l)}}
      |        [r; "for"; str_num] -> payment_ressource r str_num
      |                 ["replay"] -> replay
      | ["Wonder"; "->"; "replay"] -> wonder_to_replay
      |   ["rob"; n] when is_num n -> rob_money n
      |                 ["rob"; c] -> rob_card (color_of_string c)
      |                 "steal"::l -> stealing l*)
      | llll -> failwith ("Unknown effect 1 : " ^ (String.concat " " llll))
    in
    match duration with
    | "Instant"   -> {current_card with effects =   (Instant effect)::(current_card.effects)}
    | "Permanent" -> {current_card with effects = (Permanent effect)::(current_card.effects)}
    | "Final"     -> {current_card with effects =     (Final effect)::(current_card.effects)}
    | d -> failwith ("Unknown duration : " ^ d)
  end
  | llll -> failwith ("Unknown effect 2 : " ^ (String.concat " " llll))
;;

let parse_cost current_card = function
  |  [_; "wood"] -> {current_card with costs =                          (CostWood)::(current_card.costs)}
  | [_; "stone"] -> {current_card with costs =                         (CostStone)::(current_card.costs)}
  |  [_; "clay"] -> {current_card with costs =                          (CostClay)::(current_card.costs)}
  | [_; "glass"] -> {current_card with costs =                         (CostGlass)::(current_card.costs)}
  | [_; "paper"] -> {current_card with costs =                         (CostPaper)::(current_card.costs)}
  | [_; str_num] -> {current_card with costs = (CostMoney (int_of_string str_num))::(current_card.costs)}
  | llll -> failwith ("Unknown cost : " ^ (String.concat " " llll))
;;

let parse_card current_card = function
  | [id; name; color; age] ->
    {
      current_card with
      id        =        int_of_string id;
      name      =                    name;
      color = color_of_string color;
      age       =                     age;
    }
  | llll -> failwith ("Unknown card" ^ (String.concat " " llll))
;;

let parse_cards_input db_cards db_inputs db_costs =
  let hashmap = Hashtbl.create (len db_cards) in
  let rec aux f = function
    | [] -> ()
    | ((id::_) as l)::ll ->
      let _ = (
        match Hashtbl.find_opt hashmap id with
        | Some c -> Hashtbl.replace hashmap id (f c l)
        | None   -> Hashtbl.add     hashmap id (f (new_card ()) l)
      ) in
      aux f ll
    | llll::_ -> failwith ("Unknown line : " ^ (String.concat " " llll))
  in
  let _ = aux   parse_card  db_cards in
  let _ = aux parse_effect db_inputs in
  let _ = aux   parse_cost  db_costs in
  hashmap
;;

let db_cards  = tl (Csv.load ~separator:','  "db/cards.csv")
let db_inputs = tl (Csv.load ~separator:',' "db/inputs.csv")
let db_costs  = tl (Csv.load ~separator:','  "db/costs.csv")

let cards = parse_cards_input db_cards db_inputs db_costs