open Utils;;
open Types;;

let parse_effect current_card = function
  | [_; effect; duration] -> begin
    let effect = split ' ' effect in
    let effect = MicroLanguage.parse_effect effect in
    match duration with
    | "Instant"   -> {current_card with effects =   (Instant effect)::(current_card.effects)}
    | "Permanent" -> {current_card with effects = (Permanent effect)::(current_card.effects)}
    | "Final"     -> {current_card with effects =     (Final effect)::(current_card.effects)}
    | d -> failwith ("Unknown duration : " ^ d)
  end
  | llll -> failwith ("Unknown effect 2 : " ^ (String.concat " " llll))
;;

let parse_cost current_card = function
  | [_; str_num] when is_int str_num -> {current_card with costs = (CostMoney (int_of_string str_num))::(current_card.costs)}
  |     [_; str]                     -> {current_card with costs =                  (CostResource str)::(current_card.costs)}
  | llll -> failwith ("Unknown cost : " ^ (String.concat " " llll))
;;

let parse_card current_card = function
  | [id; name; color; age] ->
    {
      current_card with
      id        =        int_of_string id;
      name      =                    name;
      color = new_color color;
      age       =                     age;
    }
  | llll -> failwith ("Unknown card : " ^ (String.concat " " llll))
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

let  db_cards = tl (Csv.load ~separator:','  "db/cards.csv")
let db_inputs = tl (Csv.load ~separator:',' "db/inputs.csv")
let  db_costs = tl (Csv.load ~separator:','  "db/costs.csv")

let cards = list_of_hashmap (parse_cards_input db_cards db_inputs db_costs)
let    ageI = shuffle (filter (fun c -> c.age =   "I") cards)
let   ageII = shuffle (filter (fun c -> c.age =  "II") cards)
let  ageIII = shuffle (filter (fun c -> c.age = "III") cards)
let  guilds = shuffle (filter (fun c -> c.age =   "G") cards)
let wonders = shuffle (filter (fun c -> c.age =   "W") cards)
let  tokens = shuffle (filter (fun c -> c.age =   "T") cards)

let                   ageI = match    ageI with |           _::_::_::  ageI ->              ageI | _ -> failwith "Not enough cards in age I";;
let                  ageII = match   ageII with |           _::_::_:: ageII ->             ageII | _ -> failwith "Not enough cards in age II";;
let                 ageIII = match  ageIII with |           _::_::_::ageIII ->            ageIII | _ -> failwith "Not enough cards in age III";;
let                 guilds = match  guilds with |           a::b::c::_      ->           [a;b;c] | _ -> failwith "Not enough cards in guilds";;
let                wonders = match wonders with | a::b::c::d::e::f::g::h::_ -> [a;b;c;d;e;f;g;h] | _ -> failwith "Not enough cards in wonders";;
let                 tokens = match  tokens with | a::b::c::d::e::f::g::h::_ -> [a;b;c;d;e;f;g;h] | _ -> failwith "Not enough cards in tokens";;
let discard_tokens, tokens = match  tokens with |           a::b::c::tokens ->   [a;b;c], tokens | _ -> failwith "Not enough tokens";;

let  ageIII = shuffle (ageIII @ guilds);;

let init () =
  let b = {age1 = ageI; age2 = ageII; age3 = ageIII; tokens = tokens} in
  let p1 = new_player () in
  let p2 = new_player () in

  let p1 = {p1 with money = 7} in
  let p2 = {p2 with money = 7} in

  let w, wonders = Ui.take_card wonders in
  let p1 = {p1 with wonders = w::p1.wonders} in
  let w, wonders = Ui.take_card wonders in
  let p2 = {p2 with wonders = w::p2.wonders} in
  let w, wonders = Ui.take_card wonders in
  let p2 = {p2 with wonders = w::p2.wonders} in
  let w, wonders = Ui.take_card wonders in
  let p1 = {p1 with wonders = w::p1.wonders} in

  let w, wonders = Ui.take_card wonders in
  let p2 = {p2 with wonders = w::p2.wonders} in
  let w, wonders = Ui.take_card wonders in
  let p1 = {p1 with wonders = w::p1.wonders} in
  let w, wonders = Ui.take_card wonders in
  let p1 = {p1 with wonders = w::p1.wonders} in
  let w, wonders = Ui.take_card wonders in
  let p2 = {p2 with wonders = w::p2.wonders} in

  let cp = new_party () in
  let cp = {cp with board = b} in
  let cp = {cp with discard = discard_tokens} in
  let cp = {cp with p1 = p1} in
  let cp = {cp with p2 = p2} in
  cp
;;