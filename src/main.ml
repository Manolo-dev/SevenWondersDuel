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

let db_cards  = tl (Csv.load ~separator:','  "db/cards.csv")
let db_inputs = tl (Csv.load ~separator:',' "db/inputs.csv")
let db_costs  = tl (Csv.load ~separator:','  "db/costs.csv")

let cards = parse_cards_input db_cards db_inputs db_costs