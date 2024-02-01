open Utils;;
open Types;;

let choose_card = function
  | [] -> failwith "choice"
  | l ->
    let card = List.nth l (Random.int (len l)) in
    card.id
;;

let extract_card id = function
  | [] -> failwith "extract"
  | l ->
    let card = hd (filter (fun c -> c.id = id) l) in
    card, filter (fun c -> c.id <> id) l
;;

let take_card list_card =
  let id = choose_card list_card in
  extract_card id list_card
;;