open Utils;;
open Types;;

let parse_qty cp = function
  | [str_num] when is_int str_num -> int_of_string str_num
  | [str_num; "count"; str_color] when (is_int str_num) && (is_color str_color) ->
    let num = int_of_string str_num in
    let color = new_color str_color in
    let nb = len (filter (fun c -> c.color = color) cp.p1.cards) in
    num * nb
  | [str_num; "max"; str_color] when (is_int str_num) && (is_color str_color) ->
    let num = int_of_string str_num in
    let color = new_color str_color in
    let nb1 = len (filter (fun c -> c.color = color) cp.p1.cards) in
    let nb2 = len (filter (fun c -> c.color = color) cp.p2.cards) in
    num * (max nb1 nb2)
  | _ -> failwith "parse_qty"
;;

let steal = function
  | ["bank"] -> fun cp -> {cp with p1 = {cp.p1 with money = cp.p1.money + cp.bank}}
  | "discard"::str_color when forall is_color str_color ->
    fun cp ->
      let colors = foreach new_color str_color in
      let list_cards = filter (fun c -> is_in c.color colors) cp.discard in
      let card, discard = Ui.take_card list_cards in
      {cp with
        p1 = {cp.p1 with cards = card::cp.p1.cards};
        discard = discard
      }
  | llll -> failwith ("steal : " ^ (String.concat " " llll))
;;

let get = function
  | "#"::qty ->
    fun cp ->
      let num = parse_qty cp qty in
      {cp with p1 = {cp.p1 with success = cp.p1.success + num}}
  | "$"::qty ->
    fun cp ->
      let num = parse_qty cp qty in
      {cp with p1 = {cp.p1 with money = cp.p1.money + num}}
  | "!"::qty ->
    fun cp ->
      let num = parse_qty cp qty in
      {cp with p1 = {cp.p1 with military = cp.p1.military + num}}
  | ["="; str] ->
    let s = new_science str in
    fun cp -> {cp with p1 = {cp.p1 with sciences = s::cp.p1.sciences}}
  | "from"::l -> steal l
  | str_l ->
    let r = new_ressource str_l in
    fun cp -> {cp with p1 = {cp.p1 with ressources = r::cp.p1.ressources}}
;;

let replay = function
  | [] -> fun cp -> {cp with replaying = true}
  | ["when"; str_color] when is_color str_color ->
    fun cp -> {
      cp with
      p1 = {
        cp.p1 with
        effects_when_played = (new_color str_color, Instant (fun cp -> {cp with replaying = true}))::cp.p1.effects_when_played
      }
    }
  | llll -> failwith ("replay : " ^ (String.concat " " llll))
;;

let rob = function
  | "#"::qty ->
    fun cp ->
      let num = parse_qty cp qty in
      {cp with p2 = {cp.p2 with success = if cp.p2.success >= num then cp.p2.success - num else 0}}
  | "$"::qty ->
    fun cp ->
      let num = parse_qty cp qty in
      {cp with p2 = {cp.p2 with money = if cp.p2.money >= num then cp.p2.money - num else 0}}
  | "!"::qty ->
    fun cp ->
      let num = parse_qty cp qty in
      {cp with p2 = {cp.p2 with military = if cp.p2.military >= num then cp.p2.military - num else 0}}
  | [str_color] when is_color str_color ->
    fun cp ->
      let color = new_color str_color in
      let list_cards = filter (fun c -> c.color = color) cp.p2.cards in
      let card, cards = Ui.take_card list_cards in
      {cp with
        p2 = {cp.p2 with cards = cards};
        discard = card::cp.discard
      }
  | llll -> failwith ("rob : " ^ (String.concat " " llll))
;;

let parse_effect = function
  | "get"::l -> get l
  | "replay"::l -> replay l
  | "rob"::l -> rob l
  | llll -> failwith ("parse_effect : " ^ (String.concat " " llll))
;;