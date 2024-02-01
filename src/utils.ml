let is_int s =
  try ignore (int_of_string s); true
  with Failure _ -> false
;;

let hd = function
  | [] -> failwith "hd"
  | e::_ -> e
;;

let tl = function
  | [] -> []
  | _::l -> l
;;

let rec filter f = function
  | [] -> []
  | e::l when f e -> e::(filter f l)
  | e::l -> filter f l
;;

let rec exists f = function
  | [] -> false
  | e::l when f e -> true
  | e::l -> exists f l
;;

let rec foreach f = function
  | [] -> []
  | e::l -> (f e)::(foreach f l)
;;

let len l =
  let rec aux acc = function
    | [] -> acc
    | _::l -> aux (acc + 1) l
  in
  aux 0 l
;;

let split c s =
  let s = String.split_on_char c s in
  let rec aux = function
    | [] -> []
    | ""::l -> aux l
    | e::l -> e::(aux l)
  in
  aux s
;;

let rec forall f = function
  | [] -> true
  | e::l when f e -> forall f l
  | _ -> false
;;

let is_in e l = exists (fun e' -> e = e') l;;

let shuffle l =
  let () = Random.self_init () in
  let rec extract n = function
    | [] -> failwith "extract"
    | e::l when n = 0 -> (e, l)
    | e::l -> let (e', l') = extract (n - 1) l in (e', e::l')
  in
  let rec aux = function
    | [] -> []
    | l -> let (e, l') = extract (Random.int (len l)) l in e::(aux l')
  in
  aux l
;;

let list_of_hashmap h =
  let rec aux acc = function
    | [] -> acc
    | (k, v)::l -> aux (v::acc) l
  in
  aux [] (Hashtbl.to_seq h |> List.of_seq)
;;