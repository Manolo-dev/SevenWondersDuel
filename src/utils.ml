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