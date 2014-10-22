let (@@) f x = f x
let ($) g f = fun x -> g (f x)
let id x = x
let p = Printf.printf
let pr = print_endline
let tee f x = ignore @@ f x; x
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
let const c = fun _ -> c

let (!%) = Printf.sprintf
let (!$) x = Lazy.force x

let (--) a b =
  let rec iter store a bi =
    if a = bi then bi::store
    else iter (bi::store) a (bi - 1)
  in
  if a <= b then iter [] a b
  else List.rev @@ iter [] b a

let rec repeat n f x =
  if n > 0 then
    (f x; repeat (n-1) f x)
  else ()

let list_concatmap f xs = List.concat (List.map f xs)

let list_head = function
  | [] -> raise (Invalid_argument "list_head: nil")
  | x::_ -> x

let rec list_last = function
  | [] -> raise (Invalid_argument "list_last: nil")
  | [x] -> x
  | _::xs -> list_last xs

let sint   = string_of_int
let sfloat = string_of_float
let sbool  = string_of_bool

let string_foldr f s a0 =
  let rec iter i a =
    if i >= 0 then
      iter (i-1) (f s.[i] a)
    else a
  in
  iter (String.length s - 1) a0

let slist delim show l =
  String.concat delim @@ List.map show l

let chars_of_string s =
  let rec iter n =
    if n >= String.length s then
      []
    else 
      s.[n] :: iter (n+1)
  in 
  iter 0

let string_of_chars = slist "" (String.make 1)

let string1 c = String.make 1 c

let mapi f l =
  List.rev @@ snd @@
  List.fold_left (fun (i,store) b -> (i+1,f i b::store)) (0,[]) l

let iteri f l =
  ignore @@ List.fold_left (fun i x -> f i x; (i+1)) 0 l

type ('l, 'r) either = Inl of 'l | Inr of 'r

let list_of_hash t = Hashtbl.fold (fun k v store -> (k,v) :: store) t []

let list_filter_map f xs =
  List.rev @@ List.fold_left (fun ys x ->
    match f x with
    | Some y -> y::ys
    | None -> ys) [] xs

let maybe f x =
  try Inl (f x) with e -> Inr e
let value = function
    Inl v -> v | Inr e -> raise e
let value_or default = function
  | Inl v -> v | Inr _ -> default

module Option = struct
  type 'a t = 'a option
  let some x = Some x
  let none = None
      
  let of_either = function
    | Inl x -> Some x
    | Inr _ -> None

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let sopt show = function
    | Some x -> show x
    | None -> "None"

  let opt_min x y =
    match x, y with
    | Some x, Some y -> Some (min x y)
    | x, None -> x
    | None, y -> y

  let maybe f x = of_either @@ maybe f x
  let get_or_else default = function
    | Some x -> x
    | None -> default

  let cat_options os =
    let rec aux rev_xs = function
      | [] -> List.rev rev_xs
      | (Some x) :: os -> aux (x :: rev_xs) os
      | None :: os -> aux rev_xs os
    in
    aux [] os
end

let open_with (opn, close) filepath f =
  let ch = opn filepath in
  value @@ tee (fun _ -> close ch) (maybe f ch)
let open_in_with filepath f = open_with (open_in, close_in) filepath f
let open_out_with filepath f = open_with (open_out, close_out) filepath f

let read_all ch =
  let rec iter store =
    try iter @@ input_line ch :: store with
    | End_of_file -> List.rev store
  in
  iter []

let read_file filename = slist "\n" id @@ open_in_with filename read_all

let just default = function
  | Some x -> x
  | None -> default

let random_int =
  Random.self_init ();
  Random.int

let to_hex n =
  let to_char = function
    | x when 0<=x && x<=9 -> (string_of_int x).[0]
    | x when 10<=x && x<=15 -> char_of_int (int_of_char 'A'+(x-10))
    | _ -> failwith"tohex MNH"
  in
  let rec iter store n =
    if n < 16 then
      to_char n :: store
    else
      let r,q = n / 16, n mod 16 in
      iter (to_char q :: store) r
  in
  if n < 0 then raise (Invalid_argument (!%"to_hex: (%d)" n))
  else string_of_chars @@ iter [] n

open Unix
module Date = struct
  type t = float
  let make year mon day h m s =
    fst (mktime { tm_sec=s; tm_min=m; tm_hour=h;
      tm_mday=day; tm_mon=mon-1; tm_year=year-1900;
      tm_wday=0; tm_yday=0; tm_isdst=false
    })
  let make_from_gmt year mon day h m s =
    let diff =  fst (mktime (gmtime 0.)) in
    make year mon day h m s -. diff
  let now : unit -> t = Unix.time
  let year t = (localtime t).tm_year + 1900
  let mon t = (localtime t).tm_mon + 1
  let day t = (localtime t).tm_mday
  let hour t = (localtime t).tm_hour
  let min t = (localtime t).tm_min
  let sec t = (localtime t).tm_sec
  let lt d1 d2 = d1 < d2
  let to_string t = !%"%4d/%02d/%02d %02d:%02d:%02d" (year t) (mon t) (day t)
      (hour t) (min t) (sec t)
  let pmonth = function
    | "Jan" ->  1
    | "Feb" ->  2
    | "Mar" ->  3
    | "Apr" ->  4
    | "May" ->  5
    | "Jun" ->  6
    | "Jul" ->  7
    | "Aug" ->  8
    | "Sep" ->  9
    | "Oct" -> 10
    | "Nov" -> 11
    | "Dec" -> 12
    | unknown ->
	raise (Invalid_argument ("Date.pmonth: unknown month ["^unknown^"]"))
end
