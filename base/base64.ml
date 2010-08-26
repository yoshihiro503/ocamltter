open Util
open List

let grps make xs =
  let rec iter store = function
    | [] -> store
    | xs -> let g, xs = make xs in
      iter (g :: store) xs
  in
  List.rev @@ iter [] xs

let f : bool list -> (bool*bool*bool*bool*bool*bool) list=
  let z = false in
  grps (function
    | [] -> failwith "must not happen"
    | a::[]             -> (a,z,z,z,z,z), []
    | a::b::[]          -> (a,b,z,z,z,z), []
    | a::b::c::[]       -> (a,b,c,z,z,z), []
    | a::b::c::d::[]    -> (a,b,c,d,z,z), []
    | a::b::c::d::e::[] -> (a,b,c,d,e,z), []
    | a::b::c::d::e::f::xs ->
	((a,b,c,d,e,f), xs))

let g : char list -> (char*char*char*char) list =
  let x = '=' in
  grps (function
    | [] -> failwith "must not happen"
    | a::[] -> (a,x,x,x),[]
    | a::b::[] -> (a,b,x,x),[]
    | a::b::c::[] -> (a,b,c,x),[]
    | a::b::c::d::xs -> (a,b,c,d),xs)

let pow2 n =
  int_of_float (2. ** float n)

let digits (x:int) : bool list =
  let digit n x =
    let p = int_of_float (2. ** float n) in
    (p =  x land p)
  in
  map (fun i -> digit i x) (7--0)

let table = function
  | b5,b4,b3,b2,b1,b0 ->
      let p n = function
	| true  -> pow2 n
	| false -> 0
      in
      let x = 
	p 0 b0 + p 1 b1 + p 2 b2 +
	  p 3 b3 + p 4 b4 + p 5 b5
      in
      let c =
	match x with
	| x when 0 <= x && x <= 25 -> int_of_char 'A' + x
	| x when 26 <= x && x <= 51 -> int_of_char 'a' + (x - 26)
	| x when 52 <= x && x <= 61 -> int_of_char '0' + (x - 52)
	| 62 -> int_of_char '+'
	| 63 -> int_of_char '/'
	| _ -> failwith "MNH"
      in
      char_of_int c

let encode s =
  chars_of_string s
    |> map int_of_char
    |> map digits
    |> concat
    |> f
    |> map table
    |> g
    |> map (fun (a,b,c,d) -> [a;b;c;d])
    |> concat
    |> string_of_chars


