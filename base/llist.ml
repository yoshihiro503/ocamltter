open Lazy
open Util

type 'a llist = Nil | Cons of 'a * 'a llist Lazy.t

let hd = function | Nil -> failwith "hd" | Cons (x, xs) -> x
let tl = function | Nil -> failwith "tl" | Cons (x, xs) -> !$xs

let rec take n l =
  match n, l with
  | 0, _ -> []
  | n, Nil -> []
  | n, Cons (x, xs) -> x :: take (n-1) !$xs

let rec map f = function
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, lazy (map f !$xs))


(* int llist *)
let rec from n = Cons (n, lazy (from (n+1)))


(* llist <--> stream *)
let rec of_stream str =
  try
    Cons (Stream.next str, lazy (of_stream str))
  with
  | Stream.Failure -> Nil

let sllist ?(items:int=20) delim show l =
  let fin = take items l in
  if List.length fin <= items then
    slist delim show fin
  else
    slist delim show fin ^ "..."

(* string -> llist *)
let of_string =
  of_stream $ Stream.of_string
