open Util
open Llist

type ts = char llist
type state = int * int * (char list * char * char list)
type error = state * string
type 'a parser = state -> ts -> ('a * state * ts, error) either

exception ParseError of string

let lt_pos (l1,p1,_) (l2,p2,_) =
  if l1 < l2 then true
  else if l1 = l2 then p1 < p2
  else false

let eplus (st1,msg1) (st2,msg2) =
  if lt_pos st1 st2 then (st2,msg2) else (st1,msg1)

let showerr ((line,pos,(pre,c,post)),msg) =
  !%"{\n  line %d, %d: %s\n%s[%c]%s\n}"line pos msg (string_of_chars pre)
  c (string_of_chars post)
    
let return : 'a -> 'a parser =
    fun x ->
      fun state code -> Inl (x, state, code)


let error msg = fun state _code -> Inr (state, msg)

let (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser =
    fun p f ->
      fun state code ->
	match p state code with
	| Inl (x, state', ts) -> f x state' ts
	| Inr err -> Inr err
	      
let (>>) : 'a parser -> 'b parser -> 'b parser =
    fun p1 p2 ->
      p1 >>= fun _ -> p2

let (<<) : 'a parser -> 'b parser -> 'a parser =
    fun p1 p2 ->
      p1 >>= fun x -> p2 >> return x

let ( ^? ) : 'a parser -> string -> 'a parser =
    fun p msg ->
      fun state code ->
	match p state code with
	| Inl l -> Inl l
	| Inr (st,msg0) -> Inr (st,msg ^": "^msg0)
    
    (* (<|>) : 'a m -> 'a m -> 'a m *)
let (<|>) : 'a parser -> 'a parser -> 'a parser =
    fun p1 p2 ->
      fun state code ->
	match p1 state code with
	| Inl (x1, state', ts) -> Inl (x1, state', ts)
	| Inr err1 ->
	    begin match p2 state code with
	    | Inl (x2, state', ts) -> Inl (x2,state',ts)
	    | Inr err2 -> Inr (eplus err1 err2)
	    end

(*
let (<|?>) p1 p2 = fun state code ->
  match p1 state code with
  | Inl (x1, state', ts) -> Inl (x1, state', ts)
  | Inr err1 ->
      print_endline err1;
      begin match p2 state code with
      | Inl (x2, state', ts) -> Inl (x2,state',ts)
      | Inr err2 -> Inr (eplus err1 err2)
      end
*)	

let rec many : 'a parser -> ('a list) parser =
    fun p ->
      (p >>= fun x -> many p >>= fun xs -> return (x::xs))
	<|> (return [])

let many1 p =
  p >>= fun x -> many p >>= fun xs -> return (x::xs)

let sep separator p =
  (p >>= fun x -> many (separator >> p) >>= fun xs -> return (x::xs))
    <|> (return [])


let opt : 'a parser -> ('a option) parser =
    fun p ->
      (p >>= fun x -> return (Some x)) <|> (return None)


let char1_with_debug state = function
  | Nil -> Inr (state,"(Nil)")
  | Cons (x,xs) ->
      let next (pre,x0, _) =
	let pre' = if List.length pre < 100 then pre @ [x0]
	  else List.tl pre @ [x0]
	in
	(pre' , x, Llist.take 100 !$xs)
      in
      match x, state with
      | '\n', (line,pos,cs) ->
	  Inl (x,(line+1,-1, next cs), !$xs)
      | _, (line,pos,cs) ->
	  Inl (x,(line, pos+1, next cs),!$xs)

let char1_without_debug state = function
  | Nil -> Inr (state,"(Nil)")
  | Cons (x,xs) -> Inl (x, state, !$xs)

let char1 = char1_without_debug

let char_when f = char1 >>= fun c ->
  if f c then return c
  else error (!%"(char:'%c')" c)

let char c = char_when ((=) c)

let keyword w =
  let rec iter i =
    if i < String.length w then
      char w.[i] >> iter (i+1)
    else return w
  in
  iter 0

let make_ident f =
  many1 (char_when f) >>= fun cs ->
    return (string_of_chars cs)

let int =
  opt (char '-') >>= fun minus ->
  make_ident (function '0'..'9' -> true | _ -> false) >>= fun s ->
  return
    begin match minus with
    | None -> int_of_string s
    | Some _ -> - int_of_string s
    end

let run p state ts =
  match p state ts with
  | Inl (x,state',xs) -> x
  | Inr err -> 
      raise (ParseError (showerr err))

let init_state = (1, 0, ([],'_',[]))

let run_ch p ch =
  run p init_state (Llist.of_stream (Stream.of_channel ch))

let run_stdin p = run_ch p stdin

let run_file p filename =
  open_in_with filename (fun ch -> run_ch p ch)

let run_string p s =
  run p init_state (Llist.of_string s)
