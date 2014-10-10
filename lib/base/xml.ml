open Util
open ParserMonad

type attr = string * string
type xml =
  | Tag of string * attr list * xml list
  | PCData of string

let show xml =
  let indent d = String.make (2*d) ' ' in
  let rec iter d = function
    | Tag (name,attrs,body) ->
	indent d ^ "<"^name^" "^slist " " (fun(k,v)->k^"="^v) attrs^">\n"
	^slist "\n" (iter (d+1)) body
	^indent d^"</"^name^">"
    | PCData s -> indent d ^ "PCData("^s^")"
  in
  iter 0 xml
	    
let whitespace =
  let whites = [' '; '\t'; '\n'; '\r'] in
  (many @@ char_when (fun c -> List.mem c whites))

let ident =
  let igs = ['<'; '>'; '?'; '/'; ' '; '\t'; '\r'; '\n'] in
  whitespace >>
  (many1 @@ char_when (fun c -> List.mem c igs = false)) >>=
  (return $ string_of_chars)

let pattr : attr t =
  let k_char c =
    List.mem c ['='; '<'; '>'; '?'; '/'; ' '; '\t'; '\r'; '\n'] = false
  in
  let ident0 = many (char_when ((<>) '\"')) >>= (return $ string_of_chars) in
    whitespace >> make_ident k_char >>= fun k -> char '=' >> char '\"' >>
    ident0 <.< char '\"' >>= fun v -> return (k,v)

let pcdata =
  let igs = ['<'; '>'] in
  whitespace >>
  (many1 @@ char_when (fun c -> List.mem c igs = false)) >>= fun cs ->
    return @@ PCData(string_of_chars cs)
    
let parser_ =
    let rec iter () =
      let top = char '<' >> char '?' >> ident >>= fun _xml ->
	many pattr <.< whitespace <.< char '?' <.< char '>' >>= fun _attrs ->
	iter ()
      in
      let tag1 = char '<' >> ident >>= fun name ->
	many pattr <.< char '>' >>= fun attrs ->
	many (iter()) >>= fun children ->
	whitespace >> char '<' >> char '/' >> ident <.< char '>' >>= fun _name' ->
	return (Tag(name, attrs, children))
      in
      let tag2 = char '<' >> ident >>= fun name ->
	many pattr <.< char '/' <.< char '>' >>= fun attrs ->
	return (Tag(name, attrs, []))
      in
      whitespace >> (top <|> tag1^?"tag1" <|> tag2^?"tag2" <|> pcdata)
    in
    iter() ^? "xml"
    
let parse_ch = run_ch parser_
let parse_file = run_file parser_
let parse_string s = try run_string parser_ s with e -> prerr_endline ("[[[\n"^s^"\n]]]\n"); raise e
let getname = function
  | Tag(name, _, _) -> name
  | PCData s -> failwith("getname: PCData("^s^")")

let getchild name : xml -> xml = function
  | Tag(_, _, xmls) ->
      begin try List.find (fun x -> getname x = name) xmls with
      | _e -> failwith ("getchild: notfound:\""^name^"\":\n"^slist"\n"show xmls)
      end
  | PCData s -> failwith("getchild: PCData("^s^")")
let (%%) xml name = getchild name xml
let getchildren : xml -> xml list = function
  | Tag(_, _, xmls) -> xmls
  | PCData s -> failwith("getchildren: PCData("^s^")")

let pcdata = function
  | Tag(_, _, _) -> failwith("pcdata:")
  | PCData s -> s
let childpc = function
  | Tag(_, _, PCData s::_) -> s
  | Tag(_, _, _) -> failwith("childpc: Tag()")
  | PCData s -> failwith("childpc: PCData("^s^")")

let (%%%) xml name = childpc (xml %% name)
let (%%<) xml name = getchildren (xml %% name)

let attr name = function
  | Tag(_, attrs, _) when List.mem_assoc name attrs ->
      List.assoc name attrs
  | Tag(_, _attrs, _) -> failwith("attr: not member("^name^")")
  | PCData s -> failwith("attr: PCData("^s^")")
