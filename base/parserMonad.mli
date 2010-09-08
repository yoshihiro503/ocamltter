type ts
type state
type error
type 'a parser

val error : string -> 'a parser
val showerr : error -> string

val return : 'a -> 'a parser
val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser

val ( >> ) : 'a parser -> 'b parser -> 'b parser
val ( << ) : 'a parser -> 'b parser -> 'a parser
val ( ^? ) : 'a parser -> string -> 'a parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser

val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser
val sep : 'a parser -> 'b parser -> 'b list parser
val opt : 'a parser -> 'a option parser

val char1 : char parser
val char_when : (char -> bool) -> char parser
val char : char -> char parser
val keyword : string -> string parser
val make_ident : (char -> bool) -> string parser
val int : int parser

val init_state : state
val run_ch : 'a parser -> in_channel -> 'a
val run_stdin : 'a parser -> 'a
val run_file : 'a parser -> string -> 'a
val run_string : 'a parser -> string -> 'a
