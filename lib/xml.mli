type attr = string * string
type xml = Tag of string * attr list * xml list | PCData of string
val show : xml -> string

val parse_ch : in_channel -> xml
val parse_file : string -> xml
val parse_string : string -> xml

val getname : xml -> string
val getchild : string -> xml -> xml
val ( %% ) : xml -> string -> xml
val getchildren : xml -> xml list
val pcdata : xml -> string
val childpc : xml -> string
val ( %%% ) : xml -> string -> string
val ( %%< ) : xml -> string -> xml list
val attr : string -> xml -> string
