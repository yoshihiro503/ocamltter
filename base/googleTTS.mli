type lang = En | Ja
type table = (string * string) list

val say : lang -> string -> unit
val say_ja : table -> string -> unit
