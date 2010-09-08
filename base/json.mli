type t =
    String of string
  | Number of float
  | Object of obj
  | Array of t list
  | Bool of bool
  | Null
and obj = (string * t) list

exception JSON_NotObject of t
exception JSON_InvalidField of string
exception JSON_CastErr of string

val show : t -> string
val getf : string -> t -> t
val getf_opt : string -> t -> t option

val as_bool : t -> bool
val as_object : t -> obj
val as_float : t -> float
val as_string : t -> string
val as_list : t -> t list
val as_int : t -> int

val parse_ch : in_channel -> t
val parse : string -> t
