(* Created ocamlc -i *)

val ( @@ ) : ('a -> 'b) -> 'a -> 'b
val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val id : 'a -> 'a
val p : ('a, out_channel, unit) format -> 'a
val pr : string -> unit
val tee : ('a -> 'b) -> 'a -> 'a
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
val const : 'a -> 'b -> 'a
val ( !% ) : ('a, unit, string) format -> 'a
val ( !$ ) : 'a Lazy.t -> 'a
val ( -- ) : int -> int -> int list
val repeat : int -> ('a -> 'b) -> 'a -> unit
val list_concatmap : ('a -> 'b list) -> 'a list -> 'b list
val list_head : 'a list -> 'a
val list_last : 'a list -> 'a
val sint : int -> string
val sfloat : float -> string
val sbool : bool -> string
val string_foldr : (char -> 'a -> 'a) -> string -> 'a -> 'a
val slist : string -> ('a -> string) -> 'a list -> string
val chars_of_string : string -> char list
val string_of_chars : char list -> string
val string1 : char -> string
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val iteri : (int -> 'a -> 'b) -> 'a list -> unit
type ('a, 'b) either = Inl of 'a | Inr of 'b
val list_of_hash : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val list_filter_map : ('a -> 'b option) -> 'a list -> 'b list
val maybe : ('a -> 'b) -> 'a -> ('b, exn) either
val value : ('a, exn) either -> 'a
val value_or : 'a -> ('a, exn) either -> 'a

module Option : sig
  type 'a t = 'a option
  val some : 'a -> 'a t
  val none : 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val sopt : ('a -> string) -> 'a t -> string
  val opt_min : 'a t -> 'a t -> 'a t
  val maybe : ('a -> 'b) -> 'a -> 'b t
  val get_or_else : 'a -> 'a t -> 'a
  val cat_options : 'a option list -> 'a list
end

val open_in_with : string -> (in_channel -> 'a) -> 'a
val open_out_with : string -> (out_channel -> 'a) -> 'a
val read_all : in_channel -> string list
val read_file : string -> string
val just : 'a -> 'a option -> 'a
val random_int : int -> int
val to_hex : int -> string

module Date : sig
  type t = float
  val make : int -> int -> int -> int -> int -> int -> float
  val make_from_gmt : int -> int -> int -> int -> int -> int -> float
  val now : unit -> t
  val year : float -> int
  val mon : float -> int
  val day : float -> int
  val hour : float -> int
  val min : float -> int
  val sec : float -> int
  val lt : t -> t -> bool
  val to_string : t -> string
  val pmonth : string -> int
end
