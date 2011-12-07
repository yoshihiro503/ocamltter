
val oauth_acc : (string * string * string) option ref
val config_file : string ref
val authorize : unit -> string * string * string
module Cache :
  sig
    type t
    val init : unit -> t
    val is_new : t -> TwitterApi.tweet -> bool
    val add : t -> TwitterApi.tweet -> unit
  end
val load : unit -> string * string * string
val oauth : unit -> string * string * string
val setup : unit -> unit
val tw_sort : TwitterApi.tweet list -> TwitterApi.tweet list
val get_timeline : ?c:int -> bool -> TwitterApi.tweet list
val print_timeline : TwitterApi.tweet list -> unit
val reload : unit -> TwitterApi.tweet list

val l : ?c:int -> ?u:string -> unit -> TwitterApi.tweet list
val lc : int -> TwitterApi.tweet list
val lu : string -> TwitterApi.tweet list
val m : ?c:int -> unit -> TwitterApi.tweet list
val kwsk : TwitterApi.status_id -> TwitterApi.tweet list

val u : string -> TwitterApi.status_id
val rt : TwitterApi.status_id -> unit
val re : TwitterApi.status_id -> string -> TwitterApi.status_id
val qt : TwitterApi.status_id -> string -> TwitterApi.status_id
val qtlink : TwitterApi.status_id -> string -> TwitterApi.status_id
val reqt : TwitterApi.status_id -> string -> TwitterApi.status_id
val del : TwitterApi.status_id -> unit

val follow : string -> unit
val unfollow : string -> unit

val fav : TwitterApi.status_id -> unit
val frt : TwitterApi.status_id -> unit

val report_spam : string -> unit

val s : string -> TwitterApi.tweet list
val limit : unit -> Json.t
val help : string

val stop_polling : unit -> unit
val start_polling : unit -> Thread.t
