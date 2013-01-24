open Twitter
open Oauth
open Auth
open Tiny_json
open Meta_conv
open Api_intf

type tweet = Api_intf.Tweet.t

module Auth : sig
  type t = { username : string; oauth : Oauth.t; } with conv(ocaml)
  
  val authorize : Consumer.t -> Auth.VerifiedToken.t -> t
  val authorize_interactive : string (** appname *) -> Consumer.t -> t

  module Single : sig
    val save : string -> t -> unit
    val load : string -> Consumer.t -> Oauth.t
    val oauth : path:string -> appname:string -> Consumer.t -> Oauth.t
  end

  val load : string -> t list
  val save : string -> t list -> unit

end

module Cache : sig
  type t
  val init : unit -> t
  val is_new : t -> tweet -> bool
  val add : t -> tweet -> unit
end

val config_file : string ref
val get_oauth : unit -> Oauth.t
val setup : unit -> Oauth.t
val tw_sort : tweet list -> tweet list
val get_timeline : ?c:int -> ?since_id:status_id -> bool ->
  tweet list
val print_timeline : tweet list -> unit
val reload : unit -> tweet list

val format_tweet : Format.formatter -> tweet -> unit

val l : ?c:int -> ?u:string -> (* ?page:int -> *)unit -> tweet list
val lc : (* ?page:int -> *)int -> tweet list
val lu : (* ?page:int -> *)string -> tweet list
val m : ?c:int -> unit -> tweet list
val kwsk : status_id -> tweet list

val u : string -> status_id
val rt : status_id -> status_id
val re : status_id -> string -> status_id
val qt : status_id -> string -> status_id
val link : status_id -> string
val qtlink : status_id -> string -> status_id
val reqt : status_id -> string -> status_id

val del : status_id -> unit

val follow :   string -> User.t
val unfollow : string -> User.t

val favs : string -> tweet list

val fav   : status_id -> status_id
val unfav : status_id -> status_id
val frt   : status_id -> status_id

(*
val report_spam : string -> unit
*)

val s : string -> tweet list

(*
val limit_status : unit -> Json.t
*)
val help : string

val stop_polling : unit -> unit
val start_polling : unit -> Thread.t
