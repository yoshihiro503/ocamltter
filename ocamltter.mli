open Twitter
open Oauth
open Auth
open Tiny_json
open Meta_conv
open Api_intf

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
  val is_new : t -> Api.tweet -> bool
  val add : t -> Api.tweet -> unit
end

val config_file : string ref
val get_oauth : unit -> Oauth.t
val setup : unit -> Oauth.t
val tw_sort : Api.tweet list -> Api.tweet list
val get_timeline : ?c:int -> ?since_id:status_id -> bool ->
  Api.tweet list
val print_timeline : Api.tweet list -> unit
val reload : unit -> Api.tweet list

val l : ?c:int -> ?u:string -> ?page:int -> unit -> Api.tweet list
val lc : ?page:int -> int -> Api.tweet list
val lu : ?page:int -> string -> Api.tweet list
val m : ?c:int -> unit -> Api.tweet list
val kwsk : status_id -> Api.tweet list

val u : string -> status_id
val rt : status_id -> unit
val re : status_id -> string -> status_id
val qt : status_id -> string -> status_id
val link : status_id -> string
val qtlink : status_id -> string -> status_id
val reqt : status_id -> string -> status_id
val del : status_id -> unit

val follow : string -> unit
val unfollow : string -> unit

val fav : status_id -> unit
val frt : status_id -> unit

val report_spam : string -> unit

val s : string -> Api.tweet list
val limit_status : unit -> Json.t
val help : string

val stop_polling : unit -> unit
val start_polling : unit -> Thread.t
