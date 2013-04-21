open Twitter
open Oauth
open Auth
open Tiny_json
open Meta_conv
open Api_intf

exception Error of [`Http of int * string
                   | `Json of Json.t Meta_conv.Error.t 
                   | `Json_parse of exn * string
                   ]

type tweet = Api_intf.Tweet.t

module Auth : sig

  module App : sig
    type t = { name : string; consumer : Consumer.t; } with conv(ocaml)
    val dummy : t
    val ocamltter : t
  end
  
  module User : sig
    type t = { token : string; token_secret : string; verif : string; } 
    with conv(ocaml)
    val dummy : t
  end

  type t = { app : App.t; users : (string, User.t) Hashtbl.t; } with conv(ocaml)
  type tbl = (App.t, (string, User.t) Hashtbl.t) Hashtbl.t
  val dummy : t
  val oauth : App.t -> User.t -> Twitter.Oauth.t
  val load : string -> tbl
  val save : string -> tbl -> unit
  val save_dummy : string -> unit
  val find : tbl -> App.t -> string -> User.t list
  val authorize : App.t -> Twitter.Auth.VerifiedToken.t -> string * User.t
  val authorize_interactive : App.t -> string * User.t
  
  module Single : sig
    val save : string -> User.t -> unit
    val load : string -> User.t
    val oauth : string -> App.t -> Twitter.Oauth.t
  end
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

val report_spam : string -> User.t

val s : string -> tweet list

val limit_status : unit -> Rate_limit_status.t

val help : string

val stop_polling : unit -> unit
val start_polling : unit -> Thread.t
