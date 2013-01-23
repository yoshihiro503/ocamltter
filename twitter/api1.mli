(**
   Twitter APIs, version 1.0
*)
open Util
open Api_intf
open Meta_conv

val (~?) : ('a * 'b option) list -> ('a * 'b) list

exception TwErr of string

type tweet =
  | U of tweet_base
  | RT of tweet_base (** itself *) * tweet_base (** referred *)
  | RE of tweet_base * status_id
and tweet_base =
  {date:Date.t; sname:string; id:status_id; client:Xml.xml; text:string;
   json:Json.t}

val date : tweet -> Util.Date.t
val sname : tweet -> string
val status_id : tweet -> status_id
val client : tweet -> Xml.xml
val text : tweet -> string
val json : tweet -> Json.t

val show_tweet : tweet -> string
val tw_compare : tweet -> tweet -> int
(** Date comparision of tweets *)

(** {6 basic operations for twitter} *)

val twitter :
  Oauth.t ->
  ?host:string -> Http.meth -> string -> (string * string) list -> Json.t
(** [twitter (tok,secret,verif) method path parameters] *)

val twitter_without_auth :
  ?host:string -> Http.meth -> string -> (string * string) list -> Json.t
(** [twitter_without_auth] *)

val twitter_low :
  ?host:string -> Http.meth -> string -> (string * string) list -> string
(** [twitter_without_auth for debug] *)

(** {6 APIs} *)

(** {7 Access with Cursor} *)

module Cursor : sig

  val gets 
    : ((string * string) list -> Json.t)  (** basic api func with params *)
    -> (Json.t -> 'a)                     (** the decoder of the content *)
    -> (string * string) list             (** params without cursor *)
    -> 'a Spotlib.Stream.t                (** result as a lazy list *)

end 

(** {7 Timeline Methods} *)

val home_timeline :
  ?since_id:string -> ?count:int -> ?page:int ->
  Oauth.t -> tweet list

val home_timeline' :
  ?since_id:string -> ?count:int -> ?page:int ->
  Oauth.t -> (Tweet.t list, Json.t Error.t) Result.t

val print_home_timeline : Oauth.t -> unit

val user_timeline :
  ?since_id:string ->
  ?count:int ->
  ?page:int ->
  ?max_id:string ->
  Oauth.t -> string -> tweet list

val user_timeline' :
  ?since_id:string ->
  ?count:int ->
  ?page:int ->
  ?max_id:string ->
  Oauth.t -> string -> (Tweet.t list, Json.t Error.t) Result.t

val show : status_id -> tweet
val show' : status_id -> (Tweet.t list, Json.t Error.t) Result.t

val show_low : status_id -> string

val get_tweet : status_id -> tweet
val get_tweet' : status_id -> (Tweet.t list, Json.t Error.t) Result.t
(** same as [show] *)

val mentions : Oauth.t -> int -> tweet list
val mentions' : Oauth.t -> int -> (Tweet.t list, Json.t Error.t) Result.t

(** {7 Status Methods} *)

val update :
  ?in_reply_to_status_id:string ->
  Oauth.t -> string -> tweet

val update' :
  ?in_reply_to_status_id:string ->
  Oauth.t -> string -> (Tweet.t list, Json.t Error.t) Result.t

val destroy : Oauth.t -> status_id -> Json.t
val retweet : Oauth.t -> string -> Json.t

(** {7 User Methods} *)

(* CR jfuruse: input must be at most 100, but no check is performed. *)
val users_lookup_uids : Oauth.t -> int64 list -> User.t list
val user_lookup_uid : Oauth.t -> int64 -> User.t
val users_lookup_uids_raw : Oauth.t -> int64 list -> Json.t
val users_search: Oauth.t -> string -> int -> Json.t

val users_lookup_names : Oauth.t -> string list -> User.ts
val user_lookup_name : Oauth.t -> string -> User.t

(** {7 Friendship Methods} *)

val friendship_create    : Oauth.t -> string -> Json.t
val friendship_destroy   : Oauth.t -> string -> Json.t
val friendship_create_id : Oauth.t -> int64 -> (User.t, Json.t Error.t) Result.t

(** {7 Social Graph Methods} *)

type ids = int64 list
val friends   : ?sname:string -> Oauth.t -> ids
val followers : ?sname:string -> Oauth.t -> ids

(** {7 Account Methods} *)

val friends   : ?sname:string -> Oauth.t -> int64 list
val followers : ?sname:string -> Oauth.t -> int64 list

val rate_limit_status : unit -> Json.t

(** {7 Favorite Methods} *)

val favorites : ?sname:string -> Oauth.t -> Json.t
val favorites_create : Oauth.t -> status_id -> Json.t
val favorites_destroy : Oauth.t -> status_id -> Json.t

(** {7 Spam Reporting Methods} *)

val report_spam : Oauth.t -> string -> Json.t

(** {7 Oauth.T Methods} *)

val fetch_request_token : Auth.Consumer.t -> string (* URL *) * Auth.Token.t
val fetch_access_token : Auth.Consumer.t -> Auth.VerifiedToken.t -> string (* username *) * Auth.Token.t

(** {7 Help Methods} *)

val help_test : unit -> Json.t

(** {7 Search API Methods} *)

val search : ?rpp:int -> ?since_id:status_id -> string -> tweet list
(** 1.0 *)
