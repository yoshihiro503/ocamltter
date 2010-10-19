(**
   Twitter APIs.
*)
open Util

exception TwErr of string

type status_id = int64
type tweet =
  | U of tweet_base
  | RT of tweet_base * tweet_base
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

(** {6 basic operations for twitter} *)

val twitter :
    OauthForTwitter.token ->
  ?host:string -> Http.meth -> string -> (string * string) list -> Json.t
(** [twitter (tok,secret,verif) method path parameters] *)

val twitter_without_auth :
  ?host:string -> Http.meth -> string -> (string * string) list -> Json.t
(** [twitter_without_auth] *)

(** {6 APIs} *)

(** {7 Timeline Methods} *)

val home_timeline :
  ?since_id:string -> ?count:int -> OauthForTwitter.token -> tweet list

val user_timeline :
  ?since_id:string ->
  ?count:int -> OauthForTwitter.token -> string -> tweet list

val show : status_id -> tweet

val get_tweet : status_id -> tweet
(** same as [show] *)

val mentions : OauthForTwitter.token -> int -> tweet list

(** {7 Status Methods} *)

val update :
  ?in_reply_to_status_id:string ->
  OauthForTwitter.token -> string -> Json.t

val destroy : OauthForTwitter.token -> status_id -> Json.t
val retweet : OauthForTwitter.token -> string -> Json.t

(** {7 User Methods} *)

val users_lookup : OauthForTwitter.token -> string -> Json.t
val users_lookup_uid : OauthForTwitter.token -> string -> Json.t

(** {7 Friendship Methods} *)

val friendship_create : OauthForTwitter.token -> string -> Json.t
val friendship_destroy : OauthForTwitter.token -> string -> Json.t

(** {7 Social Graph Methods} *)

val friends : ?sname:string -> OauthForTwitter.token -> Json.t
val followers : ?sname:string -> OauthForTwitter.token -> Json.t

(** {7 Account Methods} *)

val rate_limit_status : unit -> Json.t

(** {7 Favorite Methods} *)

val favorites : ?sname:string -> OauthForTwitter.token -> Json.t
val favorites_create : OauthForTwitter.token -> status_id -> Json.t
val favorites_destroy : OauthForTwitter.token -> status_id -> Json.t

(** {7 Spam Reporting Methods} *)

val report_spam : OauthForTwitter.token -> string -> Json.t

(** {7 OAuth Methods} *)

val fetch_request_token : unit -> OauthForTwitter.token
val fetch_access_token :
  string -> string -> string -> OauthForTwitter.token

(** {7 Help Methods} *)

val help_test : unit -> Json.t

(** {7 Search API Methods} *)

val search : ?rpp:int -> string -> tweet list
