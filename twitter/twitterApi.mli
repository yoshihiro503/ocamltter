(**
   Twitter APIs (version 1.0).
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
val set_text : string -> tweet -> tweet
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

val twitter_low :
  ?host:string -> Http.meth -> string -> (string * string) list -> string
(** [twitter_without_auth for debug] *)

(** {6 APIs} *)

(** {7 Timeline Methods} *)

val home_timeline :
  ?since_id:string -> ?count:int -> ?page:int ->
    OauthForTwitter.token -> tweet list

val user_timeline :
    ?id:string -> ?user_id:string -> ?count:int -> ?since:string ->
      ?since_id:string -> ?max_id:string -> ?page:int -> ?trim_user:bool ->
        ?include_rts:bool -> ?include_entities:bool ->
          OauthForTwitter.token -> string -> tweet list

val show : status_id -> tweet

val show_low : status_id -> string

val get_tweet : status_id -> tweet
(** same as [show] *)

val mentions : OauthForTwitter.token -> int -> tweet list

(** {7 Status Methods} *)

val update :
  ?in_reply_to_status_id:string ->
  OauthForTwitter.token -> string -> tweet

val destroy : OauthForTwitter.token -> status_id -> Json.t
val retweet : OauthForTwitter.token -> string -> Json.t

(** {7 User Methods} *)

val users_lookup : OauthForTwitter.token -> string -> Json.t
val users_lookup_uid : OauthForTwitter.token -> string -> Json.t
val users_search: OauthForTwitter.token -> string -> int -> Json.t

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

val search : ?rpp:int -> ?since_id:status_id -> string -> tweet list
