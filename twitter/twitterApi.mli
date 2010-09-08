(**
   Twitter APIs.
*)
exception TwErr of string

type status_id = int64
type tweet = U of (Util.Date.t * string * status_id * string * string)

val date : tweet -> Util.Date.t
val sname : tweet -> string
val text : tweet -> string
val status_id : tweet -> status_id

val show_tweet : tweet -> string
val tw_compare : tweet -> tweet -> int

(** {6 basic operations for twitter} *)

val twitter :
  string * string * string ->
  ?host:string -> Http.meth -> string -> (string * string) list -> Json.t
(** [twitter (tok,secret,verif) method path parameters] *)

val twitter_without_auth :
  ?host:string -> Http.meth -> string -> (string * string) list -> Json.t
(** [twitter_without_auth] *)

(** {6 APIs} *)

val home_timeline :
  ?since_id:string -> ?count:int -> string * string * string -> tweet list

val user_timeline :
  ?since_id:string ->
  ?count:int -> string * string * string -> string -> tweet list

val show : status_id -> tweet

val get_tweet : status_id -> tweet
(** same as [show] *)

val mentions : string * string * string -> int -> tweet list

val update :
  ?in_reply_to_status_id:string ->
  string * string * string -> string -> Json.t
val retweet : string * string * string -> string -> Json.t
val search : string -> tweet list
val rate_limit_status : unit -> Json.t

(** {6 Initial Authentications} *)

val fetch_request_token : unit -> string * string * string
val fetch_access_token :
  string -> string -> string -> string * string * string
