open Spotlib.Spot
open OCamltter_oauth
open Api_intf

(** {6 HTTP parameters} *)

type params = (string * string option) list
(** HTTP parameter. Element with None means "there is no such a field". *)

(** {6 Error and results} *)

module Error : sig

  type http       = Http.error
  type json_conv  = [ `Json of Api_intf.Json.t Meta_conv.Error.t ]
  type json_parse = [ `Json_parse of exn * string ]
  type t = [ http | json_conv | json_parse ]

  val format : Format.t -> t -> unit

end

type 'a result = ('a, Error.t) Result.t
(** The type of results *)

(** {6 Base communication} *)

val twitter :
  Oauth.t
  -> ?host:string           (** Normally ["api.twitter.com"] *)
  -> Http.meth              (** POST/GET *)
  -> string                 (** URL path part *)
  -> (string * string) list (** header *)
  -> (Json.t, [> Error.http | Error.json_parse ]) Result.t

type 'a json_converter = Json.t -> ('a, Json.t Meta_conv.Error.t) Result.t 
(** The type of Json to OCaml converter *)

val api' : 
  'b json_converter
  -> Http.meth 
  -> string
  -> params 
  -> Oauth.t 
  -> 'b result
(** API call with JSON-to-OCaml conversion *)

val api :
  'b json_converter
  -> Http.meth 
  -> ('c, unit, string,  'b result) format4 
  -> params 
  -> Oauth.t 
  -> 'c
(** Same as [api'], but enhanced with OCaml Printf *)

(** {6 Twitter API option/required argument combinator} *)

module Arg : sig

  val of_bool   : bool option   -> string option
  val of_int64  : int64 option  -> string option
  val of_int    : int option    -> string option
  val of_float  : float option  -> string option
  val of_string : string option -> string option
  
  val optional_args : ('a list -> 'b) -> 'a list -> 'a list -> 'b
  
  val run : ('a -> 'b list -> 'c) -> 'a -> 'c
  
  val get :
    'b json_converter
    -> ('c, unit, string, 'b result) format4 
    -> ((params -> Oauth.t -> 'c) -> 'd list -> 'e) 
    -> 'e
  
  val post :
    'b json_converter
    -> ('c, unit, string, 'b result) format4 
    ->  ((params -> Oauth.t -> 'c) -> 'd list -> 'e) 
    -> 'e

  (** { 6 API option combinators } *)

  type ('a, 'b) opt = (params -> 'a) -> params -> 'b
  (** The type of API option combinator. 
      A combinator of type [('a, ?xxx:t -> ... -> ?yyy:t -> 'a) opt] adds 
      [?xxx:t -> ... -> ?yyy:t ->] arguments to APIs functions.
  *)

  val count                 : ('a, ?count:int -> 'a) opt
  val since_max_ids         : ('a, ?since_id:int64 -> ?max_id:int64 -> 'a) opt
  val since_id              : ('a, ?since_id:int64 -> 'a) opt
  val trim_user             : ('a, ?trim_user:bool -> 'a) opt
  val contributor_details   : ('a, ?contributor_details:bool -> 'a) opt
  val include_entities      : ('a, ?include_entities:bool -> 'a) opt
  val include_my_tweet      : ('a, ?include_my_tweet:bool -> 'a) opt
  val include_user_entities : ('a, ?include_user_entities:bool -> 'a) opt
  val user_id_screen_name   : ('a, ?user_id:int64 -> ?screen_name:string -> 'a) opt
  val required_either_user_id_or_screen_name : ('a, ?user_id:int64 -> ?screen_name:string -> 'a) opt
  val include_rts           : ('a, ?include_rts:bool -> 'a) opt
  val exclude_replies       : ('a, ?exclude_replies:bool -> 'a) opt
  val skip_status           : ('a, ?skip_status:bool -> 'a) opt
  val in_reply_to_status_id : ('a, ?in_reply_to_status_id:int64 -> 'a) opt
  val geo                   : ('a, ?lat:float -> ?long:float -> ?place_id:string -> ?display_coordinates:bool -> 'a) opt
  val cursor                : ('a, ?cursor:string -> 'a) opt
  val resources             : ('a, ?resources:string list -> 'a) opt
  val follow                : ('a, ?follow:bool -> 'a) opt

  (** { 6 API required argument combinators } *)

  type ('a, 'b) required_arg = (params -> Oauth.t -> 'a) -> params -> Oauth.t -> 'b -> 'a
  (** The type of required argument combinators.
      A combinator of type [[('a, t) required_arg] is to add a required argument of type [t] to APIs.
      In other words, it enriches a function of type [params -> Oauth.t -> 'a] to
      [params -> Oauth.t -> 'b -> 'a].
  *)

  val required_args   : ('a, params) required_arg
  val required_arg    : ('arg -> params) -> ('b, 'arg) required_arg

  val required_status : ('a, string) required_arg
  val required_q      : ('a, string) required_arg
  val required_id     : ('a, int64)  required_arg
end

(** { 6 Cursor API to OCaml lazy list } *)

module Cursor : sig

  (** A converter from Twitter API's cursor to OCaml's lazy list (Stream.t).

      I know you do not understand this type. 

      Read the use cases first.
  *)    

  val streaming : 
    Http.meth 

    -> ( 'elem result Stream.t -> 'final_result) 
    (** final kontinuation *)

    -> 'sublist_record Json_conv.decoder 
    (** how to decode the raw result of sublist records *)

    -> ('sublist_record -> 'elem list) 
    (** how to get a sublist from a sublist record *)

    -> string  (** url piece. CR jfuruse: no printf interface? *)

    -> ((params -> Oauth.t -> 'final_result) -> params -> 'the_function_type)
    (** argument accumulator *)

    -> 'the_function_type

end

(** { 6 APIs } 


    They are simply Twitter API's translation in OCaml. Read Twitter's API documents for details.
*)

module Timelines : sig

  val mentions_timeline :
    ?count:int ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    ?trim_user:bool ->
    ?contributor_details:bool ->
    ?include_entities:bool ->
    Oauth.t ->
    Tweet.ts result

  val mentions_timeline_stream :
    ?trim_user:bool ->
    ?contributor_details:bool ->
    ?include_entities:bool ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    Oauth.t ->
    Tweet.t result Stream.t

  val user_timeline :
    ?count:int ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    ?user_id:int64 ->
    ?screen_name:string ->
    ?trim_user:bool ->
    ?exclude_replies:bool ->
    ?contributor_details:bool ->
    ?include_rts:bool ->
    Oauth.t ->
    Tweet.ts result

  val user_timeline_stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?trim_user:bool ->
    ?exclude_replies:bool ->
    ?contributor_details:bool ->
    ?include_rts:bool ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    Oauth.t ->
    Tweet.t result Stream.t

  val home_timeline :
    ?count:int ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    ?trim_user:bool ->
    ?exclude_replies:bool ->
    ?contributor_details:bool ->
    ?include_entities:bool ->
    Oauth.t ->
    Tweet.ts result

  val home_timeline_stream :
    ?trim_user:bool ->
    ?exclude_replies:bool ->
    ?contributor_details:bool ->
    ?include_entities:bool ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    Oauth.t ->
    Tweet.t result Stream.t

  val retweets_of_me :
    ?count:int ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    ?trim_user:bool ->
    ?include_entities:bool ->
    ?include_user_entities:bool ->
    Oauth.t ->
    Tweet.ts result

  val retweets_of_me_stream :
    ?trim_user:bool ->
    ?include_entities:bool ->
    ?include_user_entities:bool ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    Oauth.t ->
    Tweet.t result Stream.t
end

module Tweets : sig
  val retweets :
    ?count:int ->
    ?trim_user:bool ->
    Oauth.t ->
    int64 ->
    Tweet.ts result    

  val show :
    ?trim_user:bool ->
    ?include_my_tweet:bool ->
    ?include_entities:bool ->
    Oauth.t ->
    int64 ->
    Tweet.t result

  val destroy :
    ?trim_user:bool ->
    Oauth.t ->
    int64 ->
    Json.t result

  val update :
    ?in_reply_to_status_id:int64 ->
    ?lat:float ->
    ?long:float ->
    ?place_id:string ->
    ?display_coordinates:bool ->
    ?trim_user:bool ->
    Oauth.t ->
    string ->
    Tweet.t result

  val retweet :
    ?trim_user:bool ->
    Oauth.t ->
    int64 ->
    Tweet.t result
end

module Search : sig
  val tweets :
    ?count:int ->
    ?since_id:int64 ->
    Oauth.t ->
    string ->
    Search_tweets.t result
end

module Streaming : sig  
  (* empty... means not yet implemented *)
end

module DirectMessages : sig  
  (* empty... means not yet implemented *)
end

module FriendsAndFollowers(A : sig val dir : string end) : sig

  val ids_stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t ->
    int64 result Stream.t

  val ids :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t -> int64 result list

  val list_stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?skip_status:bool ->
    ?include_user_entities:bool ->
    Oauth.t ->
    User.t result Stream.t

  val list :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?skip_status:bool ->
    ?include_user_entities:bool ->
    Oauth.t -> User.t result list

end

module Friends : sig

  val ids_stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t -> int64 result Stream.t

  val ids :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t -> int64 result list

  val list_stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?skip_status:bool ->
    ?include_user_entities:bool ->
    Oauth.t ->
    User.t result Stream.t

  val list :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?skip_status:bool ->
    ?include_user_entities:bool ->
    Oauth.t -> User.t result list
end

module Followers : sig

  val ids_stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t -> int64 result Stream.t

  val ids :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t -> int64 result list

  val list_stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?skip_status:bool ->
    ?include_user_entities:bool ->
    Oauth.t ->
    User.t result Stream.t

  val list :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?skip_status:bool ->
    ?include_user_entities:bool ->
    Oauth.t -> User.t result list

end

module Friendships : sig

  type connection =
    [ `Followed_by | `Following | `Follwing_requested | `None ]
  with conv(json, ocaml)

  type t =
       < connections : connection list; id : int64; id_str : string;
    name : string; screen_name : string >
  with conv(json, ocaml)

  type ts = t list with conv(json, ocaml)

  val lookup :
    ?screen_name:string list ->
    ?user_id:int64 list ->
    Oauth.t ->
    ts result

  type ids = { ids : int64 list; } with conv(json, ocaml)

  val gen_io :
    string ->
    (int64 result Stream.t -> 'a) ->
    Oauth.t -> 'a

  val incoming_stream :
    Oauth.t -> int64 result Stream.t

  val incoming : Oauth.t -> int64 result list

  val outgoing_stream :
    Oauth.t -> int64 result Stream.t

  val outgoing : Oauth.t -> int64 result list

  val create :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?follow:bool ->
    Oauth.t ->
    User.t result

  val follow :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?follow:bool ->
    Oauth.t ->
    User.t result

  val destroy :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t ->
    User.t result

  val unfollow :
    ?user_id:int64 ->
    ?screen_name:string ->
    Oauth.t ->
    User.t result
end

module Users : sig  
  (* empty... means not yet implemented *)
end

module Blocks : sig

  val list_stream :
    ?include_entities:bool ->
    ?skip_status:bool ->
    Oauth.t ->
    User.t result Stream.t

  val list :
    ?include_entities:bool ->
    ?skip_status:bool ->
    Oauth.t -> User.t result list

  val ids_stream :
    Oauth.t -> int64 result Stream.t

  val ids : Oauth.t -> int64 result list

  val create :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?include_entities:bool ->
    ?skip_status:bool ->
    Oauth.t ->
    Json.t result

  val destroy :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?include_entities:bool ->
    ?skip_status:bool ->
    Oauth.t ->
    Json.t result
end

module SuggestedUsers : sig  
  (* empty... means not yet implemented *)
end

module Favorites : sig

  val list :
    ?user_id:int64 
    -> ?screen_name:string 
    -> ?count:int 
    -> ?since_id:int64 
    -> ?max_id:int64 
    -> ?include_entities:bool 
    -> Oauth.t 
    -> Tweet.ts result

  val stream :
    ?user_id:int64 ->
    ?screen_name:string ->
    ?include_entities:bool ->
    ?since_id:int64 ->
    ?max_id:int64 ->
    Oauth.t ->
    Tweet.t result Stream.t

  val create :
    ?include_entities:bool ->
    Oauth.t ->
    int64 ->
    Tweet.t result

  val destroy :
    ?include_entities:bool ->
    Oauth.t ->
    int64 ->
    Tweet.t result
end

module Lists : sig  
  (* empty... means not yet implemented *)
end

module SavedSearches : sig  
  (* empty... means not yet implemented *)
end

module PlacesAndGeo : sig  
  (* empty... means not yet implemented *)
end

module Trends : sig  
  (* empty... means not yet implemented *)
end

module SpamReporting : sig

  val report_spam :
    ?user_id:int64 
    -> ?screen_name:string 
    -> Oauth.t 
    -> User.t result

end

module OAuth : sig  
  (* empty... means not yet implemented *)
end

module Help : sig

  val rate_limit_status :
    ?resources:string list 
    -> Oauth.t 
    -> Rate_limit_status.t result

end
