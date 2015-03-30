open Spotlib.Spot
open Meta_conv.Open
open Ocaml_conv
open OCamltter_oauth

module Json = struct
  include Tiny_json.Json
  let json_of_t x = x
  let t_of_json ?trace:_ x = `Ok x

  let ocaml_of_t t = Ocaml.String (show t)
  let t_of_ocaml = Ocaml_conv.Helper.of_deconstr (function
    | Ocaml.String s -> parse s
    | _ -> failwith "Ocaml.String expected")
end

type 'json mc_leftovers = (string * 'json) list [@@deriving conv{ocaml}]

type status_id = int64 [@@deriving conv{ocaml; json}]

module Time : sig
  type t [@@deriving conv{ocaml; json}]
  include Mtypes.Comparable with type t := t  

  val from_unix   : float -> t
  val to_unix     : t -> float
  val from_string : string -> t
  val to_string   : t -> string
end = struct

  open Util.Date
      
  (* CR jfuruse: it is named `date`, but for time. *)    
  let parse_date st =
    let parse_date01 st = (* Wed Apr 21 02:29:17 +0000 2010 *)
      let mon = pmonth & String.sub st 4 3 in
      let day = int_of_string & String.sub st 8 2 in
      let h = int_of_string & String.sub st 11 2 in
      let m = int_of_string & String.sub st 14 2 in
      let s = int_of_string & String.sub st 17 2 in
      let year = int_of_string & String.sub st 26 4 in
      make_from_gmt year mon day h m s
    in
    let parse_date02 st = (* Sat, 17 Apr 2010 08:23:55 +0000 *)
      let mon = pmonth & String.sub st 8 3 in
      let day = int_of_string & String.sub st 5 2 in
      let h = int_of_string & String.sub st 17 2 in
      let m = int_of_string & String.sub st 20 2 in
      let s = int_of_string & String.sub st 23 2 in
      let year = int_of_string & String.sub st 12 4 in
      make_from_gmt year mon day h m s
    in
    try parse_date01 st with
    | _ -> parse_date02 st
  	  
  type t = { 
    printable : string lazy_t;
    tick      : float lazy_t 
  } [@@deriving conv{ocaml}]
    
  type _t = t

  include Mtypes.Make_comparable(struct
    type t = _t
    let compare t1 t2 = compare (Lazy.force t1.tick) (Lazy.force t2.tick)
  end)
    
  let from_unix f = 
    { printable = Lazy.from_val (Printf.sprintf "@%.0f" f);
      tick      = Lazy.from_val f 
    }
      
  let to_unix t = Lazy.force t.tick

  let from_string s =
    { printable = Lazy.from_val s;
      tick      = lazy (parse_date s)
    }

  let to_string t = Lazy.force t.printable

  open Json

  let json_of_t t = String (to_string t)
  let t_of_json = Json_conv.Helper.of_deconstr (function
    | String s -> from_string s
    | _ -> failwith "Time.t_of_json: String expected")
  let t_of_json_exn = Json_conv.exn t_of_json
end

module Text : sig 
  (* HTML encoded text *)
  type t = string [@@deriving conv{ocaml; json}]
end = struct
  type t = string [@@deriving conv{ocaml}]

  open Json

  let t_of_json = Json_conv.Helper.of_deconstr (function
    | String s -> Http.html_decode s
    | _ -> failwith "Text.t_of_json: String expected")

  let t_of_json_exn = Json_conv.exn t_of_json

  let json_of_t _t = Json.String(Http.html_encode _t)
end

module Client : sig
  type t = string * [`Ok of Xml.xml | `Error of exn] [@@deriving conv{ocaml; json}]
  val name : t -> string
end = struct
  type t = string * [`Ok of Xml.xml | `Error of exn]

  open Meta_conv
  open Json

  let t_of_json = Json_conv.Helper.of_deconstr & function
    | String s -> s, Result.catch_exn & fun () -> Xml.parse_string s
    | _ -> failwith "Client.t_of_json: String expected"

  let t_of_json_exn = Json_conv.exn t_of_json

  let json_of_t (s, _) = String s

  let t_of_ocaml ?trace:_ _ = assert false
  let t_of_ocaml_exn ?trace:_ _ = assert false
  let ocaml_of_t (s, _) = Ocaml.String s   

  let name = function
    | (_, `Ok (Xml.PCData client_name))
    | (_, `Ok (Xml.Tag ("a", _, [Xml.PCData client_name]))) ->
        client_name
    | (s, `Ok _) -> s
    | (s, `Error _) -> s

end

module User = struct
  (* Lots of optional fields! *)

  type details = <
      is_translator                         : bool;
      (* profile_background_color           : string; *)
      (* notifications                      : bool option; *)
      profile_image_url_https               : string;
      url                                   : string option;
      (* profile_background_image_url_https : string *)
      created_at                            : Time.t;
      (* profile_background_image_url       : string; *)
      (* utc_offset                         : float *)
      (* profile_link_color                 : string *)
      name                                  : string;
      default_profile                       : bool;
      screen_name                           : string;
      lang                                  : string;
      protected                             : bool;
      statuses_count                        : int64;
      location                              : string option;
      (* profile_use_background_image       : bool; *)
      (* profile_text_color                 : string; *)
      (* contributors_enabled               : bool; *) 
      listed_count                          : int64;
      time_zone                             : string option;
      description                           : string;
      profile_image_url                     : string;
      (* profile_sidebar_border_color       : string *)
      following                             : bool option;
      geo_enabled                           : bool;
      (* profile_background_tile            : bool *)
      followers_count                       : int64;
      (* profile_sidebar_fill_color         : string; *)
      verified                              : bool;
      (* status                             : Twitter.Json.Object ... *)
      (* default_profile_image              : bool *)
      follow_request_sent                   : bool option;
      friends_count                         : int64;
      favourites_count                      : int64 
  > [@@deriving conv{ocaml; json}]

  type t = <
    unknowns : Json.t mc_leftovers;
    id       : int64;
    details  : details mc_option_embeded;
  > [@@deriving conv{ocaml; json}]

  type ts = t list [@@deriving conv{ocaml; json}]

  let format x =  Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t x
end

module Hashtag = struct
  type t = <
    text    : string;
    indices : (int * int) (** location in the tweet string *)
  > [@@deriving conv{ocaml; json}]
end

module URL = struct
  type t = <
    unknown      : Json.t mc_leftovers;

    url          : string;
    expanded_url : string;
    display_url  : string;
  > [@@deriving conv{ocaml; json}]
end
 
module UserMention = struct
  type t = <
      unknown     : Json.t mc_leftovers;

      screen_name : string;
      name        : string;
      id          : int64;
      indices     : int * int;
  > [@@deriving conv{ocaml; json}]
      
end

module Entities = struct
  type t = <
      unknown       : Json.t mc_leftovers;

      hashtags      : Hashtag.t list; 
      urls          : URL.t list;
      user_mentions : UserMention.t list;
  > [@@deriving conv{ocaml; json}]
end

module Tweet = struct
  (* CR jfuruse: exclude_replies ... unknown 
     include_user_entities ... unknown
  *)

  type t = <
    unknowns : Json.t mc_leftovers;

    id        : int64;
    user      : User.t;
    text      : Text.t;
    truncated : bool;

    in_reply_to_status_id   : int64 option; (* RE or RT *)
    in_reply_to_user_id     : int64 option;
    in_reply_to_screen_name : string option;
    retweeted_status        : t mc_option; (* RT *)

    created_at : Time.t;

    source : Client.t; (* html piece *)

    geo         : Json.t option;
    coordinates : Json.t option;
    place       : Json.t option;

    contributors : Json.t option; (* something can be trimed... *)

    retweet_count : int;
    favorited     : bool;
    retweeted     : bool;

    possibly_sensitive : bool mc_option;

    entities : Entities.t mc_option;

  > [@@deriving conv{ocaml; json}]

  type ts = t list [@@deriving conv{ocaml; json}]

  let format    x = Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t x
  let format_ts x = Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_ts x

end

module Search_tweets = struct

  module Search_metadata = struct
    type t = <
      unknowns     : Json.t mc_leftovers;

      query        : string;
      next_results : string mc_option; (* url GET piece for next search *)
      refresh_url  : string; (* url GET piece for refresh *)
      count        : int;
      max_id       : int64;
      since_id     : int64;
      completed_in : float;
    > [@@deriving conv{ocaml; json}]
  end

  type t = <
    unknowns : Json.t mc_leftovers;

    statuses : Tweet.t list;
    search_metadata : Search_metadata.t;
  > [@@deriving conv{ocaml; json}]

  let format x = Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t x

end

module Rate_limit_status = struct

  type limit = <
      limit     : float;
      remaining : float;
      reset     : float (** unix epoch *)
    > [@@deriving conv{ocaml; json}]

  type t = <
    rate_limit_context : < access_token : string >;
    resources : < 
      lists : <
          subscribers         [@conv.as {json="/lists/subscribers"}]                 : limit;
          list                [@conv.as {json="/lists/list"}]                        : limit;
          memberships         [@conv.as {json="/lists/memberships"}]                 : limit;
          ownerships          [@conv.as {json="/lists/ownerships"}]                  : limit;
          subscriptions       [@conv.as {json="/lists/subscriptions"}]               : limit;
          members             [@conv.as {json="/lists/members"}]                     : limit;
          subscribers_show    [@conv.as {json="/lists/subscribers/show"}]            : limit;
          statuses            [@conv.as {json="/lists/statuses"}]                    : limit;
          members_show        [@conv.as {json="/lists/members/show"}]                : limit;
          show                [@conv.as {json="/lists/show"}]                        : limit;
        >;
      application : <
          rate_limit_status   [@conv.as {json="/application/rate_limit_status"}]     : limit;
        >;
      friendships : <
	  incoming            [@conv.as {json="/friendships/incoming"}]              : limit;
	  lookup              [@conv.as {json="/friendships/lookup"}]                : limit;
	  outgoing            [@conv.as {json="/friendships/outgoing"}]              : limit;
          no_retweets_ids     [@conv.as {json="/friendships/no_retweets/ids"}]       : limit;
	  show                [@conv.as {json="/friendships/show"}]                  : limit;
        >;

      blocks : <
	  ids                 [@conv.as {json="/blocks/ids"}]                        : limit;
          list                [@conv.as {json="/blocks/list"}]                       : limit;
        >;
      geo : <
	  similar_places      [@conv.as {json="/geo/similar_places"}]                : limit;
          search              [@conv.as {json="/geo/search"}]                        : limit;
	  reverse_geocode     [@conv.as {json="/geo/reverse_geocode"}]               : limit;
	  place_id            [@conv.as {json="/geo/id/:place_id"}]                  : limit;
        >;
      users : <
	  profile_banner      [@conv.as {json="/users/profile_banner"}]              : limit;
          suggestions_members [@conv.as {json="/users/suggestions/:slug/members"}]   : limit;
	  show                [@conv.as {json="/users/show/:id"}]                    : limit;
	  suggestions         [@conv.as {json="/users/suggestions"}]                 : limit;
	  lookup              [@conv.as {json="/users/lookup"}]                      : limit;
	  search              [@conv.as {json="/users/search"}]                      : limit;
	  suggestions_slug    [@conv.as {json="/users/suggestions/:slug"}]           : limit;
          report_spam         [@conv.as {json="/users/report_spam"}]                 : limit;
          derived_info        [@conv.as {json="/users/derived_info"}]                : limit;
        >;
      followers : <
	  list                [@conv.as {json="/followers/list"}]                    : limit;
	  ids                 [@conv.as {json="/followers/ids"}]                     : limit;
        >;
      statuses : <
	  mentions_timeline   [@conv.as {json="/statuses/mentions_timeline"}]        : limit;
          show                [@conv.as {json="/statuses/show/:id"}]                 : limit;
	  oembed              [@conv.as {json="/statuses/oembed"}]                   : limit;
	  retweeters_ids      [@conv.as {json="/statuses/retweeters/ids"}]           : limit;
	  home_timeline       [@conv.as {json="/statuses/home_timeline"}]            : limit;
	  user_timeline       [@conv.as {json="/statuses/user_timeline"}]            : limit;
	  retweets            [@conv.as {json="/statuses/retweets/:id"}]             : limit;
	  retweets_of_me      [@conv.as {json="/statuses/retweets_of_me"}]           : limit;
          friends             [@conv.as {json="/statuses/friends"}]                  : limit;
          lookup              [@conv.as {json="/statuses/lookup"}]                   : limit;
        >;
      help : <
	  privacy             [@conv.as {json="/help/privacy"}]                      : limit;
          tos                 [@conv.as {json="/help/tos"}]                          : limit;
	  configuration       [@conv.as {json="/help/configuration"}]                : limit;
	  languages           [@conv.as {json="/help/languages"}]                    : limit;
          settings            [@conv.as {json="/help/settings"}]                     : limit;
        >;
      friends : <
	  ids                 [@conv.as {json="/friends/ids"}]                       : limit;
	  list                [@conv.as {json="/friends/list"}]                      : limit;
	  following_ids                 [@conv.as {json="/friends/following/ids"}]                       : limit;
	  following_list                [@conv.as {json="/friends/following/list"}]                      : limit;
        >;
      direct_messages : <
	  show                [@conv.as {json="/direct_messages/show"}]              : limit;
          sent_and_received   [@conv.as {json="/direct_messages/sent_and_received"}] : limit;
	  sent                [@conv.as {json="/direct_messages/sent"}]              : limit;
	  direct_messages     [@conv.as {json="/direct_messages"}]                   : limit;
        >;
      account : <
	  verify_credentials  [@conv.as {json="/account/verify_credentials"}]        : limit;
          settings            [@conv.as {json="/account/settings"}]                  : limit;
          login_verification_enrollment [@conv.as {json="/account/login_verification_enrollment"}] : limit;
          update_profile [@conv.as {json="/account/update_profile"}] : limit;
        >;
      favorites : <
	  list                [@conv.as {json="/favorites/list"}]                    : limit;
        >;
      saved_searches : <
	  destroy             [@conv.as {json="/saved_searches/destroy/:id"}]        : limit;
          list                [@conv.as {json="/saved_searches/list"}]               : limit;
	  show                [@conv.as {json="/saved_searches/show/:id"}]           : limit;
        >;
      search : <
	  tweets              [@conv.as {json="/search/tweets"}]                     : limit;
        >;
      trends : <
	  available           [@conv.as {json="/trends/available"}]                  : limit;
          place               [@conv.as {json="/trends/place"}]                      : limit;
	  closest             [@conv.as {json="/trends/closest"}]                    : limit;
        >;
      mutes : <
          users_list [@conv.as {json="/mutes/users/list"}] : limit;
          users_ids [@conv.as {json="/mutes/users/ids"}] : limit;
        >;
      device : <
          token [@conv.as {json="/device/token"}] : limit;
        >;
     >
  > [@@deriving conv{ocaml; json}]

  let format x =  Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t x

end
