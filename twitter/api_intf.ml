open Spotlib.Spot
open Tiny_json

open Meta_conv.Open
open Json_conv
open Ocaml_conv

module Json = struct
  include Json
  let json_of_t x = x
  let t_of_json ?trace:_ x = `Ok x

  let ocaml_of_t t = Ocaml.String (Json.show t)
  let t_of_ocaml = Meta_conv.Internal.prim_decode (function
    | Ocaml.String s -> Json.parse s
    | _ -> failwith "Ocaml.String expected")
end

type 'json mc_leftovers = (string * 'json) list with conv(ocaml)

type status_id = int64 with conv(json, ocaml)

module Time : sig
  type t with conv(json, ocaml)
  val from_unix : float -> t
  val to_unix : t -> float
  val from_string : string -> t
  val to_string : t -> string
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
  } with conv(ocaml)

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
  let t_of_json = Meta_conv.Internal.prim_decode (function
    | String s -> from_string s
    | _ -> failwith "Time.t_of_json: String expected")
  let t_of_json_exn = Json_conv.exn t_of_json
end

module Text : sig 
  (* HTML encoded text *)
  type t = string with conv(json, ocaml)
end = struct
  type t = string with conv(ocaml)

  open Json

  let t_of_json = Meta_conv.Internal.prim_decode (function
    | String s -> Http.html_decode s
    | _ -> failwith "Text.t_of_json: String expected")

  let t_of_json_exn = Json_conv.exn t_of_json

  let json_of_t _t = assert false
end

module Client : sig
  type t = string * [`Ok of Xml.xml | `Error of exn] with conv(json, ocaml)
end = struct
  type t = string * [`Ok of Xml.xml | `Error of exn]

  open Meta_conv
  open Json

  let t_of_json = Meta_conv.Internal.prim_decode & function
    | String s -> s, Result.catch_exn & fun () -> Xml.parse_string s
    | _ -> failwith "Client.t_of_json: String expected"

  let t_of_json_exn = Json_conv.exn t_of_json

  let json_of_t (s, _) = String s

  let t_of_ocaml ?trace:_ _ = assert false
  let t_of_ocaml_exn ?trace:_ _ = assert false
  let ocaml_of_t (s, _) = Ocaml.String s   
end

module User = struct
  (* Lots of optional fields! *)

  type details = <
      is_translator : bool;
      (* profile_background_color : string; *)
      (* notifications : bool option; *)
      profile_image_url_https : string;
      url : string option;
      (* profile_background_image_url_https : string *)
      created_at : Time.t;
      (* profile_background_image_url : string; *)
      (* utc_offset : float *)
      (* id_str : string *)
      (* profile_link_color : string *)
      name : string;
      default_profile : bool;
      screen_name : string;
      lang : string;
      protected : bool;
      statuses_count : int64;
      location : string option;
      (* profile_use_background_image : bool; *)
      (* profile_text_color : string; *)
      (* contributors_enabled : bool; *) 
      listed_count : int64;
      time_zone : string option;
      description : string;
      profile_image_url : string;
      (* profile_sidebar_border_color : string *)
      following : bool option;
      geo_enabled : bool;
      (* profile_background_tile : bool *)
      followers_count : int64;
      (* profile_sidebar_fill_color : string; *)
      verified : bool;
      (* status : Twitter.Json.Object ... *)
      (* default_profile_image : bool *)
      follow_request_sent : bool option;
      friends_count : int64;
      favourites_count : int64 
  > with conv(json, ocaml)

  type t = <
    unknowns : Json.t mc_leftovers;
    id : int64;
    details: details mc_option_embeded;
  > with conv(json, ocaml)

  type ts = t list with conv(json, ocaml)

  let format =  Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t
end

module Hashtag = struct
  type t = <
    text    : string;
    indices : (int * int) (** location in the tweet string *)
  > with conv(json, ocaml)
end

module URL = struct
  type t = <
    unknown : Json.t mc_leftovers;

    url          : string;
    expanded_url : string;
    display_url  : string;
  > with conv(json, ocaml)
end
 
module UserMention = struct
  type t = <
      unknown : Json.t mc_leftovers;

      screen_name : string;
      name : string;
      id : int64;
      indices : int * int;
  > with conv(json, ocaml)
      
end

module Entities = struct
  type t = <
      unknown : Json.t mc_leftovers;

      hashtags : Hashtag.t list; 
      urls     : URL.t list;
      user_mentions : UserMention.t list;
  > with conv(json, ocaml)
end

module Tweet = struct
  (* CR jfuruse: exclude_replies ... unknown 
     include_user_entities ... unknown
  *)

  type t = <
    unknowns : Json.t mc_leftovers;

    id : int64;
    user : User.t;

    text : Text.t;
    truncated : bool;

    in_reply_to_status_id   : int64 option; (* RE or RT *)
    in_reply_to_user_id     : int64 option;
    in_reply_to_screen_name : string option;
    retweeted_status        : t mc_option; (* RT *)

    created_at : Time.t;

    source : Client.t; (* html piece *)
    geo : Json.t option;
    coordinates : Json.t option;
    place : Json.t option;

    contributors : Json.t option; (* something can be trimed... *)

    retweet_count : int;
    favorited : bool;
    retweeted : bool;

    possibly_sensitive : bool mc_option;
    entities : Entities.t mc_option;

  > with conv(json, ocaml)

  type ts = t list with conv(json, ocaml)

  let format =  Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t
  let format_ts = Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_ts
end

module Search_tweets = struct

  module Search_metadata = struct
    type t = <
      unknowns : Json.t mc_leftovers;

      completed_in : float;
      max_id : int64;
      (* max_id_str : string; *)
      next_results : string;
      query : string;
      refresh_url : string;
      count : int;
      since_id : int64;
      (* since_id_str : string; *)
    > with conv(json, ocaml)
  end

  type t = <
    unknowns : Json.t mc_leftovers;

    statuses : Tweet.t list;
(*     search_metadata : Search_metadata.t;*)
  > with conv(json, ocaml)

  let format = Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t

end

module Rate_limit_status = struct

  type t = <
    rate_limit_context : < access_token : string >;
    resources : < 
      lists           : Json.t;
      application     : Json.t;
      friendships     : Json.t;
      blocks          : Json.t;
      geo             : Json.t;
      users           : Json.t;
      followers       : Json.t;
      statuses        : Json.t;
      help            : Json.t;
      friends         : Json.t;
      direct_messages : Json.t;
      account         : Json.t;
      favorites       : Json.t;
      saved_searches  : Json.t;
      search          : Json.t;
      trends          : Json.t;
     >
  > with conv(json, ocaml)

  let format =  Ocaml.format_with ~no_poly:true ~raw_string:true ocaml_of_t

end
