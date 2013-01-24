open Meta_conv.Open
open Meta_conv.Result.Open
open Ocaml_conv
open Json_conv

open Spotlib.Spot

open Http
open Api_intf


(** {6 HTTP parameters} *)

(** We handle HTTP parameters as [(string * string option) list] *)

type params = (string * string option) list

(** Coercing to the normal HTTP header type, [(string * string) list]. *)
let (~?) l = List.filter_map (function
  | (key, Some v) -> Some (key, v)
  | (_, None) -> None) l

(** {6 Base communication} *)

(* Api 1.1 seems to requir https *)
let twitter oauth ?(host="api.twitter.com") meth cmd params =
  (* prerr_endline cmd; *)
  (* List.iter (fun (k,v) -> Format.eprintf "%s=%s@." k v) params; *)
  Auth.access_https oauth meth host cmd params
  >>| Json.parse

(* CR jfuruse: Should we use `JSON instead of `Json? *)
(** To live with other errors like `HTTP, Json decoding erros are
    tagged with `Json. *)
let json_error_wrap f v = match v with
  | `Error e -> `Error e
  | `Ok res ->
      match f res with
      | `Ok v -> `Ok v
      | `Error e -> `Error (`Json e)

(** 
  [api post meth fmt ...(format args)... params oauth] 

  post : Parser of JSON. If you want to have the raw JSON, use [fun x -> `Ok x].
  meth : GET or POST
  fmt  : piece of path, you can use Printf % format
  params : the type is [params]
  oauth : OAuth value
      
*)
let api post meth fmt = 
  Printf.ksprintf (fun name params oauth -> 
    twitter oauth meth ~host:"api.twitter.com" (!% "/1.1/%s" name) ~?params
    |> json_error_wrap post
  ) fmt

(** { 6 Argment handling } *)

module Arg = struct

  let (>>|) v f = Option.map ~f v

  (** { 7 to_string functions of option values *)

  let of_bool  x = x >>| string_of_bool
  let of_int64 x = x >>| Int64.to_string
  let of_int   x = x >>| string_of_int
  let of_float x = x >>| string_of_float
  let of_string (x : string option) = x

  (** { 7 Argument generators and consumers } *)

  let optional_args k opts addition = k (addition @ opts)
  (** optional argument function accumulation *)

  let run optf consumer = optf consumer []
  (** "runs" optional argument function [optf] with the init empty
      parameters, then give the final set of parameters to [consumer] *)

  (* Being puzzled? Yes so was I... *)
  let get post pathfmt optf  = run optf & api post GET  pathfmt
  let post post pathfmt optf = run optf & api post POST pathfmt

  (** { 7 General optional argument generators } *)

  (* Here, we define the set of optional argument generators 
     in quite a "functional" way. 
     Those generators can be composed with functional composition 
     [Spotlib.Spot.(**)].

     If you want to add new optional args, you do not really understand 
     what they do, just use the following format:

     let <name> k opts ?<new_opt_arg1> ... ?<new_opt_argn> = optional_args k opts
       [ "<new_opt_arg1_name>", of_<type> <new_opt_arg1>
       ; ...
       ; "<new_opt_argn_name>", of_<type> <new_opt_argn>
       ]
  *)
     
  (*
    val <name> : (params -> 'a)  ->  (params -> ?<name>:<t> -> 'a)

    They are to add new arguments just after [params].
  *)

  let count k opts ?count = optional_args k opts 
    ["count" , of_int count]

  let since_max_ids k opts ?since_id ?max_id = optional_args k opts
    [ "since_id" , of_int64 since_id
    ; "max_id"   , of_int64 max_id
    ]

  let since_id k opts ?since_id = optional_args k opts
    [ "since_id" , of_int64 since_id
    ]

  let trim_user k opts ?trim_user = optional_args k opts
    ["trim_user" , of_bool  trim_user]

  let contributer_details k opts ?contributer_details = optional_args k opts
    ["contributer_details", of_bool contributer_details]

  let include_entities k opts ?include_entities = optional_args k opts
    ["include_entities" , of_bool  include_entities]

  let include_my_tweet k opts ?include_my_tweet = optional_args k opts
    ["include_my_tweet" , of_bool  include_my_tweet]

  let include_user_entities k opts ?include_user_entities = optional_args k opts
    ["include_user_entities" , of_bool  include_user_entities]

  let user_id_screen_name k opts ?user_id ?screen_name = optional_args k opts
    [ "screen_name" , of_string screen_name
    ; "user_id"     , of_int64 user_id ]

  let required_either_user_id_or_screen_name k opts ?user_id ?screen_name = 
    match user_id, screen_name with
    | None, None -> failwithf "Neither user_id nor screen_name is set"
    | _ -> 
        optional_args k opts
          [ "screen_name" , of_string screen_name
          ; "user_id"     , of_int64 user_id ]

  let include_rts k opts ?include_rts = optional_args k opts
    ["include_rts" , of_bool  include_rts]

  let exclude_replies k opts ?exclude_replies = optional_args k opts
    ["exclude_replies" , of_bool exclude_replies]

  let skip_status k opts ?skip_status = optional_args k opts
    ["skip_status" , of_bool skip_status]

  let in_reply_to_status_id k opts ?in_reply_to_status_id = optional_args k opts
    ["in_reply_to_status_id" , of_int64 in_reply_to_status_id]

  let geo k opts ?lat ?long ?place_id ?display_coordinates = optional_args k opts
    [ "lat"                   , of_float  lat
    ; "long"                  , of_float  long
    ; "place_id"              , of_string place_id
    ; "display_coordinates"   , of_bool   display_coordinates]

  let cursor k opts ?cursor = optional_args k opts
    ["cursor" , of_string cursor]

  let resources k opts ?resources = optional_args k opts
    [ "resources" , resources >>| String.concat ","
    ]

  let follow k opts ?follow = optional_args k opts
    [ "follow", of_bool follow ]

  (** { 7 Required argument generators } *)

  (*
    val <name> : (params -> Oauth.t -> 'a)  ->  (params -> Oauth.t -> t -> 'a)

    They are to add new required arguments just after [Oauth.t].

    This must go at the end of argument generator compositions.
  *)

  let required_args f (params : params) (oauth : Oauth.t) addition = 
    f (addition @ params) oauth

  let required_arg make_addition = fun f (params : params) (oauth : Oauth.t) x -> 
    f (make_addition x @  params) oauth

  let required_status = fun x -> x |> 
      required_arg (fun status -> [ "status", of_string (Some status) ])
      
  let required_q = fun x -> x |>
      required_arg (fun q -> [ "q", Some q ])

  let required_id = fun x -> x |>
      required_arg (fun id -> [ "id", Some (Int64.to_string id) ])

  (** { 7 Format argument generators } *)

  (** Introduce format string arguments. They must appear at the end of
      function compositions, just before the call of api.
  *)
  let format1 optf = fun params x -> optf x params
  let format2 optf = fun params x1 x2 -> optf x1 x2 params
  let format3 optf = fun params x1 x2 x3 -> optf x1 x2 x3 params

end

open Arg

(** { 6 Cursor based API } *)


module Cursor : sig

  (* I know you do not understand this type. *)    
  val get_stream : 
    ((params -> Oauth.t -> 'final_result) -> params -> 'the_function_type)
    (** argument accumulator *)

    -> 'sublist_record Json_conv.decoder 
       (** how to decode the raw result of sublist records *)

    -> ('sublist_record -> 'elem list) 
       (** how to get a sublist from a sublist record *)

    -> (params 
        -> Oauth.t 
        -> [< `Error of [> `Json of Json.t Meta_conv.Error.t ] as 'error
           | `Ok of Json.t ]) 
       (** retrieval function *)

    -> ([> `Error of 'error | `Ok of 'elem ] Stream.t -> 'final_result) 
       (** final kontinuation *)

    -> 'the_function_type

end = struct
  type 'a t = { 
    previous_cursor : Json.t;
    next_cursor : Json.t;
    next_cursor_str : string;
    previous_cursor_str : string;
    contents : 'a mc_embeded;
  } with conv(json, ocaml)

  let get_stream optf dec acc apicall k =
    optf (fun params oauth ->
      let rec loop cursor = 
        let params = ("cursor", of_string cursor) :: params in
        match apicall params (oauth : Oauth.t) with
        | `Error e -> Stream.singleton (`Error e)
        | `Ok json ->
            match t_of_json dec json with
            | `Error e -> Stream.singleton (`Error (`Json e))
            | `Ok t -> 
                let xs = Stream.of_list (List.map (fun x -> `Ok x) (acc t.contents)) in
                match t.next_cursor_str with
                | "0" -> xs
                | next -> Stream.append xs (loop (Some next))
      in
      k & loop None) []
      
end

(* Basic API function can be implemented by the following simple rule,
   even if you do not understand the funcitonal tricks I used here.

   let <name> = <get/post> <postprocess_function> <path format>
     &  <argment_functions concatenated by **>

   So for example,

      let show = get Tweet.ts_of_json "statuses/show/%Ld.json" 
        &  trim_user
        ** include_my_tweet
        ** include_entities
        ** format1

   * This is called "show".
   * The method is GET.
   * The result JSON is parsed by Tweet.ts_of_json. If you want the raw JSON, 
       use (fun x -> `Ok x).
   * The URL piece is "statuses/show/%Ld.json" 
   * It has trim_user, include_my_tweet and include_entities parameters.
       They are defined in Arg module.
   * It also takes one format argument for "%Ld", so format1 is added
     at the end.

*)

(** { 6 The API endpoints } *)

module Timelines = struct (* CR jfuruse: or Statuses ? *)
  (* Careful. The returned JSON may have different type based on the options *)

  let mentions_timeline = get Tweet.ts_of_json "statuses/mentions_timeline.json"
    &  count 
    ** since_max_ids
    ** trim_user
    ** contributer_details
    ** include_entities

  let user_timeline = get Tweet.ts_of_json "statuses/user_timeline.json"
    &  count
    ** since_max_ids
    ** user_id_screen_name
    ** trim_user
    ** exclude_replies
    ** contributer_details
    ** include_rts

  let home_timeline = get Tweet.ts_of_json "statuses/home_timeline.json" 
    &  count
    ** since_max_ids
    ** trim_user
    ** exclude_replies
    ** contributer_details
    ** include_entities

  let retweets_of_me = get Tweet.ts_of_json "statuses/retweets_of_me.json" 
    &  count
    ** since_max_ids
    ** trim_user
    ** include_entities
    ** include_user_entities

end

module Tweets = struct (* CR jfuruse: or Statuses ? *)

  let retweets = get Tweet.ts_of_json "statuses/retweets/%Ld.json"
    &  count
    ** trim_user
    ** format1

  let show = get Tweet.t_of_json "statuses/show/%Ld.json" 
    &  trim_user
    ** include_my_tweet
    ** include_entities
    ** format1

  let destroy = post (fun x -> `Ok x) "statuses/destroy/%Ld.json"
    &  trim_user 
    ** format1

  let update = post Tweet.t_of_json "statuses/update.json"
    &  in_reply_to_status_id
    ** geo
    ** trim_user
    ** required_status (* Required argument should come at the last *)

  let retweet = post Tweet.t_of_json "statuses/retweet/%Ld.json"
    &  trim_user 
    ** format1

  (* not yet: update_with_media *)
  (* not yet: oembed *)
end

module Search = struct

  let tweets = get Search_tweets.t_of_json "search/tweets.json" 
    &  count
    ** since_id
    ** required_q
      
end

module Streaming = struct
end

module DirectMessages = struct
end

module FriendsAndFollowers(A : sig
  val dir : string (** "friends" or "followers" *)
end) = struct

  type ids = {
    ids : int64 list;
  } with conv(json, ocaml)

  let ids_stream, ids = 
    let f k = 
      (* Cursor.get ids_of_json (fun x -> x.ids) "%s/ids.json" A.dir
         & required_either_user_id_or_screen_name *)
      Cursor.get_stream
        required_either_user_id_or_screen_name
        ids_of_json 
        (fun x -> x.ids)
        (api (fun x -> `Ok x) GET "%s/ids.json" A.dir)
        k
    in
    f id,
    f Stream.to_list


  type users = {
    users : User.t list
  } with conv(json, ocaml)

  let list_stream, list =
    let f k =
      Cursor.get_stream
        (required_either_user_id_or_screen_name
         ** skip_status
         ** include_user_entities)
        users_of_json
        (fun x -> x.users)
        (api (fun x -> `Ok x) GET "%s/list.json" A.dir)
        k
    in
    f id,
    f Stream.to_list

end

module Friends   = FriendsAndFollowers(struct let dir = "friends" end)

module Followers = FriendsAndFollowers(struct let dir = "followers" end)

module Friendships = struct

  type connection = [ `Following          as "following"
                    | `Follwing_requested as "following_requested"
                    | `Followed_by        as "followed_by"
                    | `None               as "none" 
                    ] with conv(json, ocaml)

  type t = <
      name : string;
      screen_name : string;
      id : int64;
      id_str : string;
      connections : connection list
  > with conv(json, ocaml)

  type ts = t list with conv(json, ocaml)

  let lookup ?screen_name ?user_id oauth =
    api ts_of_json GET "friendships/lookup.json"
      [ "screen_name", 
        Option.map ~f:(String.concat ",") screen_name

      ; "user_id", 
        Option.map ~f:(String.concat "," ** List.map Int64.to_string) user_id
      ]
      oauth

  type ids = {
    ids : int64 list;
  } with conv(json, ocaml)

  let gen_io name k = 
    Cursor.get_stream
      id
      ids_of_json 
      (fun x -> x.ids)
      (api (fun x -> `Ok x) GET "friendships/%s.json" name)
      k

  let incoming_stream = gen_io "incoming" id
  let incoming        = gen_io "incoming" Stream.to_list

  let outgoing_stream = gen_io "outgoing" id
  let outgoing        = gen_io "outgoing" Stream.to_list

  let create = post User.t_of_json "friendships/create.json"
    &  required_either_user_id_or_screen_name
    ** follow
    
  let follow = create

  let destroy = post User.t_of_json "friendships/destroy.json"
    &  required_either_user_id_or_screen_name

  let unfollow = destroy
end

module Users = struct

end 

module Blocks = struct
  type users = {
    users : User.t list
  } with conv(json, ocaml)

  let list_stream, list = 
    let f k = 
      Cursor.get_stream
        (include_entities
         ** skip_status)
        users_of_json 
        (fun x -> x.users)
        (api (fun x -> `Ok x) GET "blocks/list.json")
        k
    in
    f id,
    f Stream.to_list

  type ids = {
    ids : int64 list;
  } with conv(json, ocaml)

  let ids_stream, ids = 
    let f k = 
      Cursor.get_stream 
        id (* omitting stringify_ids *)
        ids_of_json 
        (fun x -> x.ids)
        (api (fun x -> `Ok x) GET "blocks/ids.json")
        k
    in
    f id,
    f Stream.to_list

  let create = post (fun x -> `Ok x) "blocks/create.json"
    &  required_either_user_id_or_screen_name
    ** include_entities
    ** skip_status

  let destroy = post (fun x -> `Ok x) "blocks/destroy.json"
    &  required_either_user_id_or_screen_name
    ** include_entities
    ** skip_status

end
    
module SuggestedUsers = struct
end

module Favorites = struct

  let list = get Tweet.ts_of_json "favorites/list.json"
    &  user_id_screen_name
    ** count
    ** since_max_ids
    ** include_entities

  let create = get Tweet.t_of_json "favorites/create.json"
    &  include_entities
    ** required_id

  let destroy = get Tweet.t_of_json "favorites/destroy.json"
    &  include_entities
    ** required_id

end

module Lists = struct
end

module SavedSearches = struct
end

module PlacesAndGeo = struct
end

module Trends = struct
end

module SpamReporting = struct
end 

module OAuth = struct
(*
GET oauth/authenticate	 Allows a Consumer application to use an OAuth request_token to request user authorization. This method is a replacement of Section 6.2 of the OAuth 1.0 authentication flow for applications using the callback authentication flow. The method will use the currently logged in user as the account for...
GET oauth/authorize	 Allows a Consumer application to use an OAuth Request Token to request user authorization. This method fulfills Section 6.2 of the OAuth 1.0 authentication flow. Desktop applications must use this method (and cannot use GET oauth/authenticate). Please use HTTPS for this method, and all other OAuth...
POST oauth/access_token	 Allows a Consumer application to exchange the OAuth Request Token for an OAuth Access Token. This method fulfills Section 6.3 of the OAuth 1.0 authentication flow. The OAuth access token may also be used for xAuth operations. Please use HTTPS for this method, and all other OAuth token negotiation...
POST oauth/request_token	 Allows a Consumer application to obtain an OAuth Request Token to request user authorization. This method fulfills Section 6.1 of the OAuth 1.0 authentication flow. It is strongly recommended you use HTTPS for all OAuth authorization steps. Usage Note: Only ASCII values are accepted for the...*)
end

module Help = struct

  let rate_limit_status = get Rate_limit_status.t_of_json "application/rate_limit_status.json"
    &  resources
      
end
