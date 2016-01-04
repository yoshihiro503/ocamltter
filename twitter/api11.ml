open Meta_conv.Open

open Spotlib.Spot
open Spotlib.Result.Open (* Monads are Result *)

open OCamltter_oauth
open Api_intf


(** {6 HTTP parameters} *)

(** We handle HTTP parameters as [(string * string option) list] *)

type params = (string * string option) list

(** [ val (~?) : (string * string option) -> (string * string) list] *)
let (~?) l = List.filter_map (function
  | (key, Some v) -> Some (key, v)
  | (_, None) -> None) l

(** {6 Error} *)

module Error = struct

  type http       = Http.error
  type json_conv  = [ `Json of Api_intf.Json.t Meta_conv.Error.t ]
  type json_parse = [ `Json_parse of exn * string ]
  type t = [ http | json_conv | json_parse ]

  let format ppf = function
    | `Http (code, mes) ->
        Format.fprintf ppf "HTTP error %d: %s" code mes
    | `Curl (_curlCode, code, mes) ->
        Format.fprintf ppf "CURL error %d: %s" code mes
    | `Json e ->
        Format.fprintf ppf "@[<2>JSON error:@ %a@]"
          (Meta_conv.Error.format Json_conv.format) e
    | `Json_parse (exn, s) ->
        Format.fprintf ppf "@[<2>JSON error: %s@ %s@]@."
          (Printexc.to_string exn)
          s
end

type 'a result = ('a, Error.t) Result.t

(** {6 Base communication} *)

let twitter oauth ?(host="api.twitter.com") meth path params =
  (* prerr_endline cmd; *)
  (* List.iter (fun (k,v) -> Format.eprintf "%s=%s@." k v) params; *)
  Oauth.access 
    ~host
    ~path
    ~meth:( match meth with `GET -> `GET [] | `POST -> `POST [] )
    ~oauth_other_params:params 
    oauth >>= fun s -> 
  Result.catch (fun ~fail -> try Json.parse s with e -> fail (`Json_parse (e,s)))
      

(* CR jfuruse: Should we use `JSON instead of `Json? *)
(** To live with other errors like `HTTP, Json decoding erros are
    tagged with `Json. *)
let json_error_wrap f v = match v with
  | `Error e -> `Error e
  | `Ok res ->
      match f res with
      | `Ok v -> `Ok v
      | `Error e -> `Error (`Json e)

type 'a json_converter = Json.t -> ('a, Json.t Meta_conv.Error.t) Result.t 
(** The type of Json to OCaml converter *)

(** 
  [api post meth fmt ...(format args)... params oauth] 

  post : Postprocess: a parser of JSON. If you want to have the raw JSON, use [fun x -> `Ok x].
  meth : GET or POST
  fmt  : piece of path, you can use Printf % format
  params : the type is [params]
  oauth : OAuth value
      
*)
let api post meth fmt = fun params oauth -> 
  Printf.ksprintf (fun name ->
    twitter oauth meth ~host:"api.twitter.com" (!% "/1.1/%s" name) ~?params
    |> json_error_wrap post) fmt

let api' post meth name = fun params oauth -> 
  twitter oauth meth ~host:"api.twitter.com" (!% "/1.1/%s" name) ~?params
      |> json_error_wrap post

(** {6 Argment handling } *)

module Arg = struct

  let (>>|) v f = Option.map f v

  (** {7 to_string functions of option values} *)

  let of_bool  x = x >>| string_of_bool
  let of_int64 x = x >>| Int64.to_string
  let of_int   x = x >>| string_of_int
  let of_float x = x >>| string_of_float
  let of_string (x : string option) = x

  (** {7 Argument generators and consumers } *)

  let optional_args k opts addition = k (addition @ opts)
  (** optional argument function accumulation *)

  let run optf consumer = optf consumer []
  (** "runs" optional argument function [optf] with the init empty
      parameters, then give the final set of parameters to [consumer] *)

  (* Being puzzled? Yes so was I... *)
  let get  post pathfmt optf = run optf & api post `GET  pathfmt
  let post post pathfmt optf = run optf & api post `POST pathfmt

  (** {7 General optional argument generators } *)

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

  type ('a, 'b) opt = (params -> 'a) -> params -> 'b

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

  let contributor_details k opts ?contributor_details = optional_args k opts
    ["contributor_details", of_bool contributor_details]

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

  (** {7 Required argument generators } *)

  (*
    val <name> : (params -> Oauth.t -> 'a)  ->  (params -> Oauth.t -> t -> 'a)

    They are to add new required arguments just after [Oauth.t].

    This must go at the end of argument generator compositions.
  *)

  type ('a, 'b) required_arg = (params -> Oauth.t -> 'a) -> params -> Oauth.t -> 'b -> 'a

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

end

open Arg

(** {6 Cursor based API } *)


module Cursor : sig

  (* I know you do not understand this type. *)    
  val streaming : 
    Http.meth 

    -> ( ('elem, Error.t) Result.t Stream.t -> 'final_result) 
    (*+ final kontinuation *)

    -> 'sublist_record Json_conv.decoder 
    (*+ how to decode the raw result of sublist records *)

    -> ('sublist_record -> 'elem list) 
    (*+ how to get a sublist from a sublist record *)

    -> string  (*+ url piece. CR jfuruse: no printf interface? *)

    -> ((params -> Oauth.t -> 'final_result) -> params -> 'the_function_type)
    (*+ argument accumulator *)

    -> 'the_function_type

end = struct
  type 'a t = { 
    previous_cursor : Json.t;
    next_cursor : Json.t;
    next_cursor_str : string;
    previous_cursor_str : string;
    contents : 'a mc_embeded;
  } [@@deriving conv{ocaml; json}]

  let streaming meth k dec acc s optf =
    optf (fun params oauth ->
      let (!!) = Lazy.force in
      let rec loop cursor = lazy (
        let params = ("cursor", of_string cursor) :: params in
        match api (fun x -> `Ok x) meth "%s" params (oauth : Oauth.t) s with
        | `Error e -> !! (Stream.singleton (`Error e))
        | `Ok json ->
            match t_of_json dec json with
            | `Error e -> !! (Stream.singleton (`Error (`Json e)))
            | `Ok t -> 
                let xs = Stream.of_list (List.map (fun x -> `Ok x) (acc t.contents)) in
                match t.next_cursor_str with
                | "0" -> !!xs
                | next -> !!(Stream.append xs (loop (Some next)))
      )
      in
      k & loop None) []
      
end

(** { 6 since_id + max_id based API } *)

module SinceMaxID = struct
  
  open Stream

  (* count: max number possible *)
  let create_stream f ~count ?since_id ?max_id o = 
    let (!!) = Lazy.force in
    let rec loop ~since_id ~max_id = lazy (
      if Spotlib.Option.liftM2 (>) since_id max_id = Some true then !!null
      else begin
        match f ?count:(Some count) ?since_id ?max_id (o : Oauth.t) with
        | `Error e -> !!(Stream.singleton (`Error e))
        | `Ok [] ->  !!Stream.null
        | `Ok xs ->
            let last_id = (List.last xs)#id in
            let since_id = Some (Int64.( last_id  + 1L )) in
            let xs = Stream.of_list (List.map (fun x -> `Ok x) xs) in
            !! (Stream.append xs (loop ~since_id ~max_id))
      end )
    in
    loop ~since_id ~max_id

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

   * This is called "show".
   * The method is GET.
   * The result JSON is parsed by Tweet.ts_of_json. If you want the raw JSON, 
       use (fun x -> `Ok x).
   * The URL piece is "statuses/show/%Ld.json" 
   * It has trim_user, include_my_tweet and include_entities parameters.
       They are defined in Arg module.

*)

(** {6 The API endpoints } *)

module Timelines = struct (* CR jfuruse: or Statuses ? *)
  (* Careful. The returned JSON may have different type based on the options *)

  let mentions_timeline = get Tweet.ts_of_json "statuses/mentions_timeline.json"
    &  count 
    ** since_max_ids
    ** trim_user
    ** contributor_details
    ** include_entities

  let mentions_timeline_stream  
    ?trim_user ?contributor_details ?include_entities =
    SinceMaxID.create_stream ~count:200
    & mentions_timeline
      ?trim_user ?contributor_details ?include_entities

  let user_timeline = get Tweet.ts_of_json "statuses/user_timeline.json"
    &  count
    ** since_max_ids
    ** user_id_screen_name
    ** trim_user
    ** exclude_replies
    ** contributor_details
    ** include_rts
      
  let user_timeline_stream
      ?user_id ?screen_name ?trim_user ?exclude_replies ?contributor_details ?include_rts =
    SinceMaxID.create_stream ~count:200
    & user_timeline
      ?user_id ?screen_name ?trim_user ?exclude_replies ?contributor_details ?include_rts
      
  let home_timeline = get Tweet.ts_of_json "statuses/home_timeline.json" 
    &  count
    ** since_max_ids
    ** trim_user
    ** exclude_replies
    ** contributor_details
    ** include_entities

  let home_timeline_stream
      ?trim_user ?exclude_replies ?contributor_details ?include_entities =
      SinceMaxID.create_stream ~count:200
      & home_timeline
        ?trim_user ?exclude_replies ?contributor_details ?include_entities
      
  let retweets_of_me = get Tweet.ts_of_json "statuses/retweets_of_me.json" 
    &  count
    ** since_max_ids
    ** trim_user
    ** include_entities
    ** include_user_entities

  let retweets_of_me_stream
      ?trim_user ?include_entities ?include_user_entities =
    SinceMaxID.create_stream ~count:100
    & retweets_of_me
      ?trim_user ?include_entities ?include_user_entities

end

module Tweets = struct (* CR jfuruse: or Statuses ? *)

  (* CR jfuruse: id comes before the o *)    
  let retweets = 
    get Tweet.ts_of_json "statuses/retweets/%Ld.json"
    &  count
    ** trim_user

  (* CR jfuruse: id comes before the o *)    
  let show = get Tweet.t_of_json "statuses/show/%Ld.json" 
    &  trim_user
    ** include_my_tweet
    ** include_entities

  (* CR jfuruse: id comes before the o *)    
  let destroy = post (fun x -> `Ok x) "statuses/destroy/%Ld.json"
    &  trim_user 

  let update = post Tweet.t_of_json "statuses/update.json"
    &  in_reply_to_status_id
    ** geo
    ** trim_user
    ** required_status (* Required argument should come at the last *)

  (* CR jfuruse: id comes before the o *)    
  let retweet = post Tweet.t_of_json "statuses/retweet/%Ld.json"
    &  trim_user 

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
  } [@@deriving conv{ocaml; json}]

  let ids_stream, ids = 
    let f k = 
      Cursor.streaming `GET k
        ids_of_json 
        (fun x -> x.ids)
        (!% "%s/ids.json" A.dir)
      & required_either_user_id_or_screen_name
    in
    f id,
    f Stream.to_list


  type users = {
    users : User.t list
  } [@@deriving conv{ocaml; json}]

  let list_stream, list =
    let f k =
      Cursor.streaming `GET k
        users_of_json
        (fun x -> x.users)
        (!% "%s/list.json" A.dir)
      &  required_either_user_id_or_screen_name
      ** skip_status
      ** include_user_entities

    in
    f id,
    f Stream.to_list

end

module Friends   = FriendsAndFollowers(struct let dir = "friends" end)

module Followers = FriendsAndFollowers(struct let dir = "followers" end)

module Friendships = struct

  type connection = [ `Following          [@conv.as {json="following"}]
                    | `Follwing_requested [@conv.as {json="following_requested"}]
                    | `Followed_by        [@conv.as {json="followed_by"}]
                    | `None               [@conv.as {json="none"}] 
                    ] [@@deriving conv{ocaml; json}]

  type t = <
      name : string;
      screen_name : string;
      id : int64;
      id_str : string;
      connections : connection list
  > [@@deriving conv{ocaml; json}]

  type ts = t list [@@deriving conv{ocaml; json}]

  let lookup ?screen_name ?user_id oauth =
    api ts_of_json `GET "friendships/lookup.json"
      [ "screen_name", 
        Option.map (String.concat ",") screen_name

      ; "user_id", 
        Option.map (String.concat "," ** List.map Int64.to_string) user_id
      ]
      oauth

  type ids = {
    ids : int64 list;
  } [@@deriving conv{ocaml; json}]

  let gen_io name k = 
    Cursor.streaming `GET k
      ids_of_json 
      (fun x -> x.ids)
      (!% "friendships/%s.json" name)
    & id

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
  } [@@deriving conv{ocaml; json}]

  let list_stream, list = 
    let f k = 
      Cursor.streaming `GET k
        users_of_json 
        (fun x -> x.users)
        "blocks/list.json"
      & include_entities
      ** skip_status
    in
    f id,
    f Stream.to_list

  type ids = {
    ids : int64 list;
  } [@@deriving conv{ocaml; json}]

  let ids_stream, ids = 
    let f k = 
      Cursor.streaming `GET k
        ids_of_json 
        (fun x -> x.ids)
        "blocks/ids.json"
      & id (* omitting stringify_ids *)
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

  let stream 
    ?user_id
    ?screen_name
    ?include_entities =
    SinceMaxID.create_stream ~count:200
    & list
      ?user_id 
      ?screen_name 
      ?include_entities

  let create = post Tweet.t_of_json "favorites/create.json"
    &  include_entities
    ** required_id

  let destroy = post Tweet.t_of_json "favorites/destroy.json"
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

  let report_spam = post User.t_of_json "users/report_spam.json"
    &  required_either_user_id_or_screen_name

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

