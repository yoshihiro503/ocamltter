open Spotlib.Spot
open Ocaml_conv
open Json_conv
open Result

open OCamltter_oauth

module Oauth = struct
  include Oauth_ex.Make(Conf)

  let load_acc_token auth_file =
    match Ocaml.load_with_exn Access_token.t_of_ocaml auth_file with
    | [a] -> a
    | _ -> assert false
  
  let get_acc_token auth_file =
    try load_acc_token auth_file with
    | _ -> 
        let _res, acc_token = authorize_cli_interactive () in
        Ocaml.save_with Access_token.ocaml_of_t ~perm:0o600 auth_file [acc_token];
        acc_token
  
  let get_oauth auth_file =
    let acc_token = get_acc_token auth_file in
    oauth Conf.app acc_token
end


let raw_api o meth m fields = 
  Oauth.access o
    ~host: "api.flickr.com"
    ~path: "/services/rest"
    ~meth: (match meth with `POST -> `POST [] | `GET -> `GET [])
    ~oauth_other_params: 
      ( [ "method", m
        ; "format", "json" ]
        @ fields )

type 'json mc_leftovers = (string * 'json) list [@@deriving conv{ocaml}]
type 'a mc_option = 'a option [@@deriving conv{ocaml}]

module Json = struct
  include Tiny_json.Json
  let ocaml_of_t t = Ocaml.String (show t)
  let t_of_ocaml = Ocaml_conv.Helper.of_deconstr (function
    | Ocaml.String s -> parse s
    | _ -> failwith "Ocaml.String expected")
 
  let ocaml_of_exn = ocaml_of_string *< Exn.to_string

  type error = 
    | NotObject of t
    | InvalidField of string
    | CastErr of string
    | UnknownErr of string
    | NotJsonErr of exn
    | NoJsonResponse
  [@@deriving conv{ocaml_of}]

  let format_error = Ocaml.format_with ocaml_of_error

  let of_exn = function
    | JSON_NotObject t -> NotObject t
    | JSON_InvalidField s -> InvalidField s
    | JSON_CastErr s -> CastErr s
    | JSON_UnknownErr s -> UnknownErr s
    | exn -> NotJsonErr exn

  let parse s = try `Ok (parse s) with exn -> `Error (`Json (of_exn exn, s))

  type sint = int [@@deriving conv{ocaml}]

  let sint_of_json ?(trace=[]) = function
    | (Number _ as j) -> int_of_json ~trace j
    | (String s as t) -> 
        begin try `Ok (int_of_string s) with
        | e -> `Error (`Exception e, t, `Node t :: trace)
        end
    | t -> `Error (`Exception (Failure "Number or String expected"), t, `Node t :: trace)

  let json_of_sint = json_of_int

  type sint64 = int64 [@@deriving conv{ocaml}]

  let sint64_of_json ?(trace=[]) = function
    | (Number _ as j) -> int64_of_json ~trace j
    | (String s as t) -> 
        begin try `Ok (Int64.of_string s) with
        | e -> `Error (`Exception e, t, `Node t :: trace)
        end
    | t -> `Error (`Exception (Failure "Number or String expected"), t, `Node t :: trace)

  let json_of_sint64 = json_of_int64

  type ibool = bool [@@deriving conv{ocaml}]

  let ibool_of_json ?trace j = do_;
    n <-- sint_of_json ?trace j;
    return & n <> 0

  let json_of_ibool b = Number (if b then "1" else "0")

  (** Lift x_of_json errors *)
  let lift_error f s = match f s with
    | (`Ok _ as v) -> v
    | `Error e -> `Error (`Json_conv e)
end

open Json

module Fail = struct

  type t = < 
    stat : string (** "fail" *);
    code : sint;
    message : string 
  > [@@deriving conv{ocaml; json}]

  let format x = Ocaml.format_with ocaml_of_t x

  let check j =
    match lift_error t_of_json j with
    | `Ok e when e#stat = "fail" -> `Error (`API e)
    | _ -> `Ok j
end

module Content = struct
  type raw_content = < content [@conv.as {json="_content"}] : string >
    [@@deriving conv{json}]

  type t = string [@@deriving conv{ocaml}]

  let json_of_t x = json_of_raw_content (object method content = x end)
  let t_of_json ?trace x = do_;
    o <-- raw_content_of_json ?trace x;
    return (o#content : string)
      
  let t_of_json_exn = exn t_of_json
end

module EmptyResp = struct
  type t = < stat : string > [@@deriving conv{ocaml; json}]

  let check j = lift_error t_of_json j >>| fun _ -> ()
end

module Error = struct
  type t =
    [ `API of Fail.t
    | `Curl of Curl.curlCode * int * string
    | `Http of int * string
    | `Json of Json.error * string
    | `Json_conv of Json.t Meta_conv.Error.t ]
end

(* Flickr's JSON response is always surrounded by

     jsonFlickrApi({ stats: <ok/fail>
                   ; k = ... })
*)
let json_api o meth m fields = do_;
  s <-- raw_api o meth m fields;
  (* jsonFlickrApi(JSON) *)
  let len = String.length s in
  match 
    String.sub s 0 14,
    String.sub s 14 (len - 15),
    s.[len-1]
  with
  | "jsonFlickrApi(", s, ')' ->
      (* Result has always the top field "stat" *)
      Json.parse s >>= Fail.check
  | exception _ -> `Error (`Json (Json.NoJsonResponse, s))
  | _ -> `Error (`Json (Json.NoJsonResponse, s))

type ('a, 'error) result = ('a, ([> Error.t] as 'error)) Result.t 
    
(** Option field constructor *)
let opt f k = function
  | None -> None
  | Some v -> Some (k, f v)

let api_key = ("api_key", App.app.Oauth.Consumer.key)
    
module Auth = struct
  module Oauth = struct
    let checkToken (* _oauth_token *) o =
      Job.create & fun () ->
        json_api o `GET "flickr.auth.oauth.checkToken"
          [ api_key
          (* ; "oauth_token", oauth_token *)
          ]
  end
end

module Page = struct

  (** Page to stream interface *)

  let to_seq
      : (per_page: int -> page: int -> ('res, 'error) Job.t)
      -> per_page:int
      -> ('res -> 'info)
      -> ('res -> 'a list)
      -> (('info * ('a, 'error) Job.Seq.t), 'error) Job.t
    = fun f ~per_page get_info get_list ->
    let open Job in
    let info = ref None in
    let rec loop page : ('a list, 'error) Job.Seq.t = do_;
      res <-- f ~per_page ~page;
      (); begin match !info with
          | Some _ -> ()
          | None -> info := Some (get_info res)
          end;
      (); assert (res#perpage = per_page);
      let final = res#perpage * res#page >= res#total in
      let next =
        if final then Job.return `None
        else loop (page+1)
      in
      return & `Some (get_list res, next)
    in
    do_
    ; seq <-- loop 1
    ; match !info with
      | None -> assert false
      | Some info -> return (info, Seq.flatten & return seq)

  let to_list x = Job.do_
    ; (info, seq) <-- x
    ; xs <-- Job.of_seq seq;
    return (info, xs)

end

module TagList = struct
  type t = string list [@@deriving conv{ocaml}]

  let json_of_t xs = json_of_string (String.concat " " xs)
  let t_of_json ?trace j =
    string_of_json ?trace j >>= fun s -> 
    return & String.split (function ' ' -> true | _ -> false) s
  let t_of_json_exn = Json_conv.exn t_of_json
end

(* ************************* APIs ***********************)
  
module Photos = struct  

(*
  let int_of_privacy_filter = function
    | `Public -> 1
    | `Friends -> 2
    | `Family -> 3
    | `Friends_and_family -> 4
    | `Private -> 5

  module Media = struct
    type t = [`All     as "all"
             | `Photos as "photos"
             | `Videos as "videos" ] 
    [@@deriving conv{ocaml; json}]
  end
*)

  module GetNotInSet = struct
     
    type resp = < 
      photos : photos;
      stat   : string; 
    >

    and photos = <
      page    : sint;
      pages   : sint;
      perpage : sint;
      total   : sint;
      photo   : photo list;
    >

    and photo = <
      id       : string;
      owner    : string;
      secret   : string;
      server   : string;
      farm     : sint;
      title    : string;
      ispublic : ibool;
      isfriend : ibool;
      isfamily : ibool;

      (* optional fields *)

      tags : TagList.t mc_option;
    >
    [@@deriving conv{ocaml; json}]
  end

  let raw_getNotInSet ~per_page ~page (* ?privacy_filter ?media *) 
      ?(tags=false) o = 
    assert (page > 0);
    let tags = if tags then Some "tags" else None in
    let extras =
      match List.filter_map id [ tags ] with
      | [] -> None
      | xs -> Some (String.concat "," xs)
    in
    Job.create & fun () ->
      json_api o `GET "flickr.photos.getNotInSet"
        ([ api_key
         ; "per_page", string_of_int per_page
         ; "page", string_of_int page
         ]
         @ List.filter_map id
           [ opt id "extras" extras ]
        )
      >>= lift_error GetNotInSet.resp_of_json
      >>| fun x -> x#photos

  let getNotInSet ?(per_page=100) ?tags o =
    Page.to_seq (raw_getNotInSet ?tags o) ~per_page
      (fun res -> res#total)
      (fun res -> res#photo)

  let getNotInSet' ?per_page ?tags o =
    getNotInSet ?per_page ?tags o |> Page.to_list

  module GetInfo = struct

    type owner = <
        nsid     : string;
        username : string;
        realname : string;
        location : string;
        unknowns : Json.t mc_leftovers;
      >
    
    and dates = < 
        posted   : sint64;
        taken    : string;
        unknowns : Json.t mc_leftovers 
      >

    and urls = < url: type_content list >

    and type_content = < 
        type_   [@conv.as {json="type"}]     : string; (* "photopage", *)
        content [@conv.as {json="_content"}] : string 
      >

    and resp = < photo: photo; stat: string >

    and photo = <
        id             : string;
        secret         : string;
        server         : string;
        farm           : sint;
        dateuploaded   : sint64;
        isfavorite     : ibool;
        license        : sint;
        safety_level   : sint;
        rotation       : sint;
        originalsecret : string; (* "edf076f1f2" *)
        originalformat : string; (* "jpg" *)
        owner          : owner;
        title          : Content.t;
        description    : Content.t;
        visibility     : visibility;
        dates          : dates;
        views          : sint;
        comments       : Content.t;
        urls           : urls;
        media          : string; (* "photo" *)
        unknowns       : Json.t mc_leftovers;
      >

    and visibility = < ispublic: ibool; isfriend: ibool; isfamily: ibool >

    [@@deriving conv{ocaml; json}]

  end
      

  let getInfo photo_id o =
    Job.create & fun () ->
      json_api o `GET "flickr.photos.getInfo"
        [ api_key
        ; "photo_id", photo_id
        ]
      >>= lift_error GetInfo.resp_of_json
      >>| fun x -> x#photo
    
  module GetExif = struct

    type exif = <
        tagspace   : string;
        tagspaceid : sint;
        tag        : string;
        label      : string;
        raw        : Content.t;
        clean      : string mc_option
      >

    and resp = < photo : photo; stat : string >

    and photo = <
        id      : string;
        secret  : string;
        camera  : string;
        exif    : exif list;
        unknown : Json.t mc_leftovers 
      >
    [@@deriving conv{ocaml; json}]

  end
                  
  let getExif photo_id o =
    Job.create & fun () ->
      json_api o `GET "flickr.photos.getExif"
        [ api_key
        ; "photo_id", photo_id
        ]
      >>= lift_error GetExif.resp_of_json
      >>| fun x -> x#photo  

  let addTags photo_id tags o =
    Job.create & fun () ->
      json_api o `GET "flickr.photos.addTags"
        [ api_key
        ; "photo_id", photo_id
        ; "tags", String.concat " " tags
        ]
      >>= EmptyResp.check

  let setTags photo_id tags o =
    Job.create & fun () ->
      json_api o `GET "flickr.photos.setTags"
        [ api_key
        ; "photo_id", photo_id
        ; "tags", String.concat " " tags
        ]
      >>= EmptyResp.check

  let delete photo_id o =
    Job.create & fun () ->
      json_api o `POST "flickr.photos.delete"
        [ api_key
        ; "photo_id", photo_id
        ]
      >>= EmptyResp.check

(*
{ "photos": { "page": 1, "pages": 34, "perpage": 100, "total": "3343",
              "photo": [ { "id": "15379632298", "owner": "9582328@N04",
                           "secret": "c90f3ac8c1", "server": "3939",
                           "farm": 4, "title": "test", "ispublic": 0,
                           "isfriend": 0, "isfamily": 0 },
                         { "id": "15505863036", "owner": "9582328@N04",
*)

  module Search = struct

    type resp = <
        photos : photos;
        stat   : string
      >

    and photos = <
        page    : sint;
        pages   : sint;
        perpage : sint;
        total   : sint;
        photo   : photo list
      >

    and photo = <
        id       : string;
        owner    : string;
        secret   : string;
        server   : string;
        farm     : sint;
        title    : string;
        ispublic : ibool;
        isfriend : ibool;
        isfamily : ibool
      >
    [@@deriving conv{ocaml; json}]
      
  end

  let search ?user_id ?tags ?text ?per_page ?page o =
    let tags, tag_mode = match tags with
      | None -> None, None
      | Some (`Any ts) -> Some (String.concat "," ts), Some "any"
      | Some (`All ts) -> Some (String.concat "," ts), Some "all"
    in
    Job.create & fun () ->
      json_api o `POST "flickr.photos.search"
      ( [ api_key ]
        @ List.filter_map id 
          [ opt id "user_id" user_id 
          ; opt id "tags" tags
          ; opt id "tag_mode" tag_mode
          ; opt id "text" text
          ; opt string_of_int "per_page" per_page (* <= 500 *)
          ; opt string_of_int "page" page
          ]
      )
      >>= lift_error Search.resp_of_json
      >>| fun x -> x#photos

end

module Photosets = struct

  module Create = struct

    type resp = < photoset: photoset; stat : string >

    and photoset = < id : string; url : string >
    [@@deriving conv{ocaml; json}]
  end

  let create ~title ~primary_photo_id o =
    Job.create & fun () ->
      json_api o `GET "flickr.photosets.create"
        [ api_key
        ; "title", title
        ; "primary_photo_id", primary_photo_id
        ]
      >>= lift_error Create.resp_of_json
      >>| fun x -> x#photoset
    
  module GetList = struct

    type set = < 
        id                     : string;
        primary                : string;
        secret                 : string;
        server                 : string;
        farm                   : sint;
        photos                 : Json.sint;
        videos                 : Json.sint;
        title                  : Content.t;
        description            : Content.t;
        needs_interstitial     : ibool;
        visibility_can_see_set : ibool;
        count_views            : sint;
        count_comments         : sint;
        can_comment            : ibool;
        date_create            : string;
        date_update            : string
      >
  
    and photoset = < 
        cancreate : ibool;
        page      : sint;
        pages     : sint;
        perpage   : sint;
        total     : sint;
        photoset  : set list 
      >
  
    and t = < 
        cancreate : bool;
        total : sint;
        photoset : set list 
      >

    and resp = < 
        photosets : photoset;
        stat      : string; 
      >
    [@@deriving conv{ocaml_of; json}]

  end

  let raw_getList ~per_page ~page o = 
    assert (page > 0);
    Job.create & fun () ->
      json_api o `GET "flickr.photosets.getList"
        [ api_key
        ; "page", string_of_int page
        ; "per_page", string_of_int per_page
        ]
      >>= lift_error GetList.resp_of_json
      >>| fun x -> x#photosets

  let getList ?(per_page=500) o =
    Page.to_seq (raw_getList o) ~per_page
      (fun psets -> object
        method cancreate = psets#cancreate
        method total = psets#total
      end)
      (fun psets -> psets#photoset)

  let getList' ?per_page o =
    getList ?per_page o |> Page.to_list
        
  module GetPhotos = struct

    type resp = < photoset : photoset; stat : string; >
        
    and photoset = <
      id        : string;
      primary   : string;
      owner     : string;
      ownername : string;
      photo     : photo list;
      page      : sint;
      per_page  : sint;
      perpage   : sint;
      pages     : sint;
      total     : sint;
      title     : string;
    >

    and photo = <
      id        : string;
      secret    : string;
      server    : string;
      farm      : sint;
      title     : string;
      isprimary : ibool;
      ispublic  : ibool;
      isfriend  : ibool;
      isfamily  : ibool 
    >
    [@@deriving conv{ocaml; json}]

  end


  let raw_getPhotos photoset_id ~per_page ~page o =
    assert (page > 0);
    Job.create & fun () ->
      json_api o `GET "flickr.photosets.getPhotos"
        [ api_key
        ; "photoset_id", photoset_id
        ; "page", string_of_int page
        ; "per_page", string_of_int per_page
        ]
      >>= lift_error GetPhotos.resp_of_json
      >>| fun x -> x#photoset

  (* CRv2 jfuruse: todo: Fancy lazy loading *)
  let getPhotos ?(per_page=500) photoset_id o =
    Page.to_seq (raw_getPhotos photoset_id o) ~per_page
      (fun pset -> object
        method id        = pset#id
        method primary   = pset#primary
        method owner     = pset#owner
        method ownername = pset#ownername
        method total     = pset#total
        method title     = pset#title
      end)
      (fun pset -> pset#photo)

  let getPhotos' ?per_page photoset_id o =
    getPhotos ?per_page photoset_id o |> Page.to_list

  let removePhotos photoset_id photo_ids o =
    Job.create & fun () ->
    json_api o `POST "flickr.photosets.removePhotos"
      [ api_key
      ; "photoset_id", photoset_id
      ; "photo_ids", String.concat "," photo_ids
      ]
    >>= EmptyResp.check


  let addPhoto photoset_id ~photo_id o =
    Job.create & fun () ->
    json_api o `GET "flickr.photosets.addPhoto"
      [ api_key
      ; "photoset_id", photoset_id
      ; "photo_id", photo_id
      ]
    >>= EmptyResp.check

end

module People = struct

  module GetUploadStatus = struct

    type bandwidth = <
        max            : int64;
        used           : int64;
        maxbytes       : int64;
        usedbytes      : int64;
        remainingbytes : int64;
        maxkb          : int64;
        usedkb         : int64;
        remainingkb    : int64;
        unlimited      : ibool
      >

    and filesize = <
        max      : int64;
        maxbytes : int64;
        maxkb    : int64;
        maxmb    : int64;
      >

    and videosize = <
        maxbytes : int64;
        maxkb    : int64;
        maxmb    : int64;
      >

    and sets = < 
        created   : sint;
        remaining : string
      >

    and videos = <
        uploaded  : sint;
        remaining : string
      >

    and resp = < 
      user : user;
      stat : string 
    >

    and user = < 
      id        : string;
      ispro     : ibool;
      username  : Content.t;
      bandwidth : bandwidth;
      filesize  : filesize;
      sets      : sets;
      videosize : videosize;
      videos    : videos;
    > [@@deriving conv{ocaml; json}]
  end

  let getUploadStatus o =
    Job.create & fun () ->
    json_api o `GET "flickr.people.getUploadStatus"
      [ api_key
      ]
    >>= lift_error GetUploadStatus.resp_of_json
    >>| fun x -> x#user

end

module Tags = struct
    (* flickr.tags.getListPhoto *)

  module GetListPhoto = struct

    type resp = <
        photo : photo;
        stat : string 
      >
        
    and photo = <
        id : string;
        tags : tags;
      >

    and tags = <
        tag : t;
      >

    and t = tag list

    and tag = < 
        id : string;
        author : string;
        authorname : string;
        raw : string;
        tag [@conv.as {json="_content"}] : string;
        machine_tag : ibool
      >
    [@@deriving conv{ocaml; json}]

  end

  let getListPhoto photo_id o =
    Job.create & fun () ->
    json_api o `GET "flickr.tags.getListPhoto"
      [ api_key
      ; "photo_id", photo_id
      ]
    >>= lift_error GetListPhoto.resp_of_json
    >>| fun x -> x#photo#tags#tag

end

module Test = struct

  module Login = struct
    type resp = < user : t; stat : string >
    and t = < id : string;
              username : Content.t >
    [@@deriving conv{ocaml; json}]
  end
  let login o =
    Job.create & fun () ->
    json_api o `GET "flickr.test.login"
      [ api_key
      ]
    >>= lift_error Login.resp_of_json
    >>| fun x -> x#user
end

module Upload = struct

  (* up.flickr.com does not support JSON response *)

  let raw_api fields img o = 
    Oauth.access o
      ~host: "up.flickr.com"
      ~path: "/services/upload"
      ~meth: (`POST_MULTIPART ["photo", `File img])
      ~oauth_other_params: fields
      
  let catch_with err f v = try `Ok (f v) with e -> `Error (err e)

  open Xml

  let tag p = function
    | Tag (name, _, _) as xml when p name -> [xml]
    | _ -> []

  let tag_named name = tag (fun x -> x = name)

  let pcdata = function
    | PCData _ as xml -> [xml]
    | _ -> []

  let children = function
    | Tag (_, _, xs) -> xs
    | PCData _ -> []
        
  let attr k = function
    | PCData _ -> []
    | Tag (_, attrs, _) -> 
        match List.assoc_opt k attrs with
        | None -> []
        | Some v -> [ PCData v ]
        
  let assoc_attr k = function
    | PCData _ -> None
    | Tag (_, attrs, _) -> List.assoc_opt k attrs
        
  let (^.) a b xml = List.concat_map a & b xml

  let parse_rsp xml =
    try
      match (attr "stat" ^. tag_named "rsp") xml with
      | [ PCData "ok" ] ->
          let photoids = (pcdata ^. children ^. tag_named "photoid" ^. children) xml in
          begin match photoids with
          | [] -> `Error ("<photoid> is not found", xml)
          | [PCData x] -> `Ok (`Ok x)
          | _ -> `Error ("malformed <photoid>(s)", xml)
          end
          
      | [ PCData "fail" ] ->
          begin match (tag_named "err" ^. children) xml with
          | [e] ->
              begin match assoc_attr "code" e, assoc_attr "msg" e with
              | Some code, Some msg -> 
                  begin try
                    `Ok (`Error (`API (object method stat = "fail" method code = int_of_string code method  message=msg end)))
                    with _ -> `Error ("error code is not int", xml)
                  end
                          
              | _ -> assert false
              end
          | [] -> assert false
          | _ -> assert false
          end
      | [ PCData s ] -> failwith s
      | [] -> assert false
      | _ -> assert false
    with
    | _ -> `Error ("rsp parse failed", xml)


  let upload 
      ?(is_public=false)
      ?(is_friend=false)
      ?(is_family=false)
      ?(hidden=true)
      ?title
      ?description
      ?tags (** Must not contain white space chars, but not check performed yet *) 
      img_file o =
        let title = Option.default title & fun () ->
          Filename.(basename *> split_extension *> fst) img_file
        in
        let bool k b = k, Some (if b then "1" else "0") in
  	let fields = 
          List.filter_map (fun (k,vopt) ->
            flip Option.map vopt & fun v -> k,v)
            [ bool "is_public" is_public
  	    ; bool "is_friend" is_friend
  	    ; bool "is_family" is_family
  	    ; "hidden", Some (if hidden then "2" else "1")
            ; "title", Some title
            ; "description", description
            ; "tags", Option.map (String.concat " ") tags
            ] 
  	in
  	raw_api fields img_file o >>= fun s -> 
        catch_with (fun exn -> `XML_parse (s, exn)) Xml.parse_string s >>= fun xml ->
        match parse_rsp xml with
        | `Ok v -> v
        | `Error (mes, xml) -> `Error (`XML_conv (mes, xml))

        
  let upload 
      ?is_public
      ?is_friend
      ?is_family
      ?hidden
      ?title
      ?description
      ?tags (** Must not contain white space chars, but not check performed yet *) 
      img_file o =
    Job.create & fun () ->
      upload 
        ?is_public
        ?is_friend
        ?is_family
        ?hidden
        ?title
        ?description
        ?tags
        img_file o
    
(*
safety_level (optional)
Set to 1 for Safe, 2 for Moderate, or 3 for Restricted.
content_type (optional)
Set to 1 for Photo, 2 for Screenshot, or 3 for Other.
*)
    
end

let format_error ppf =
  function
  | (`Http _ | `Curl _ as e) -> 
      Format.fprintf ppf "HTTP: %s@." & Http.string_of_error e;
  | `Json (e, s) -> 
      Format.fprintf ppf "Json: %a@." Json.format_error e;
      Format.fprintf ppf "  %S@." s;
  | `Json_conv e ->
      Format.fprintf ppf "Json_conv: %a@." Json_conv.format_full_error e;
  | `API fail ->
      Format.fprintf ppf "API: %a@." Fail.format fail;
  | `Load (name, exn) ->
      Format.fprintf ppf "Local file load failure: %s: %a@." name Exn.format exn;
  | `XML_parse (s, exn) ->
      Format.fprintf ppf "XML parse failure: %a : %s@." Exn.format exn s;
  | `XML_conv (mes, xml) ->
      Format.fprintf ppf "XML conv failure: %s : %s@." mes (Xml.show xml)

let error e = 
  format_error Format.stderr e;
  assert false
