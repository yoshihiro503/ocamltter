open Spotlib.Spot
open OCamltter_oauth

module Oauth : module type of struct include Oauth_ex.Make(Conf) end

type 'json mc_leftovers = (string * 'json) list with conv(ocaml)

val load_auth     : string -> Oauth.Access_token.t
val get_acc_token : string -> Oauth.Access_token.t
val get_oauth     : string -> Oauth.t

module Json : sig
  include module type of struct include Tiny_json.Json end

  type error = 
    | NotObject of t
    | InvalidField of string
    | CastErr of string
    | UnknownErr of string
    | NotJsonErr of exn
    | NoJsonResponse
  with conv(ocaml_of)

  val parse : string -> [> `Error of [> `Json of error * string ] | `Ok of t ]
end

val raw_api :
  Oauth.t ->
  [< `GET | `POST ] ->
  string ->
  (string * string) list ->
  (string, [> Http.error ]) Result.t

module Fail : sig
  type t = < code : int;
             message : string; 
             stat : string >
  with conv(json, ocaml)
  val format : Format.formatter -> t -> unit
  val check : Json.t -> [> `Error of [> `API of t ] | `Ok of Json.t ]
end

val json_api :
  Oauth.t ->
  [< `GET | `POST ] ->
  string ->
  (string * string) list ->
  (Json.t,
   [> `API of Fail.t
    | `Curl of Curl.curlCode * int * string
    | `Http of int * string
    | `Json of Json.error * string ])
  Result.t

type content = < content : string > with conv(json, ocaml)

module EmptyResp : sig
  type resp = < stat : string > with conv(json, ocaml) 
  val check :
    Json.t ->
    (unit, [> `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t
end

module Auth : sig
  module Oauth : sig
    val checkToken :
      (* 'a -> *)
      Oauth.t ->
      (Json.t,
       [> `API of Fail.t
       | `Curl of Curl.curlCode * int * string
       | `Http of int * string
       | `Json of Json.error * string ])
        Result.t
  end
end

module TagList : sig
  type t = string list with conv(ocaml, json)
end

module Photos : sig

  module GetNotInSet : sig

    type photos =
        < page : int; 
          pages : int; 
          perpage : int; 
          photo : photo list; 
          total : int 
        >
    and photo = 
        < farm : int
        ; id : string
        ; isfamily : bool
        ; isfriend : bool
        ; ispublic : bool
        ; owner : string
        ; secret : string
        ; server : string
        ; tags : TagList.t option
        ; title : string 
        >
    with conv(json, ocaml)
  end

  val raw_getNotInSet :
      per_page:int ->
      page:int ->
      ?tags:bool ->
      Oauth.t ->
      (GetNotInSet.photos,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val getNotInSet :
      ?per_page:int ->
      ?tags:bool ->
      Oauth.t ->
      (< pages : int
       ; perpage : int
       ; stream : ([> `Error of
                      ([> `API of Fail.t
                       | `Curl of Curl.curlCode * int * string
                       | `Http of int * string
                       | `Json of Json.error * string
                       | `Json_conv of Json.t Meta_conv.Error.t ]
                          as 'b) *
                        (int -> 'a Stream.t) * int
                   | `Ok of GetNotInSet.photo ]
                      as 'a) Stream.t
       ; total : int 
       >,
       'b)
      Result.t

    module GetInfo : sig
        type owner =
          < location : string
          ; nsid : string
          ; realname : string
          ; unknowns : Json.t mc_leftovers
          ; username : string 
          >
        and dates =
          < posted : int64
          ; taken : string
          ; unknowns : Json.t mc_leftovers 
          >
        and urls = 
          < url : type_content list >
        and type_content = 
          < content : string
          ; type_ : string 
          >
        and photo =
          < comments : content
          ; dates : dates
          ; dateuploaded : int64
          ; description : content
          ; farm : int
          ; id : string
          ; isfavorite : bool
          ; license : int
          ; media : string
          ; originalformat : string
          ; originalsecret : string
          ; owner : owner
          ; rotation : int
          ; safety_level : int
          ; secret : string
          ; server : string
          ; title : content
          ; unknowns : Json.t mc_leftovers
          ; urls : urls
          ; views : int
          ; visibility : visibility 
          >
        and visibility =
          < isfamily : bool
          ; isfriend : bool
          ; ispublic : bool 
          >
        with conv(ocaml, json)
    end

    val getInfo :
      string ->
      Oauth.t ->
      (GetInfo.photo,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    module GetExif :  sig
      type exif =
        < clean : string option
        ; label : string
        ; raw : content
        ; tag : string
        ; tagspace : string
        ; tagspaceid : int >
      and photo =
        < camera : string
        ; exif : exif list
        ; id : string
        ; secret : string
        ; unknown : Json.t mc_leftovers >
      with conv(json, ocaml)
    end

    val getExif :
      string ->
      Oauth.t ->
      (GetExif.photo,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val addTags :
      string ->
      string list ->
      Oauth.t ->
      (unit,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val setTags :
      string ->
      string list ->
      Oauth.t ->
      (unit,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val delete :
      string ->
      Oauth.t ->
      (unit,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    module Search :
      sig
        type photos =
          < page : int
          ; pages : int
          ; perpage : int
          ; photo : photo list
          ; total : int >
        and photo =
          < farm : int
          ; id : string
          ; isfamily : bool
          ; isfriend : bool
          ; ispublic : bool
          ; owner : string
          ; secret : string
          ; server : string
          ; title : string 
          >
        with conv(json,ocaml)
      end

    val search :
      ?user_id:string ->
      ?tags:[< `All of string list | `Any of string list ] ->
      ?text:string ->
      ?per_page:int ->
      ?page:int ->
      Oauth.t ->
      (Search.photos,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t
  end

module Photosets :
  sig
    module Create : sig
      type photoset = 
        < id : string
        ; url : string 
        >
      with conv(json, ocaml)
    end

    val create :
      title:string ->
      primary_photo_id:string ->
      Oauth.t ->
      (Create.photoset,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    module GetList : sig
      type set =
        < can_comment : bool
        ; count_comments : int
        ; count_views : int
        ; date_create : string
        ; date_update : string
        ; description : content
        ; farm : int
        ; id : string
        ; needs_interstitial : bool
        ; photos : int
        ; primary : string
        ; secret : string
        ; server : string
        ; title : content
        ; videos : int
        ; visibility_can_see_set : bool 
        >
      and photoset =
        < cancreate : bool
        ; page : int
        ; pages : int
        ; perpage : int
        ; photoset : set list
        ; total : int 
        >
      and t = 
        < cancreate : bool
        ; photoset : set list
        ; total : int 
        >
      with conv(json,ocaml_of)
    end

    val raw_getList :
      ?page:int ->
      Oauth.t ->
      (GetList.photoset,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val getList :
      Oauth.t ->
      (< cancreate : bool
       ; photoset : GetList.set list
       ; total : int >,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    module GetPhotos : sig
      type photoset =
        < id : string
        ; owner : string
        ; ownername : string
        ; page : int
        ; pages : int
        ; per_page : int
        ; perpage : int
        ; photo : photo list
        ; primary : string
        ; title : string
        ; total : int 
        >
      and photo =
        < farm : int
        ; id : string
        ; isfamily : bool
        ; isfriend : bool
        ; isprimary : bool
        ; ispublic : bool
        ; secret : string
        ; server : string
        ; title : string >
      with conv(json,ocaml)
    end

    val raw_getPhotos :
      string ->
      ?page:int ->
      Oauth.t ->
      (GetPhotos.photoset,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val getPhotos :
      string ->
      Oauth.t ->
      (< id : string
       ; owner : string
       ; ownername : string
       ; photo : GetPhotos.photo list
       ; primary : string
       ; title : string
       ; total : int >,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val removePhotos :
      string ->
      string list ->
      Oauth.t ->
      (unit,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t

    val addPhoto :
      string ->
      photo_id:string ->
      Oauth.t ->
      (unit,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t
  end

module People :
  sig
    module GetUploadStatus :
      sig
        type bandwidth =
          < max : int64
          ; maxbytes : int64
          ; maxkb : int64
          ; remainingbytes : int64
          ; remainingkb : int64
          ; unlimited : bool
          ; used : int64
          ; usedbytes : int64
          ; usedkb : int64 >
        and filesize =
          < max : int64
          ; maxbytes : int64
          ; maxkb : int64
          ; maxmb : int64 
          >
        and videosize = 
          < maxbytes : int64
          ; maxkb : int64
          ; maxmb : int64 >
        and sets = 
          < created : int
          ; remaining : string >
        and videos = 
          < remaining : string
          ; uploaded : int >
        and user =
          < bandwidth : bandwidth
          ; filesize : filesize
          ; id : string
          ; ispro : bool
          ; sets : sets
          ; username : content
          ; videos : videos
          ; videosize : videosize 
          >
        with conv(json,ocaml)
      end

    val getUploadStatus :
      Oauth.t ->
      (GetUploadStatus.user,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t
  end

module Tags :
  sig
    module GetListPhoto :
      sig
        type photo = 
          < id : string
          ; tags : tags >
        and tags = < tag : t >
        and t = tag list
        and tag =
          < author : string
          ; authorname : string
          ; id : string
          ; machine_tag : bool
          ; raw : string
          ; tag : string >
        with conv(json,ocaml)
      end

    val getListPhoto :
      string ->
      Oauth.t ->
      (GetListPhoto.t,
       [> `API of Fail.t
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `Json of Json.error * string
        | `Json_conv of Json.t Meta_conv.Error.t ])
      Result.t
  end
module Test : sig

  module Login : sig
    type t = 
      < id : string
      ; username : content >
    with conv(json,ocaml)
  end

  val login :
    Oauth.t ->
    (Login.t,
     [> `API of Fail.t
      | `Curl of Curl.curlCode * int * string
      | `Http of int * string
      | `Json of Json.error * string
      | `Json_conv of Json.t Meta_conv.Error.t ])
    Result.t
end
    
module Upload : sig

  val raw_api :
    Http.params ->
    string ->
    Oauth.t ->
    (string, [> Http.error ]) Result.t

  val parse_rsp :
    Xml.xml -> 
    ( (string, 
       [> `API of < code : int
; message : string
; stat : string > ]) Result.t
    , string * Xml.xml) Result.t

  val upload :
      ?is_public:bool ->
      ?is_friend:bool ->
      ?is_family:bool ->
      ?hidden:bool ->
      ?title:string ->
      ?description:string ->
      ?tags:string list ->
      string ->
      Oauth.t ->
      (string,
       [> `API of < code : int
                  ; message : string
                  ; stat : string >
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `XML_conv of string * Xml.xml
        | `XML_parse of string * exn ])
      Result.t
  end

val format_error :
  Format.formatter ->
  [< `API of Fail.t
   | `Curl of Curl.curlCode * int * string
   | `Http of int * string
   | `Json of Json.error * string
   | `Json_conv of Json.t Meta_conv.Error.t
   | `Load of string * exn
   | `XML_conv of string * Xml.xml
   | `XML_parse of string * exn ] ->
  unit

val error :
  [< `API of Fail.t
   | `Curl of Curl.curlCode * int * string
   | `Http of int * string
   | `Json of Json.error * string
   | `Json_conv of Json.t Meta_conv.Error.t
   | `Load of string * exn
   | `XML_conv of string * Xml.xml
   | `XML_parse of string * exn ] ->
  'raise_exception
