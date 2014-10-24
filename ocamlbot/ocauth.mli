(** OCamllter application level auth library *)

open OCamltter_oauth

(* application level library *)

module Auth : sig
  type t = (string, app) Hashtbl.t
  and app = { consumer : Oauth_ex.Consumer.t;
              users : (string, Oauth_ex.Access_token.t) Hashtbl.t }
  with conv(ocaml)

  val dummy : t

  val find_app : t -> string -> app option
  val find_user 
    : t 
    -> app:string 
    -> user:string 
    -> [> `Found of Oauth.t
       | `NoApp
       | `NoUser of Oauth_ex.Consumer.t 
       ]

  val load : string -> t
  val save : string -> t -> unit

  val add_user : t -> app:string -> user:string -> Oauth_ex.Access_token.t -> unit
end 

module Single : sig
  val get 
    : string 
    -> (unit -> 'a * Oauth_ex.Access_token.t) 
    -> Oauth_ex.Access_token.t
end
