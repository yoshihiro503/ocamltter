(* application level library *)
open Spotlib.Spot
open OCamltter_oauth
open Oauth_ex

module Auth = struct

  type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t
  let ocaml_of_hashtbl = Ocaml_conv.ocaml_of_hashtbl
  let hashtbl_of_ocaml = Ocaml_conv.hashtbl_of_ocaml

  type t = (string, app) hashtbl 
  and app = { consumer : Consumer.t;
              users : (string, Access_token.t) hashtbl }
  [@@deriving conv{ocaml}]

  let dummy = 
    Hashtbl.of_list 17
      [ "dummy_app", 
        { consumer = Consumer.dummy;
          users = Hashtbl.of_list 17 [ "dummy_user", 
                                       { Access_token.token = "dummy_token";
                                         secret = "dummy_secret" } ]
        }
      ]

  let find_app (tbl : t) app = Hashtbl.find_opt tbl app

  let find_user (tbl : t) ~app ~user = 
    match Hashtbl.find_opt tbl app with
    | None -> `NoApp
    | Some { consumer; users } ->
        match Hashtbl.find_all users user with
        | [] -> `NoUser consumer
        | [atoken] -> `Found (Oauth_ex.oauth consumer atoken)
        | _ -> failwithf "User %s has more than one entry for app %s" user app

  let load path = match Ocaml.load_with_exn t_of_ocaml path with
    | [x] -> x
    | _ -> assert false

  let save path x = Ocaml.save_with ocaml_of_t ~perm:0o600 path [x]

  let add_user tbl ~app ~user atoken =
    match find_app tbl app with
    | None -> assert false
    | Some {users} ->
        Hashtbl.add users user atoken
end

module Single = struct
  let load auth_file =
    match Ocaml.load_with_exn Access_token.t_of_ocaml auth_file with
    | [a] -> a
    | _ -> assert false
  
  let get auth_file authorize_cli_interactive =
    try load auth_file with
    | _ -> 
        let _res, acc_token = authorize_cli_interactive () in
        Ocaml.save_with Access_token.ocaml_of_t ~perm:0o600 auth_file [acc_token];
        acc_token
end

