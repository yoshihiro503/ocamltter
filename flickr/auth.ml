open OCamltter_oauth

module Oauth = Oauth_ex.Make(Conf)

let load_auth auth_file =
  match Ocaml.load_with Oauth.Access_token.t_of_ocaml auth_file with
  | Ok [a] -> a
  | _ -> assert false

let get_acc_token auth_file =
  try load_auth auth_file with
  | _ -> 
      let _res, acc_token = Oauth.authorize_cli_interactive () in
      Ocaml.save_with Oauth.Access_token.ocaml_of_t ~perm:0o600 auth_file [acc_token];
      acc_token

let get_oauth auth_file =
  let acc_token = get_acc_token auth_file in
  Oauth.oauth Oauth.Conf.app acc_token


