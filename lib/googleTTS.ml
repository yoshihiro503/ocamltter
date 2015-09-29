open Util

let play_command = "mpg123"

type lang = En | Ja
let slang = function
  | En -> "en"
  | Ja -> "ja"

let cmd s =
(*  print_endline s;*)
  ignore @@ Sys.command s

let agent = "Mozilla/5.0 (X11; U; Linux i686; ja; rv:1.9.2.11) Gecko/20101013 Ubuntu/9.04 (jaunty) Firefox/3.6.11"

let _wget outfile url =
  cmd (!%"wget -O %s -U '%s' '%s' >> log 2>> log" outfile agent url)
    

let play file =
  cmd (!% "%s %s >> log 2>> log" play_command file)

let say lang s =
  let open Lwt in
  let stream_to_file dst s =
    let open Lwt_stream in
    let oc = open_out_bin dst in
    let rec f () =
      get s >>= function
        | None -> return `Ok
        | Some s -> output_string oc s; f ()
    in
    catch f (fun _e -> return `Error) >>= fun res -> close_out oc; return res
  in
  let download =
    let open Cohttp in
    let open Cohttp_lwt_unix in
    let headers = Header.of_list [("User-Agent", agent)] in
    let uri = Uri.make ~scheme:"http" ~host:"translate.google.com" ~path:"/translate_tts" ~query:[("tl", [slang lang]); ("q",[s])] () in
    Client.get ~headers uri >>= fun (resp, body) ->
    let open Response in
    match Code.code_of_status resp.status with
    | 200 ->
        prerr_endline "ok";
        stream_to_file "say.mp3" (Cohttp_lwt_body.to_stream body)
    | code ->
        Format.eprintf "error code=%d@." code;
        return `Error (* we ignore the error boldly *)
  in
  Lwt_main.run (download >>= fun res ->
    begin match res with
    | `Ok -> play "say.mp3"
    | `Error -> ()
    end;
    return ())

let tr = [
  ("ttlweb","TTLウェブ");
  ("cho_tekitou","超適当");
  ("mzp","ミズピー");
  ("erutuf13","エルツフ");
  ("aua2008","アウア");
  ("shimomura1004","下村");
  ("keigoi","ケイゴイ");
  ("twiSearchLog","ツイサーチログ");
  ("bleis","");
  ("kmizu","ケイミズ");
  ("wof_moriguchi","WOF森口");
  ("F#","Fシャープ");
  ("yoshihiro503","ヨシヒロ503");
  ("soutaro","ソウタロウ");
  ("shelarcy","シェラーシィ");
  ("MoCo7","モコ7");
  ("tmiya_","Tミヤ");
  ("igeta","イゲタ");
  ("sunflat","サンフラット");
  ("bonotake","ボノタケ");
  ("garriguejej","ガリグ");
  ("50storms","50ストームズ");
  ("ksknac","KSKナック");
  ("wtakuo","Wタクオ");
  ("kencoba","ケンコバ");
  ("fukaminmin","フカミンミン");
  ("dico_leque","ディコレキ");
  ("yoriyuki","ヨリユキ");
  ("kikx","キックス");
  ("athos0220","アチョス");
  ("nixie_san","にくしーさん");
  ("mayahjp","まやJP");
  ("ksuenaga","K末永");
  ("camlspotter","キャムルスポッター");
  ("Dominion525","ドミニオン525");
  ("osiire","押入れ");
  ("Gemmat","ゲンマッと");
  ("esumii","Eすみい");
  ("suer","すえぁ");
  ("masahiro_sakai","まさひろさかい");
  ("keita44_f4", "ケイタヨンヨン");
  ("gabu", "がぶ");
  ("kazu_yamamoto","カズヤマモト");
  ("chiguri", "チグリ");
  ("camloeba", "俺は天才キャミバ様");
  ("hamatz", "ハマッツ");
  ("pirapira", "ピラピラ");
  (*===== もの ====*)
  ("Android","アンドロイド");
  ("ocaml","おきゃむる");
  ("OCaml","おきゃむる");
  ("Scala", "スカラ");
  ("deploy","デプロイ");
  ("Coq","コック");
  ("Gallina","ガリナ");
  ("haskell","ハスケル");
  ("Haskell","ハスケル");
  ("関数型", "カンスウガタ");
  ("Alloy","アロイ");
  ("javascript","ジャバスクリプト");
  ("Javascript","ジャバスクリプト");
  ("java", "ジャバ");
  ("Java", "ジャバ");
  ("RedBull","レッドブル");
  ("Mac", "マック");
  ("Book", "ブック");
  ("#", "");
]


let say_ja s =
  let s' =
    List.fold_left (fun s (a,b) ->
      Str.global_replace (Str.regexp_string a) b s) s tr
  in
  say Ja s'
