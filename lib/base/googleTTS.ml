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
  let save fname ch =
    open_out_with fname (fun out ->
      let rec iter () =
        try output_char out (input_char ch); iter() with
        | End_of_file -> ()
      in
      iter ())
  in
  let () = Http.conn "translate.google.com" `GET
      ~headers:[("User-Agent", agent)]
      "/translate_tts"
    [("tl", slang lang); ("q",s)]
    (fun _ ch -> save "say.mp3" ch)
  in
(*  let url =
    !%"http://translate.google.com/translate_tts?tl=%s&q=%s" (slang lang) s
  in
  wget "say.mp3" url;*)
  play "say.mp3"

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
