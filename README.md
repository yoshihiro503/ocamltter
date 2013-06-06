The OCAMLTTER twitter client
============================

License
-------

        MIT License: see the MIT-LICENSE file


Requirements
------------
	
* [OCaml](http://caml.inria.fr/) >= 4.00.1
* OCamlFind
* [Cryptokit](http://pauillac.inria.fr/~xleroy/software.html) >= 1.3
* OCurl(known also as ocaml-curl or curl for OCaml) >= 0.5.3
* Type_conv >= 1.0.0
* Meta_conv = 1.1.1
* Tiny_json_conv = 1.4.0
* Spotlib >= 2.2.0
* OMake

Installation
------------

* Install required libraries. OPAM is an easy and highly recommended way to install them semi-automatically.
* omake

How to use
----------

#### Run

           $ ./ocamltter

#### You can use following commands:

           - l();;                 list timeline
           - lc COUNT;;            list timeline (COUNT lines)
           - lu "NAME";;           list NAME's timeline
           - m();;                 list mentions (tweet containing @YOU)
           - u "TEXT";;            post a new message
           - re ID "TEXT";;        reply to ID
           - del ID;;              delete tweet of ID
           - rt ID;;               retweet ID
           - qt ID "TEXT";;        qt ID
           - follow "NAME";;       follow NAME
           - unfollow "NAME";;     unfollow NAME
           - fav ID;;              mark ID as favorites
           - report_spam "NAME"    report NAME as a spam user
           - s "WORD";;            search tweets by a WORD
           - setup ();;            (re)authorize ocamltter
           - let CMD = ...;;       define a your own command CMD
           - help;;                print the help
	   
#### You may modify the config.ml
* You can set your twitter id
* You can set your interesting keywords
* You can set ignored users

#### Quit

            #quit;;  or  ^D


Bugs report
-----------

* y.imai at ocaml.jp
* [@yoshihiro503](http://twitter.com/yoshihiro503)
	
