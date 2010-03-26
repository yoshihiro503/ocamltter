#!/bin/sh
ocamlmktop -I +threads -I ./base unix.cma str.cma threads.cma ./base/util.ml ./base/base64.ml ./base/utf16.ml ./base/llist.ml ./base/parserMonad.ml ./base/json.ml ./base/http.ml twitterApi.ml ocamltter.ml top.ml -o ocamltter
rm -f ./base/*.cm[io]