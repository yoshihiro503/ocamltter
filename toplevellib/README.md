OCamltter
========================

How to start:

After installation,

```shell
$ ocamltter
```

How to read help:

```ocaml
# print_string help;;
```

How to configure
-----------------------

* Your OAuth information is saved at `$HOME/.ocamltter.toplevel.auth`.
* `ocamlinit.ml` file found in `ocamlfind query ocamltter` directory is executed at the initialization by default. You can copy it somewhere and edit it, then start `ocamltter <your_init_file>` to change the start-up behaviour.
* There are several configurable options ready in module `OConfig`. See the module at `ocamlfind query ocamltter` directory for details. You can modify them in your `ocamlinit.ml` file explained above.




