Required preparation
=========================

Prepare `$HOME/.ocamltter_auths`:

```
[ ("ocamlbot",
   { consumer= { key= "YOUR APP KEY";
                 secret= "YOUR APP SECRET" };
     users= []
   }
  )
]
```

What is APP KEY/SECRET and USER KEY/SECRET? Read Twitter API documentation!

If you want to write your own Twitter bot, start changing `"ocamlbot"` names in `bot.ml`.

How to run
=================

```
$ ./ocamlbot
```
