type 'a llist = Nil | Cons of 'a * 'a llist Lazy.t

val hd : 'a llist -> 'a
val tl : 'a llist -> 'a llist
val take : int -> 'a llist -> 'a list
val map : ('a -> 'b) -> 'a llist -> 'b llist

val from : int -> int llist

val of_stream : 'a Stream.t -> 'a llist
val sllist : ?items:int -> string -> ('a -> string) -> 'a llist -> string
val of_string : string -> char llist
