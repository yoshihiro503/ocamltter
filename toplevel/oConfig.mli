(** Change these values from toplevel or your ocamlinit.ml *)

val filter : (OCamltter_twitter.Api_intf.Tweet.t -> bool) ref
val watching_words : bytes list ref
val coffee_break : float ref
val talk : bool ref
