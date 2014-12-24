open Spotlib.Spot

type (+'a, 'error) t
    
include Monad_intf.T2 with type ('a,'error) t := ('a, 'error) t

val run : ('a, 'error) t -> ('a, 'error * ('a, 'error) t) Result.t
  
val run_until_success : ('a, 'error) t -> 'a
  
val run_with_retry :
  ('d, 'error) t  
  -> ('error -> [ `Fail | `Continue ])
  -> int
  -> ('d, [ `Failed_by_predicate of 'error
          | `Retried_but_failed  of 'error * int
          ]
          * ('d, 'error) t) Result.t
