open Spotlib.Spot

module M = struct

  type ('a, 'error) t = unit -> ('a, ('error * ('a, 'error) t)) Result.t
    
  let return res = fun () -> `Ok res
  
  let rec bind at f = fun () ->
    match at () with
    | `Error (e, _) -> `Error (e, bind at f)
    | `Ok v -> f v ()
end

include Monad.Make2(M)

type ('a, 'error) t = ('a, 'error) M.t
    
let run t = t ()

let rec run_until_success t =
  match run t with
  | `Ok v -> v
  | `Error (_, t) -> run_until_success t

let run_with_retry t p max_retry =
  let rec run_with_retry t p n =
    match run t with
    | `Ok v -> `Ok v
    | `Error (e, t) when p e = `Fail ->
        `Error (`Failed_by_predicate e, t)
    | `Error (e, t) when n <= 0 ->
        `Error (`Retried_but_failed (e, max_retry), t)
    | `Error (_e, t) -> run_with_retry t p (n-1)
  in
  run_with_retry t p max_retry

