open Spotlib.Spot

module M = struct
  type ('a, 'error) t = unit -> ('a, ('error * ('a, 'error) t)) Poly_result.t

  let return a = fun () -> `Ok a

  let rec bind at f = fun () -> match at () with
    | `Ok v -> f v ()
    | `Error (e,at') -> `Error (e, bind at' f)
end
  
include M
include Monad.Make2(M)
  
type ('a, 'error) job = ('a, 'error) t

let empty = fun () -> `Ok ()
  
let rec create f = fun () -> match f () with
  | Ok res -> `Ok res
  | Error e -> `Error (e, create f)

let rec retry p st t = fun () -> match t () with
  | `Ok _ as res -> res
  | `Error (e, t') ->
      match p st e with
      | `Ok st' -> retry p st' t' ()
      | `Error e -> `Error (e, t')

let run t = t ()

module Seq = struct
  type ('a, 'error) t = ( [`None | `Some of 'a * ('a, 'error) t], 'error ) job

  let rec flatten : ('a list, 'error) t -> ('a, 'error) t = fun t ->
    t >>= function
      | `None -> return `None
      | `Some (xs, cont) ->
          let rec loop = function
            | [] -> flatten cont
            | x::xs -> return & `Some (x, loop xs)
          in
          loop xs

end

let rec of_seq : ('a, 'error) Seq.t -> ('a list, 'error) t = fun seq ->
  seq >>= function
    | `None -> return []
    | `Some (x, seq) ->
        of_seq seq >>= fun xs -> return & x::xs

    
