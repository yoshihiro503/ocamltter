open Spotlib.Spot

module M = struct
  type ('a, 'error) t = Job of (unit -> ('a, ('error * ('a, 'error) t)) Result.t)

  let job j = Job j
            
  let run (Job j) = j ()
                  
  let return a = job & fun () -> Ok a

  let rec bind t f = job & fun () ->
    match run t with
    | Error (e, t') -> Error (e, bind t' f)
    | Ok a -> run & f a
end

include M
include Monad.Make2(M)
  
type ('a, 'error) job = ('a, 'error) t

let empty = return ()
  
let rec create f = job & fun () -> match f () with
  | Ok _ as res -> res
  | Error e -> Error (e, create f)

let rec retry
          (p : 'st -> 'error -> ('st, 'error2) result)
          (st : 'st)
          (j : ('a, 'error) t) : ('a, 'error2) t =
  job & fun () ->
      match run j with
      | Ok _ as res -> res
      | Error (e, t') ->
         match p st e with
         | Error e -> (Error (e, t'))
         | Ok st' -> run & retry p st' t'

let fold f acc =
  let rec loop acc =
    match%m f acc with
    | `End res -> return res
    | `Continue acc -> loop acc
  in
  loop acc
                    

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
