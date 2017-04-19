open Spotlib.Spot

module M = struct
  type ('a, 'error) t = Job of (unit -> ('a, ('error * ('a, 'error) t)) Result.t)

  let run (Job x) = x ()
                  
  let return a = Job (fun () -> Ok a)

  let rec bind j f = Job (fun () ->
    match run j with
    | Error (e, j') -> Error (e, bind j' f)
    | Ok a -> run & f a)
end

include M
include Monad.Make2(M)
  
type ('a, 'error) job = ('a, 'error) t

let empty = return ()
  
let rec create f = Job (fun () -> match f () with
  | Ok _ as res -> res
  | Error e -> Error (e, create f))

let rec retry
          (p : 'st -> 'error -> ('st, 'error2) result)
          (st : 'st)
          (j : ('a, 'error) t) : ('a, 'error2) t =
  Job (fun () ->
      match run j with
      | Ok _ as res -> res
      | Error (e, j') ->
         match p st e with
         | Error e -> (Error (e, j'))
         | Ok st' ->
            run & retry p st' j')

let run (Job t) = t ()

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

    
