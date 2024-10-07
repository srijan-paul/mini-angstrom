type 'a t = Input.t -> ('a * Input.t, string) result

module Functor = struct
  let map (f : 'a -> 'b) (p : 'a t) : 'b t =
   fun input ->
    match p input with
    | Ok (out, rest) -> Ok (f out, rest)
    | Error _ as e -> e

  let ( <$> ) = map
end

module Monad = struct
  let pure (v : 'a) : 'a t = fun input -> Ok (v, input)

  let bind (p : 'a t) (f : 'a -> 'b t) input =
    match p input with
    | Ok (p_out, rest) -> (f p_out) rest
    | Error _ as err -> err

  let ( >>= ) = bind
  let ( let* ) = bind

  let ( <* ) p q = 
    p >>= fun pout ->
    q >>= Fun.const (pure pout)

  let ( *> ) p q = 
    p >>= fun _ ->
    q >>= fun qout -> 
    pure qout


  (*a -> b to a parser -> b parser*)
  let lift (f : 'a -> 'b) (p : 'a t) : 'b t =
    p >>= fun x ->
    pure (f x)

  let lift2 f p q =
    p >>= fun x ->
    q >>= fun y ->
    pure (f x y)
    
  let lift3 f p q r =
    p >>= fun x ->
    q >>= fun y ->
    r >>= fun z ->
    pure (f x y z)

  let void p = p >>= Fun.const (pure ())
end

(* 'a t -> 'b t -> 'c t *)

module MonadFail = struct
  let fail (msg : string) : 'a t = Fun.const (Error msg)
end

module Alternative = struct
  let or' (p : 'a t) (q : 'a t) : 'a t =
   fun input ->
    match (p input, q input) with
    | (Ok _ as p_out), _ -> p_out
    | _, (Ok _ as q_out) -> q_out
    | p_error, _ -> p_error

  let ( <|> ) = or'
end

include Monad
include MonadFail
include Alternative

let item input =
  match Input.read_char input with
  | Ok chr -> Ok (chr, Input.advance input)
  | Error _ as err -> err

let sat_char pred =
  item >>= fun c ->
  if pred c then pure c else fail "Predicate not satisfied"

let char c : char t = sat_char (fun x -> x = c)

let take n : string t =
  fun input ->
    match Input.take n input with
    | Ok s -> Ok (s, Input.advance_by n input)
    | Error _ as err -> err

let string s : string t =
  take (String.length s) >>= function
  | prefix when prefix = s -> pure s
  | _ -> fail (Printf.sprintf "Expected %s." s)

let rec many (p : 'a t) : 'a list t =
  let many' =
    p >>= fun x ->
    many p >>= fun xs -> pure (x :: xs)
  in
  or' many' (pure [])

let bracket p_open p_x p_close =
  p_open *> p_x <* p_close

(*delimiter separated list*)
(* 1 . 2 . 3 . 4*)
let sep_by p_delim p_item =
  lift2 (fun x xs -> x::xs )
    (p_item)
    (many (p_delim *> p_item))

let is_space = function
  | ' '  | '\t' -> true
  | _ -> false

let spaces = void (many (sat_char is_space))

let token p = spaces *> p <* spaces
