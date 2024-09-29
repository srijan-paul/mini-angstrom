open Parzer
open Parzer.Parser

type t =
  | Str of string
  | Num of int
  | Lizt of t list
  | Dict of (string * t) list

let num n = Num n
let str s = Str s
let lizt l = Lizt l
let dict d = Dict d

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let string_of_chars chars = chars |> List.to_seq |> String.of_seq
let num_of_chars chars = chars |> string_of_chars |> int_of_string_opt

let sorted = function
  | [] -> true
  | _ :: t as keys ->
      t
      |> List.to_seq
      |> Seq.zip (List.to_seq keys)
      |> Seq.for_all (fun (a, b) -> a <= b)

(* TODO: -ve numbers *)
let number =
  is_digit |> sat_char |> many |> lift num_of_chars >>= function
  | Some n -> pure n
  | None -> fail "not a number"

let byte_string =
  let* n = number in
  char ':' *> take n

let integer = bracket (char 'i') number (char 'e')
let b_integer = lift num integer
let b_byte_string = lift str byte_string

(* TODO: Improve error messages *)
let rec b_value input =
  (b_integer <|> b_byte_string <|> b_list <|> b_dict) input

(* XXX: Parse [t list] instead of [Lizt]? *)
and b_list input =
  let p = bracket (char 'l') (many b_value) (char 'e') in
  lift lizt p input

(* Can we do something about [lift _ p input] pattern (looks ugly)? *)

(* XXX: Parse [(string * t) list] instead of [Dict]? *)
and b_dict input =
  let entry =
    let* key = byte_string in
    let* value = b_value in
    pure (key, value)
  in
  let p = bracket (char 'd') (many entry) (char 'e') in
  lift dict p input

(* TODO: Improve tests *)
let%test "bencoding int" =
  "i42e" |> Input.create |> b_value |> Result.map fst = Ok (Num 42)

let%test "bencoding string" =
  "5:hello" |> Input.create |> b_value |> Result.map fst = Ok (Str "hello")

let%test "bencoding homogeneous list" =
  "l4:spami42ee"
  |> Input.create
  |> b_value
  |> Result.map fst
  = Ok (Lizt [ Str "spam"; Num 42 ])

let%test "bencoding dictionaries" =
  "d3:foo3:bar4:spami42ee"
  |> Input.create
  |> b_value
  |> Result.map fst
  = Ok (Dict [ ("foo", Str "bar"); ("spam", Num 42) ])
