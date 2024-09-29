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

let number =
  is_digit |> sat_char |> many |> lift num_of_chars >>= function
  | Some n -> pure n
  | None -> fail "not a number"

let rec bvalue input = (bint <|> bstring <|> blist <|> bdict) input
and bint = bracket (char 'i') number (char 'e') |> lift num
and bstring' = number >>= fun n -> char ':' *> take n
and bstring input = (bstring' |> lift str) input

and blist input =
  (bracket (char 'l') (many bvalue) (char 'e') |> lift lizt) input

and key_value input =
  (let* key = bstring' in
   let* value = bvalue in
   pure (key, value))
    input

and sorted = function
  | [] -> true
  | _ :: t as keys ->
      t
      |> List.to_seq
      |> Seq.zip (List.to_seq keys)
      |> Seq.for_all (fun (a, b) -> a <= b)

and bdict input =
  (bracket (char 'd')
     ( many key_value >>= fun pairs ->
       if pairs |> List.map fst |> sorted then pure pairs else fail "not sorted"
     )
     (char 'e')
  |> lift dict)
    input

let%test "bencoding int" =
  "i42e" |> Input.create |> bvalue |> Result.map fst = Ok (Num 42)

let%test "bencoding string" =
  "5:hello" |> Input.create |> bvalue |> Result.map fst = Ok (Str "hello")

let%test "bencoding homogeneous list" =
  "l4:spami42ee"
  |> Input.create
  |> bvalue
  |> Result.map fst
  = Ok (Lizt [ Str "spam"; Num 42 ])

let%test "bencoding dictionaries" =
  "d3:foo3:bar4:spami42ee"
  |> Input.create
  |> bvalue
  |> Result.map fst
  = Ok (Dict [ ("foo", Str "bar"); ("spam", Num 42) ])

(* This is failing because the error string is different.
   Although, it is as expected when run from [utop]. I suspect that this has
   something to do with the preprocessor. The error is as follows:

   File "examples/bencoder/dune", line 4, characters 1-15:
   4 |  (inline_tests)
        ^^^^^^^^^^^^^^
   Predicate not satisfied
   File "examples/bencoder/bencoder.ml", line 89, characters 0-208: bencoding unsorted keys is false.

   FAILED 1 / 5 tests *)
let%test "bencoding unsorted keys" =
  match
    "d4:spami42e3:foo3:bare" |> Input.create |> bvalue |> Result.map fst
  with
  | Error "not sorted" -> true
  | Error e -> print_endline e; false
  | _ -> false
