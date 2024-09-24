open Parzer
open Parzer.Parser

module P = struct
  let is_digit = function
    | '0'..'9' -> true
    | _ -> false

  let is_htmlchar = function
    | 'a'..'z'
    | 'A'..'Z'
    | '0'..'9'
    | ' '
    | '\t'
    | '\n' -> true
    | _ -> false
end



let otag name =
  bracket 
    (char '<')
    (string name)
    (char '>')

let ctag name =
  bracket 
    (char '<' *> char '/')
    (string name)
    (char '>')

type _html_tag = {
  name: string;
  content: string;
}

let ( << ) f g x = f (g x)

let html_content = 
  Functor.map 
  (String.of_seq << List.to_seq) 
  (many (sat_char P.is_htmlchar))

let tag name =
  lift (fun content -> { name = name; content = content })
    (bracket 
      (otag name)
      html_content
      (ctag name))

let _paragraph = tag "p"

let to_num chars = chars 
  |> List.to_seq
  |> String.of_seq
  |> int_of_string

let number = 
  lift to_num
    (many (sat_char P.is_digit))

let num_array = 
    bracket 
      (token  (char '['))
      (sep_by (char ',') (token number))
      (token  (char ']'))

let () =
  let input = Input.create " [ 1 ,2 ,3, 41 ,5]" in
  match num_array input with
  | Ok (out, rest) ->
      print_string "parse result: ";
      List.iter (Printf.printf "%d ") out;
      print_string "\nremaining input:";
      print_string (Input.to_string rest)
  | Error msg -> failwith msg
