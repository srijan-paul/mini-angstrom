type t = {
  offset : int;
  buffer : string;
  len : int;
}

let create s = { offset = 0; buffer = s; len = String.length s }

let read_char input =
  if input.offset < input.len then Ok input.buffer.[input.offset]
  else Error "Reached end of input"

let advance input = { input with offset = input.offset + 1 }

let to_string { buffer; offset; len } = 
  String.sub buffer offset (len - offset) 

let take n { buffer; offset; len } = 
  if offset + n >= len
    then Error "Reached end of input"
    else Ok (String.sub buffer offset n)

let advance_by n input = 
  if input.offset + n < input.len 
    then { input with offset = input.offset + n }
    else input

