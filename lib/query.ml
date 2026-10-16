let is_token_byte c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | c -> Char.code c >= 0x80

let tokens q =
  let n = String.length q in
  let rec skip i acc =
    if i >= n then List.rev acc
    else if is_token_byte q.[i] then take i i acc
    else skip (i + 1) acc
  and take start i acc =
    if i < n && is_token_byte q.[i] then take start (i + 1) acc
    else skip i (String.sub q start (i - start) :: acc)
  in
  skip 0 []

let quote t = "\"" ^ t ^ "\""
let and_query ts = String.concat " " (List.map quote ts)
let or_query ts = String.concat " OR " (List.map quote ts)
