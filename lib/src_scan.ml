type syntax = C_like | Slash_like | Ml_like

type comment = {
  text : string;
  first : int;
  last : int;
  own_line : bool;
  line_comment : bool;
}

type t = { code : string array; comments : comment list }
type state = Code | Line_comment | Block of int | Str of char

let blank s = String.trim s = ""

(* The scanner's mutable state over one text. *)
type sc = {
  syntax : syntax;
  src : string;
  n : int;
  mutable lines : string list;  (** Code lines, reversed. *)
  mutable found : comment list;  (** Reversed. *)
  cbuf : Buffer.t;
  com : Buffer.t;
  mutable line : int;
  mutable com_first : int;
  mutable com_own : bool;
}

let start_comment sc opener =
  Buffer.clear sc.com;
  sc.com_first <- sc.line;
  sc.com_own <- blank (Buffer.contents sc.cbuf);
  Buffer.add_string sc.com opener

let end_comment sc ~line_comment =
  sc.found <-
    {
      text = Buffer.contents sc.com;
      first = sc.com_first;
      last = sc.line;
      own_line = sc.com_own;
      line_comment;
    }
    :: sc.found

let newline sc =
  sc.lines <- Buffer.contents sc.cbuf :: sc.lines;
  Buffer.clear sc.cbuf;
  sc.line <- sc.line + 1

let at sc i s =
  let k = String.length s in
  i + k <= sc.n && String.sub sc.src i k = s

(* Each step consumes from [i] in its state and returns the next position and state. *)
let line_comment_step sc i c =
  if c = '\n' then (
    end_comment sc ~line_comment:true;
    newline sc;
    (i + 1, Code))
  else (
    Buffer.add_char sc.com c;
    (i + 1, Line_comment))

let block_step sc i c d =
  let close = match sc.syntax with C_like | Slash_like -> "*/" | Ml_like -> "*)" in
  if at sc i close then (
    Buffer.add_string sc.com close;
    if d <= 1 then (
      end_comment sc ~line_comment:false;
      (i + 2, Code))
    else (i + 2, Block (d - 1)))
  else if sc.syntax = Ml_like && at sc i "(*" then (
    Buffer.add_string sc.com "(*";
    (i + 2, Block (d + 1)))
  else (
    Buffer.add_char sc.com c;
    if c = '\n' then newline sc;
    (i + 1, Block d))

let str_step sc i c q =
  if c = '\\' && sc.syntax <> Ml_like && i + 1 < sc.n then (
    if sc.src.[i + 1] = '\n' then newline sc;
    (i + 2, Str q))
  else if c = q then (
    Buffer.add_char sc.cbuf q;
    (i + 1, Code))
  else (
    if c = '\n' then newline sc;
    (i + 1, Str q))

let code_step sc i c =
  let slash = sc.syntax = C_like || sc.syntax = Slash_like in
  if slash && at sc i "//" then (
    start_comment sc "//";
    (i + 2, Line_comment))
  else if slash && at sc i "/*" then (
    start_comment sc "/*";
    (i + 2, Block 1))
  else if sc.syntax = Ml_like && at sc i "(*" then (
    start_comment sc "(*";
    (i + 2, Block 1))
  else if c = '"' || (c = '\'' && sc.syntax = C_like) then (
    Buffer.add_char sc.cbuf c;
    (i + 1, Str c))
  else if c = '\n' then (
    newline sc;
    (i + 1, Code))
  else (
    Buffer.add_char sc.cbuf c;
    (i + 1, Code))

let finish sc = function
  | Line_comment -> end_comment sc ~line_comment:true
  | Block _ -> end_comment sc ~line_comment:false
  | Code | Str _ -> ()

let scan syntax text =
  let text = String.concat "" (String.split_on_char '\r' text) in
  let sc =
    {
      syntax;
      src = text;
      n = String.length text;
      lines = [];
      found = [];
      cbuf = Buffer.create 80;
      com = Buffer.create 80;
      line = 0;
      com_first = 0;
      com_own = false;
    }
  in
  let rec go i st =
    if i >= sc.n then (
      finish sc st;
      newline sc)
    else
      let c = text.[i] in
      let i', st' =
        match st with
        | Line_comment -> line_comment_step sc i c
        | Block d -> block_step sc i c d
        | Str q -> str_step sc i c q
        | Code -> code_step sc i c
      in
      go i' st'
  in
  go 0 Code;
  { code = Array.of_list (List.rev sc.lines); comments = List.rev sc.found }

let is_doc_c raw =
  (String.starts_with ~prefix:"/**" raw && raw <> "/**/")
  || String.starts_with ~prefix:"///" raw
  || String.starts_with ~prefix:"//!" raw

let is_doc_ml raw = String.starts_with ~prefix:"(**" raw && raw <> "(**)"

let squash s =
  String.concat " "
    (List.filter
       (fun w -> w <> "")
       (String.split_on_char ' '
          (String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) s)))

let strip_markers raw =
  let drop_prefix s ps =
    match List.find_opt (fun p -> String.starts_with ~prefix:p s) ps with
    | Some p -> String.sub s (String.length p) (String.length s - String.length p)
    | None -> s
  in
  let drop_suffix s ps =
    match List.find_opt (fun p -> String.ends_with ~suffix:p s) ps with
    | Some p -> String.sub s 0 (String.length s - String.length p)
    | None -> s
  in
  let s = drop_prefix raw [ "/**"; "///"; "//!"; "//"; "/*"; "(**"; "(*" ] in
  let s = drop_suffix s [ "*/"; "*)" ] in
  String.split_on_char '\n' s
  |> List.map (fun l ->
      let t = String.trim l in
      String.trim (drop_prefix t [ "*" ]))
  |> String.concat "\n" |> String.trim

let doc_above ?(accept = fun _ -> true) t i =
  let ending j =
    List.find_opt (fun c -> c.last = j && c.own_line && accept c.text) t.comments
  in
  match ending (i - 1) with
  | None -> ""
  | Some c when not c.line_comment -> strip_markers c.text
  | Some c ->
      let rec up j acc =
        match ending j with
        | Some d when d.line_comment && d.first = j -> up (j - 1) (d :: acc)
        | _ -> acc
      in
      let run = up (c.first - 1) [ c ] in
      String.concat "\n" (List.map (fun d -> strip_markers d.text) run) |> String.trim
