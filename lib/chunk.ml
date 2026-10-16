type t = { title : string; text : string }

let max_chars = 2000
let is_blank s = String.trim s = ""

(* ------------------------------------------------------------ length cap *)

let drop s n =
  let p = Utf8.take s n in
  String.sub s (String.length p) (String.length s - String.length p)

let rec hard_split s =
  if Utf8.length s <= max_chars then [ s ]
  else Utf8.take s max_chars :: hard_split (drop s max_chars)

let paragraphs text =
  let flush cur acc =
    if cur = [] then acc else String.concat "\n" (List.rev cur) :: acc
  in
  let rec go cur acc = function
    | [] -> List.rev (flush cur acc)
    | l :: rest when is_blank l -> go [] (flush cur acc) rest
    | l :: rest -> go (l :: cur) acc rest
  in
  go [] [] (String.split_on_char '\n' text)

(* Greedy packing of paragraphs into parts of at most [max_chars] code points. *)
let pack parts =
  let rec go cur acc = function
    | [] -> List.rev (match cur with None -> acc | Some c -> c :: acc)
    | p :: rest -> (
        match cur with
        | None -> go (Some p) acc rest
        | Some c ->
            let joined = c ^ "\n\n" ^ p in
            if Utf8.length joined <= max_chars then go (Some joined) acc rest
            else go (Some p) (c :: acc) rest)
  in
  go None [] (List.concat_map hard_split parts)

let split_long ~title text =
  let text = String.trim text in
  if text = "" then []
  else if Utf8.length text <= max_chars then [ { title; text } ]
  else
    List.mapi
      (fun i text ->
        {
          title = (if i = 0 then title else Printf.sprintf "%s (%d)" title (i + 1));
          text;
        })
      (pack (paragraphs text))

let plain ~title text = split_long ~title text

(* ------------------------------------------------------------ markdown *)

let lines_of text =
  List.map
    (fun l ->
      let n = String.length l in
      if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
    (String.split_on_char '\n' text)

(* Leading spaces when at most 3 (CommonMark block indentation), else [None]. *)
let indent l =
  let n = String.length l in
  let rec go i = if i < n && l.[i] = ' ' then go (i + 1) else i in
  let i = go 0 in
  if i <= 3 then Some i else None

let run_end l i c =
  let n = String.length l in
  let rec go j = if j < n && l.[j] = c then go (j + 1) else j in
  go i

(* [Some (char, length, rest)] when the line is a code fence (3+ backticks or tildes). *)
let fence l =
  match indent l with
  | Some i when i < String.length l && (l.[i] = '`' || l.[i] = '~') ->
      let j = run_end l i l.[i] in
      if j - i >= 3 then Some (l.[i], j - i, String.sub l j (String.length l - j))
      else None
  | _ -> None

let strip_closing t =
  let n = String.length t in
  let rec go k = if k > 0 && t.[k - 1] = '#' then go (k - 1) else k in
  let k = go n in
  if k = n then t
  else if k = 0 then ""
  else if t.[k - 1] = ' ' || t.[k - 1] = '\t' then String.trim (String.sub t 0 k)
  else t

(* [Some (level, text)] for an ATX heading line. *)
let heading l =
  match indent l with
  | None -> None
  | Some i ->
      let n = String.length l in
      let j = run_end l i '#' in
      let level = j - i in
      if level < 1 || level > 6 || (j < n && l.[j] <> ' ' && l.[j] <> '\t') then None
      else Some (level, strip_closing (String.trim (String.sub l j (n - j))))

let closes (c, len) l =
  match fence l with
  | Some (c', len', rest) -> c' = c && len' >= len && is_blank rest
  | None -> false

(* Sections in order: (heading path, lines). A section starts with its heading line. *)
let sections text =
  let step (open_fence, path, cur, acc) l =
    match open_fence with
    | Some f -> ((if closes f l then None else open_fence), path, l :: cur, acc)
    | None -> (
        match (fence l, heading l) with
        | Some (c, len, _), _ -> (Some (c, len), path, l :: cur, acc)
        | None, Some (lvl, t) when lvl <= 3 ->
            let path' = List.filter (fun (lv, _) -> lv < lvl) path @ [ (lvl, t) ] in
            (None, path', [ l ], (path, List.rev cur) :: acc)
        | _ -> (None, path, l :: cur, acc))
  in
  let _, path, cur, acc = List.fold_left step (None, [], [], []) (lines_of text) in
  List.rev ((path, List.rev cur) :: acc)

let markdown_title text =
  List.find_map
    (fun (path, _) ->
      match List.rev path with (1, t) :: _ when t <> "" -> Some t | _ -> None)
    (sections text)

let path_title path =
  String.concat " > " (List.filter (fun t -> t <> "") (List.map snd path))

let markdown ~default_title text =
  List.concat_map
    (fun (path, lines) ->
      let body = match (path, lines) with [], l -> l | _, _ :: l -> l | _, [] -> [] in
      if List.for_all is_blank body then []
      else
        let title = match path_title path with "" -> default_title | t -> t in
        split_long ~title (String.concat "\n" lines))
    (sections text)

(* ------------------------------------------------------------ OCaml interfaces *)

type seg =
  | Doc of int * int  (** Start (inclusive) and stop (exclusive) of a doc comment. *)
  | Item of int

let is_ident c =
  match c with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true | _ -> false

let item_keywords =
  [ "val"; "type"; "module"; "exception"; "class"; "external"; "include"; "open" ]

(* Lexical scan: doc comments and column-0 item keywords outside comments, strings and
   nested structure blocks. *)
let segments s =
  let n = String.length s in
  let at i c = i < n && s.[i] = c in
  let rec skip_string i =
    if i >= n then n
    else if s.[i] = '\\' then skip_string (i + 2)
    else if s.[i] = '"' then i + 1
    else skip_string (i + 1)
  in
  let rec skip_comment i lvl =
    if i >= n then n
    else if at i '(' && at (i + 1) '*' then skip_comment (i + 2) (lvl + 1)
    else if at i '*' && at (i + 1) ')' then
      if lvl = 0 then i + 2 else skip_comment (i + 2) (lvl - 1)
    else if s.[i] = '"' then skip_comment (skip_string (i + 1)) lvl
    else skip_comment (i + 1) lvl
  in
  let skip_quoted i =
    let j = ref (i + 1) in
    while !j < n && match s.[!j] with 'a' .. 'z' | '_' -> true | _ -> false do
      incr j
    done;
    if not (at !j '|') then i + 1
    else
      let close = "|" ^ String.sub s (i + 1) (!j - i - 1) ^ "}" in
      let m = String.length close in
      let rec find k =
        if k + m > n then n else if String.sub s k m = close then k + m else find (k + 1)
      in
      find (!j + 1)
  in
  let skip_char i =
    if at (i + 1) '\\' then
      match String.index_from_opt s (i + 2) '\'' with Some k -> k + 1 | None -> n
    else if i + 2 < n && s.[i + 2] = '\'' then i + 3
    else i + 1
  in
  let depth = ref 0 and acc = ref [] in
  let word i j =
    let w = String.sub s i (j - i) in
    match w with
    | "sig" | "struct" | "object" | "begin" -> incr depth
    | "end" -> if !depth > 0 then decr depth
    | _ when !depth = 0 && (i = 0 || s.[i - 1] = '\n') && List.mem w item_keywords ->
        acc := Item i :: !acc
    | _ -> ()
  in
  let rec go i =
    if i < n then
      match s.[i] with
      | '(' when at (i + 1) '*' ->
          let stop = skip_comment (i + 2) 0 in
          if at (i + 2) '*' && (not (at (i + 3) ')')) && !depth = 0 then
            acc := Doc (i, stop) :: !acc;
          go stop
      | '"' -> go (skip_string (i + 1))
      | '{' -> go (skip_quoted i)
      | '\'' -> go (skip_char i)
      | c when is_ident c ->
          let j = ref i in
          while !j < n && is_ident s.[!j] do
            incr j
          done;
          word i !j;
          go !j
      | _ -> go (i + 1)
  in
  go 0;
  List.rev !acc

let title_patterns =
  List.map
    (fun (kind, re) -> (kind, Re.compile (Re.Perl.re re)))
    [
      ("module type", {|^module\s+type\s+([A-Za-z_][A-Za-z0-9_']*)|});
      ("module", {|^module\s+(?:rec\s+)?([A-Za-z_][A-Za-z0-9_']*)|});
      ( "class type",
        {|^class\s+type\s+(?:virtual\s+)?(?:\[[^\]]*\]\s*)?([a-z_][A-Za-z0-9_']*)|} );
      ("class", {|^class\s+(?:virtual\s+)?(?:\[[^\]]*\]\s*)?([a-z_][A-Za-z0-9_']*)|});
      ( "type",
        {|^type\s+(?:nonrec\s+)?(?:(?:[+-]?'[A-Za-z0-9_]+|_)\s+|\([^)]*\)\s*)?([a-z_][A-Za-z0-9_']*)|}
      );
      ("val", {|^val\s+(\([^)]*\)|[a-z_][A-Za-z0-9_']*)|});
      ("external", {|^external\s+(\([^)]*\)|[a-z_][A-Za-z0-9_']*)|});
      ("exception", {|^exception\s+([A-Z][A-Za-z0-9_']*)|});
      ("include", {|^include\s+(\S+)|});
    ]

let spaces = Re.compile (Re.Perl.re {|\s+|})

let item_title text =
  match
    List.find_map
      (fun (kind, re) ->
        Option.map
          (fun g -> kind ^ " " ^ Re.replace_string spaces ~by:" " (Re.Group.get g 1))
          (Re.exec_opt re text))
      title_patterns
  with
  | Some t -> t
  | None -> (
      match String.index_opt text ' ' with Some i -> String.sub text 0 i | None -> text)

(* Newlines in [s.[a..b)] when it is all whitespace; [None] when it holds anything else. *)
let ws_newlines s a b =
  let rec go i nl =
    if i >= b then Some nl
    else
      match s.[i] with
      | '\n' -> go (i + 1) (nl + 1)
      | ' ' | '\t' | '\r' -> go (i + 1) nl
      | _ -> None
  in
  go a 0

(* Trailing newlines of the item text [s.[a..b)]. *)
let trailing_newlines s a b =
  let rec last i =
    if i > a && String.contains " \t\r\n" s.[i - 1] then last (i - 1) else i
  in
  Option.value (ws_newlines s (last b) b) ~default:0

type item = { text : string; before : string list; mutable after : string list }

let seg_start = function Doc (a, _) -> a | Item a -> a

(* The module doc comments and the top-level items of an interface, each item with the
   doc comments attached to it (before, then after). *)
let mli_parts text =
  let s = String.concat "\n" (lines_of text) in
  let segs = Array.of_list (segments s) in
  let n = Array.length segs in
  let next_start k = if k + 1 < n then seg_start segs.(k + 1) else String.length s in
  let items = ref []
  and module_doc = ref []
  and pending = ref []
  and last_after = ref false in
  Array.iteri
    (fun k seg ->
      match seg with
      | Item a ->
          let it =
            {
              text = String.trim (String.sub s a (next_start k - a));
              before = List.rev !pending;
              after = [];
            }
          in
          pending := [];
          items := it :: !items;
          last_after := true
      | Doc (a, b) ->
          let doc = String.sub s a (b - a) in
          let gap_after =
            match ws_newlines s b (next_start k) with Some nl -> nl | None -> 2
          in
          let gap_before =
            if k = 0 then 2
            else
              match segs.(k - 1) with
              | Item p -> trailing_newlines s p a
              | Doc (_, pb) -> Option.value (ws_newlines s pb a) ~default:2
          in
          (match !items with
          | it :: _ when !last_after && gap_before < 2 -> it.after <- it.after @ [ doc ]
          | [] when !module_doc = [] && !pending = [] && (gap_after >= 2 || k + 1 >= n) ->
              module_doc := [ doc ]
          | _ -> pending := doc :: !pending);
          last_after := !last_after && gap_before < 2)
    segs;
  let items = List.rev !items in
  (match (List.rev !pending, List.rev items) with
  | [], _ | _, [] -> ()
  | docs, it :: _ -> it.after <- it.after @ docs);
  let is_open it =
    String.starts_with ~prefix:"open" it.text
    && (String.length it.text = 4 || not (is_ident it.text.[4]))
  in
  (!module_doc, List.filter (fun it -> not (is_open it)) items)

let mli ~module_name text =
  let module_doc, items = mli_parts text in
  let module_chunks =
    match module_doc with
    | [] -> []
    | docs -> split_long ~title:(module_name ^ " (module)") (String.concat "\n" docs)
  in
  module_chunks
  @ List.concat_map
      (fun it ->
        split_long ~title:(item_title it.text)
          (String.concat "\n" (it.before @ [ it.text ] @ it.after)))
      items

type mli_item = { title : string; signature : string; docs : string list }

let mli_items text =
  let module_doc, items = mli_parts text in
  ( module_doc,
    List.map
      (fun it ->
        { title = item_title it.text; signature = it.text; docs = it.before @ it.after })
      items )
