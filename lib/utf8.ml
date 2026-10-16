let is_continuation c = Char.code c land 0xC0 = 0x80

let length s =
  let n = ref 0 in
  String.iter (fun c -> if not (is_continuation c) then incr n) s;
  !n

let truncate_bytes s n =
  if String.length s <= n then s
  else
    let rec back i = if i > 0 && is_continuation s.[i] then back (i - 1) else i in
    String.sub s 0 (back (max 0 n))

let take s n =
  let len = String.length s in
  let rec go i count =
    if i >= len then s
    else if (not (is_continuation s.[i])) && count = n then String.sub s 0 i
    else go (i + 1) (if is_continuation s.[i] then count else count + 1)
  in
  if n <= 0 then "" else go 0 0

(* [String.get_utf_8_uchar] decodes the maximal invalid subpart as one [Uchar.rep]. *)
let sanitize s =
  if String.is_valid_utf_8 s then s
  else
    let n = String.length s in
    let b = Buffer.create (n + 16) in
    let rec go i =
      if i < n then (
        let d = String.get_utf_8_uchar s i in
        Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar d);
        go (i + Uchar.utf_decode_length d))
    in
    go 0;
    Buffer.contents b

let rec sanitize_json (v : Yojson.Safe.t) : Yojson.Safe.t =
  match v with
  | `String s -> `String (sanitize s)
  | `List l -> `List (List.map sanitize_json l)
  | `Assoc m -> `Assoc (List.map (fun (k, x) -> (sanitize k, sanitize_json x)) m)
  | other -> other
