let repo_template = "libindex://repo/{host}/{owner}/{name}"
let doc_template = "libindex://doc/{host}/{owner}/{name}/{path}"
let repo_prefix = "libindex://repo/"
let doc_prefix = "libindex://doc/"

let unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let encode s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if unreserved c then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let hex c =
  match c with
  | '0' .. '9' -> Some (Char.code c - 48)
  | 'a' .. 'f' -> Some (Char.code c - 87)
  | 'A' .. 'F' -> Some (Char.code c - 55)
  | _ -> None

let decode s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then Some (Buffer.contents b)
    else if s.[i] <> '%' then (
      Buffer.add_char b s.[i];
      go (i + 1))
    else if i + 2 >= n then None
    else
      match (hex s.[i + 1], hex s.[i + 2]) with
      | Some h, Some l ->
          Buffer.add_char b (Char.chr ((h * 16) + l));
          go (i + 3)
      | _ -> None
  in
  go 0

let base ~host ~owner ~name =
  match host with
  | "github" | "gitlab" ->
      Some (Printf.sprintf "%s/%s/%s" host (encode owner) (encode name))
  | "local" -> Some ("local/_/" ^ encode name)
  | _ -> None

let repo ~host ~owner ~name =
  Option.map (fun b -> repo_prefix ^ b) (base ~host ~owner ~name)

let doc ~host ~owner ~name path =
  Option.map (fun b -> doc_prefix ^ b ^ "/" ^ encode path) (base ~host ~owner ~name)

type target = Repo of string | Doc of string * string

let strip_prefix prefix s =
  if String.starts_with ~prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let key_of host owner name =
  let ( let* ) = Option.bind in
  let* owner = decode owner in
  let* name = decode name in
  if owner = "" || name = "" then None
  else
    match host with
    | "github" | "gitlab" ->
        Some (String.lowercase_ascii (host ^ ":" ^ owner ^ "/" ^ name))
    | "local" when owner = "_" -> Some ("local:" ^ String.lowercase_ascii name)
    | _ -> None

let parse uri =
  if String.exists (fun c -> c = '?' || c = '#') uri then None
  else
    match (strip_prefix repo_prefix uri, strip_prefix doc_prefix uri) with
    | Some rest, _ -> (
        match String.split_on_char '/' rest with
        | [ host; owner; name ] -> Option.map (fun k -> Repo k) (key_of host owner name)
        | _ -> None)
    | None, Some rest -> (
        match String.split_on_char '/' rest with
        | host :: owner :: name :: (_ :: _ as path) -> (
            match (key_of host owner name, decode (String.concat "/" path)) with
            | Some k, Some p when p <> "" -> Some (Doc (k, p))
            | _ -> None)
        | _ -> None)
    | None, None -> None
