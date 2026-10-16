(* GitHub/GitLab remote URL → index key, port of refresh.py@b70f23d norm_remote
   (:269-273). Kept apart from {!Classify} so that {!Identity} can use it. *)

let lower = String.lowercase_ascii

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let n = String.length s and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

let host_part rest =
  List.find_map
    (fun (h, kind) ->
      let n = String.length h in
      if
        String.length rest > n
        && String.sub rest 0 n = h
        && (rest.[n] = ':' || rest.[n] = '/')
      then Some (kind, String.sub rest (n + 1) (String.length rest - n - 1))
      else None)
    [ ("github.com", "github:"); ("gitlab.com", "gitlab:") ]

(* The lazy group of [(.+?)(?:\.git)?/?$]: strip the longest optional suffix that leaves
   a non-empty path. *)
let path_group p =
  let cut k =
    if String.length p > k then Some (String.sub p 0 (String.length p - k)) else None
  in
  let candidates =
    (if ends_with ~suffix:".git/" p then [ cut 5 ] else [])
    @ (if ends_with ~suffix:".git" p then [ cut 4 ] else [])
    @ (if ends_with ~suffix:"/" p then [ cut 1 ] else [])
    @ [ (if p = "" then None else Some p) ]
  in
  List.find_map Fun.id candidates

let after_userinfo rest =
  match String.index_opt rest '@' with
  | Some i when i > 0 && not (String.contains (String.sub rest 0 i) '/') ->
      Some (String.sub rest (i + 1) (String.length rest - i - 1))
  | _ -> None

(* A query string or fragment is never part of a key (it may carry an access token). *)
let strip_query url =
  match (String.index_from_opt url 0 '?', String.index_from_opt url 0 '#') with
  | None, None -> url
  | a, b ->
      let cut =
        List.fold_left min (String.length url) (List.filter_map Fun.id [ a; b ])
      in
      String.sub url 0 cut

let norm_remote url =
  let url = strip_query url in
  let prefix =
    List.find_opt
      (fun prefix -> starts_with ~prefix url)
      [ "git@"; "ssh://git@"; "https://" ]
  in
  match prefix with
  | None -> None
  | Some prefix -> (
      let rest =
        String.sub url (String.length prefix) (String.length url - String.length prefix)
      in
      let try_host r =
        Option.bind (host_part r) (fun (kind, p) ->
            Option.map (fun g -> kind ^ lower g) (path_group p))
      in
      match Option.bind (after_userinfo rest) try_host with
      | Some k -> Some k
      | None -> try_host rest)
