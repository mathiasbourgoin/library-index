type stub = {
  host : string;
  owner : string;
  name : string;
  full_name : string;
  url : string option;
  private_ : bool option;
}

let normalize k = String.lowercase_ascii (String.trim k)

let forge_stub host path =
  match String.rindex_opt path '/' with
  | Some i when i > 0 && i < String.length path - 1 ->
      let owner = String.sub path 0 i in
      let name = String.sub path (i + 1) (String.length path - i - 1) in
      let base =
        if host = "github" then "https://github.com/" else "https://gitlab.com/"
      in
      Ok
        { host; owner; name; full_name = path; url = Some (base ^ path); private_ = None }
  | _ ->
      Error
        (Printf.sprintf "malformed curated key '%s:%s' (expected owner/name)" host path)

let stub_of_curated_key k =
  let k = normalize k in
  match String.index_opt k ':' with
  | None -> Error (Printf.sprintf "curated key '%s' has no host prefix" k)
  | Some i -> (
      let host = String.sub k 0 i in
      let path = String.sub k (i + 1) (String.length k - i - 1) in
      match host with
      | ("github" | "gitlab") when path <> "" -> forge_stub host path
      | "local" when path <> "" && not (String.contains path '/') ->
          Ok
            {
              host;
              owner = "(local)";
              name = path;
              full_name = path;
              url = None;
              private_ = Some true;
            }
      | "github" | "gitlab" | "local" ->
          Error (Printf.sprintf "malformed curated key '%s'" k)
      | _ ->
          Error
            (Printf.sprintf "curated key '%s' has unknown host prefix '%s' (allowed: %s)"
               k host
               (String.concat ", " Vocab.curated_hosts)))

let whitehat ~campaign ~path = "whitehat:" ^ String.lowercase_ascii (campaign ^ "/" ^ path)
