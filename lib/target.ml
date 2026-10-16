type t = {
  target : string;
  key : string;
  host : string;
  full_name : string;
  local_dir : string option;
  checkout : Local_scan.checkout option;
}

let is_url s =
  List.exists
    (fun prefix -> String.starts_with ~prefix s)
    [ "https://"; "http://"; "git@"; "ssh://" ]

let split_key key =
  match String.index_opt key ':' with
  | Some i -> (String.sub key 0 i, String.sub key (i + 1) (String.length key - i - 1))
  | None -> ("", key)

(* [scheme://userinfo@...]: the userinfo of a [scheme://] URL (an access token or a
   password before the [@] of the authority). The scp-like [git@host:path] form carries
   no credential. *)
let has_userinfo s =
  match Re.exec_opt (Re.Perl.compile_pat {|^[A-Za-z][A-Za-z0-9+.-]*://[^/?#]*@|}) s with
  | Some _ -> true
  | None -> false

let host_url = function
  | "github" -> Some "https://github.com/"
  | "gitlab" -> Some "https://gitlab.com/"
  | _ -> None

(* A refused target as an error message may show it: without its query string, fragment
   or [user[:password]@] part (where a generic credential such as [?access_token=] or
   a [scheme://user:token@] prefix lives, which {!Redact.text} cannot recognise), then redacted. *)
let shown s =
  let cut c s =
    match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
  in
  let s = cut '#' (cut '?' s) in
  let s =
    match Re.exec_opt (Re.Perl.compile_pat "^([A-Za-z][A-Za-z0-9+.-]*://)[^/@]*@") s with
    | Some g ->
        Re.Group.get g 1
        ^ String.sub s (Re.Group.stop g 0) (String.length s - Re.Group.stop g 0)
    | None -> s
  in
  Redact.text s

let of_url s =
  let s' =
    if String.starts_with ~prefix:"http://" s then
      "https://" ^ String.sub s 7 (String.length s - 7)
    else s
  in
  let shown = shown s in
  match Remote.norm_remote (String.trim s') with
  | None -> Error (Printf.sprintf "'%s' is not a GitHub or GitLab repository URL" shown)
  | Some key -> (
      let host, full_name = split_key key in
      if not (String.contains full_name '/') then
        Error (Printf.sprintf "'%s' does not name a repository (owner/name)" shown)
      else
        (* A URL with userinfo, a query string or a fragment (each may carry a
           credential) is stored as the canonical URL of its key: no credential reaches
           sources.toml, the database or any output. *)
        let credential_bearing =
          has_userinfo s || String.contains s '?' || String.contains s '#'
        in
        match (credential_bearing, host_url host) with
        | true, Some base ->
            Ok
              {
                target = base ^ full_name;
                key;
                host;
                full_name;
                local_dir = None;
                checkout = None;
              }
        | true, None -> Error "a repository URL carrying credentials is refused"
        | false, _ ->
            Ok { target = s; key; host; full_name; local_dir = None; checkout = None })

let absolute ~home p =
  let p = match home with Some h -> Config.expand_home ~home:h p | None -> p in
  let p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  try Unix.realpath p with Unix.Unix_error _ -> p

let is_dir p = try Sys.is_directory p with Sys_error _ -> false

let of_dir ~id ~call ~home s =
  let dir = absolute ~home s in
  if not (is_dir dir) then
    Error
      (Printf.sprintf "'%s' is neither a GitHub/GitLab URL nor an existing directory"
         (shown s))
  else if not (Sys.file_exists (Filename.concat dir ".git")) then
    Error (Printf.sprintf "'%s' is not a git checkout" (shown s))
  else
    let c = Local_scan.checkout ~id ~call dir in
    match c.remote_key with
    | Some key ->
        let host, full_name = split_key key in
        Ok { target = dir; key; host; full_name; local_dir = Some dir; checkout = Some c }
    | None ->
        let name = Filename.basename dir in
        Ok
          {
            target = dir;
            key = "local:" ^ String.lowercase_ascii name;
            host = "local";
            full_name = name;
            local_dir = Some dir;
            checkout = Some c;
          }

let parse ~id ~call ~home s =
  let s = String.trim s in
  if s = "" then Error "missing target"
  else if is_url s then of_url s
  else of_dir ~id ~call ~home s

let lookup_key s =
  let s = String.trim s in
  if is_url s then Result.to_option (Result.map (fun t -> t.key) (of_url s))
  else
    match split_key (Key.normalize s) with
    | ("github" | "gitlab" | "local"), rest when rest <> "" -> Some (Key.normalize s)
    | _ -> None
