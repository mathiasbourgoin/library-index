type purpose =
  | Listing
  | Search
  | Metadata
  | Readme
  | Root_listing
  | Manifest
  | Last_commit
  | Contributors
  | Languages
  | Parent
  | Local_git
  | Doc_tree
  | Doc_file

type t = { purpose : purpose; argv : string array }

let purpose_name = function
  | Listing -> "listing"
  | Search -> "search"
  | Metadata -> "metadata"
  | Readme -> "readme"
  | Root_listing -> "root-listing"
  | Manifest -> "manifest"
  | Last_commit -> "last-commit"
  | Contributors -> "contributors"
  | Languages -> "languages"
  | Parent -> "parent"
  | Local_git -> "local-git"
  | Doc_tree -> "docs-tree"
  | Doc_file -> "docs-file"

let raw_accept = "Accept: application/vnd.github.raw+json"

let gh ?(paginate = false) ?(raw = false) path =
  Array.of_list
    ([ "gh"; "api"; "-X"; "GET"; path ]
    @ (if paginate then [ "--paginate" ] else [])
    @ if raw then [ "-H"; raw_accept ] else [])

let glab ?(paginate = false) path =
  Array.of_list
    ([ "glab"; "api"; "-X"; "GET"; path ] @ if paginate then [ "--paginate" ] else [])

let encode ~keep s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if keep c then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let alnum = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true | _ -> false

let segment = function
  | "." -> "%2E"
  | ".." -> "%2E%2E"
  | s -> encode ~keep:(fun c -> alnum c || String.contains "._~-" c) s

let gl_encode = encode ~keep:(fun c -> alnum c || String.contains "_~-" c)

let gh_repo_path full_name =
  "repos/" ^ String.concat "/" (List.map segment (String.split_on_char '/' full_name))

let gl_project_path ~id ~full_name =
  match id with
  | Some n -> Printf.sprintf "projects/%d" n
  | None -> "projects/" ^ gl_encode full_name

let blob = "/-/blob/"

let find_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1)
  in
  go 0

let gl_readme_file ~default_branch url =
  match find_sub url blob with
  | None -> None
  | Some i -> (
      let rest =
        String.sub url
          (i + String.length blob)
          (String.length url - i - String.length blob)
      in
      let after_ref =
        match default_branch with
        | Some b when String.starts_with ~prefix:(b ^ "/") rest ->
            Some
              (String.sub rest
                 (String.length b + 1)
                 (String.length rest - String.length b - 1))
        | _ -> (
            match String.index_opt rest '/' with
            | Some j -> Some (String.sub rest (j + 1) (String.length rest - j - 1))
            | None -> None)
      in
      match after_ref with Some "" | None -> None | Some p -> Some p)

let git dir args = Array.of_list ([ "git"; "-C"; dir ] @ args)
let git_remotes d = git d [ "remote"; "-v" ]
let git_last_commit d = git d [ "log"; "-1"; "--format=%cI" ]
let git_count d = git d [ "rev-list"; "--count"; "HEAD" ]

let git_count_mine ~author d =
  git d [ "rev-list"; "--count"; "--regexp-ignore-case"; "--author=" ^ author; "HEAD" ]

let git_ls_files d = git d [ "ls-files" ]
let git_ls_files_z d = git d [ "ls-files"; "-z" ]

(* ------------------------------------------------------------ documents (FR-082) *)

let encode_path p = String.concat "/" (List.map segment (String.split_on_char '/' p))
let ref_or_head = function Some b when b <> "" -> b | _ -> "HEAD"

let gh_doc_tree ~full_name ~branch =
  gh
    (Printf.sprintf "%s/git/trees/%s?recursive=1" (gh_repo_path full_name)
       (segment (ref_or_head branch)))

let gh_doc_file ~full_name path =
  gh ~raw:true
    (Printf.sprintf "%s/contents/%s" (gh_repo_path full_name) (encode_path path))

let gl_doc_tree ~id ~full_name =
  glab ~paginate:true
    (gl_project_path ~id ~full_name ^ "/repository/tree?recursive=true&per_page=100")

let gl_doc_file ~id ~full_name ~branch path =
  glab
    (Printf.sprintf "%s/repository/files/%s/raw?ref=%s"
       (gl_project_path ~id ~full_name)
       (gl_encode path)
       (gl_encode (ref_or_head branch)))

(* ------------------------------------------------------------ per-repo plan *)

type repo = {
  host : string;
  owner : string;
  full_name : string;
  gl_id : int option;
  default_branch : string option;
  readme_url : string option;
  fork : bool;
  archived : bool;
  sources : string list;
  gl_contributed : bool;
  mrs : int;
}

let content_tiers = [ "1"; "collab"; "fork" ]

let gh_contrib_target ~(id : Identity.t) r =
  let user =
    String.lowercase_ascii r.owner
    = Option.fold ~none:"" ~some:String.lowercase_ascii id.gh_user
  in
  let collab = Identity.is_collab id r.owner in
  ((not (user || collab)) && not r.archived)
  || (List.mem r.owner (Identity.own_orgs id) && r.archived)
  || collab

let gl_contrib_target r = r.gl_contributed || r.mrs > 0 || List.mem "gl-user" r.sources
let cmd purpose argv = { purpose; argv }
let gl_project r = gl_project_path ~id:r.gl_id ~full_name:r.full_name

let metadata_plan ~id r (d : Decision.t) =
  if not d.detail then []
  else
    match r.host with
    | "github" ->
        (if r.fork then [ cmd Parent (gh (gh_repo_path r.full_name)) ] else [])
        @
        if gh_contrib_target ~id r then
          [
            cmd Contributors
              (gh (gh_repo_path r.full_name ^ "/contributors?per_page=100"));
          ]
        else []
    | "gitlab" ->
        cmd Languages (glab (gl_project r ^ "/languages"))
        ::
        (if gl_contrib_target r then
           [
             cmd Contributors
               (glab
                  (gl_project r
                 ^ "/repository/contributors?order_by=commits&sort=desc&per_page=100"));
           ]
         else [])
    | _ -> []

let readme_cmd r =
  match r.host with
  | "github" -> Some (cmd Readme (gh ~raw:true (gh_repo_path r.full_name ^ "/readme")))
  | "gitlab" -> (
      match
        ( Option.bind r.readme_url (gl_readme_file ~default_branch:r.default_branch),
          r.default_branch )
      with
      | Some file, Some branch ->
          Some
            (cmd Readme
               (glab
                  (Printf.sprintf "%s/repository/files/%s/raw?ref=%s" (gl_project r)
                     (gl_encode file) (gl_encode branch))))
      | _ -> None)
  | _ -> None

let last_commit_cmd r =
  match r.host with
  | "github" ->
      Some (cmd Last_commit (gh (gh_repo_path r.full_name ^ "/commits?per_page=1")))
  | "gitlab" ->
      Some (cmd Last_commit (glab (gl_project r ^ "/repository/commits?per_page=1")))
  | _ -> None

let root_listing_cmd r =
  match r.host with
  | "github" -> Some (cmd Root_listing (gh (gh_repo_path r.full_name ^ "/contents/")))
  | "gitlab" ->
      Some (cmd Root_listing (glab (gl_project r ^ "/repository/tree?per_page=100")))
  | _ -> None

let content_plan r ~tier ~campaign (d : Decision.t) =
  let selected = match tier with Some t -> List.mem t content_tiers | None -> false in
  let when_ c o = if c then Option.to_list o else [] in
  when_ (d.detail && selected) (last_commit_cmd r)
  @ when_ (d.readme && not campaign) (readme_cmd r)
  @ when_ (d.readme && selected && not campaign) (root_listing_cmd r)

let plan ~id r ~tier ~campaign d =
  metadata_plan ~id r d @ content_plan r ~tier ~campaign d

let manifest_names = [ "*.opam"; "package.json"; "Cargo.toml"; "pyproject.toml" ]

let manifest r file =
  match r.host with
  | "gitlab" ->
      let ref_ =
        match r.default_branch with Some b -> "?ref=" ^ gl_encode b | None -> ""
      in
      cmd Manifest
        (glab
           (Printf.sprintf "%s/repository/files/%s/raw%s" (gl_project r) (gl_encode file)
              ref_))
  | _ ->
      cmd Manifest (gh ~raw:true (gh_repo_path r.full_name ^ "/contents/" ^ segment file))
