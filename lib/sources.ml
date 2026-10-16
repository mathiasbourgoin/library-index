type entry = { row : Yojson.Safe.t; readme_url : string option }

type t = {
  entries : entry list;
  local : Yojson.Safe.t list;
  gaps : string list;
  failures : string list;
  listings_ok : bool;
  globals : (string * Yojson.Safe.t) list;
}

let search_pages = 10
let sprintf = Printf.sprintf

(* ------------------------------------------------------------ JSON plumbing *)

let parse_paginated out =
  try
    Some
      (List.concat_map
         (function `List l -> l | j -> [ j ])
         (List.of_seq (Yojson.Safe.seq_from_string out)))
  with Yojson.Json_error _ -> None

let count_search names =
  List.fold_left
    (fun acc n ->
      let n = String.lowercase_ascii n in
      match List.assoc_opt n acc with
      | Some (`Int c) ->
          List.map (fun (k, v) -> if k = n then (k, `Int (c + 1)) else (k, v)) acc
      | _ -> acc @ [ (n, `Int 1) ])
    [] names

let get_list ~call argv =
  match call { Commands.purpose = Listing; argv } with
  | Error f -> Error f
  | Ok out -> ( match parse_paginated out with Some l -> Ok l | None -> Error `Other)

let get_one ~call purpose argv =
  match call { Commands.purpose; argv } with
  | Error f -> Error f
  | Ok out when String.trim out = "" -> Ok `Null
  | Ok out -> ( match Jsonx.parse out with Some j -> Ok j | None -> Error `Other)

(* ------------------------------------------------------------ accumulator *)

type acc = {
  repos : (string, entry * string list) Hashtbl.t;
  mutable gaps : string list;  (** Reversed. *)
  mutable failures : string list;  (** Reversed. *)
  mutable ok : bool;
  mutable globals : (string * Yojson.Safe.t) list;
}

let info acc g = acc.gaps <- g :: acc.gaps

let failure acc g =
  info acc g;
  acc.failures <- g :: acc.failures;
  acc.ok <- false

let fail_listing acc what f =
  failure acc
    (sprintf "%s failed (%s); unseen repositories were not marked missing." what
       (Proc.describe f))

let global acc k v = acc.globals <- acc.globals @ [ (k, v) ]
let pushed row = Option.value (Jsonx.str "pushed_at" row) ~default:""

(* EC-11: a repository listed twice keeps the most recent push; sources accumulate. *)
let add acc src ?readme_url row =
  match Norm.key row with
  | None -> ()
  | Some k -> (
      match Hashtbl.find_opt acc.repos k with
      | None -> Hashtbl.replace acc.repos k ({ row; readme_url }, [ src ])
      | Some (e, srcs) ->
          let srcs = if List.mem src srcs then srcs else srcs @ [ src ] in
          let e =
            if pushed row > pushed e.row then
              {
                row;
                readme_url = (if readme_url = None then e.readme_url else readme_url);
              }
            else e
          in
          Hashtbl.replace acc.repos k (e, srcs))

let add_gh acc src l = List.iter (fun j -> Option.iter (add acc src) (Norm.gh j)) l

let add_gl acc src l =
  List.iter
    (fun j -> Option.iter (add acc src ?readme_url:(Norm.gl_readme_url j)) (Norm.gl j))
    l

let update acc key f =
  Option.iter
    (fun (e, srcs) -> Hashtbl.replace acc.repos key ({ e with row = f e.row }, srcs))
    (Hashtbl.find_opt acc.repos key)

let path_only p =
  match String.index_opt p '?' with Some i -> String.sub p 0 i | None -> p

(* ------------------------------------------------------------ GitHub *)

let gh_listing acc ~call src path =
  match get_list ~call (Commands.gh ~paginate:true path) with
  | Ok l -> add_gh acc src l
  | Error f -> fail_listing acc (sprintf "GitHub listing `%s`" (path_only path)) f

let gh_org acc ~(id : Identity.t) ~call org =
  let absent () = info acc (sprintf "GitHub org `%s` does not exist (404)." org) in
  match get_one ~call Listing (Commands.gh ("orgs/" ^ Commands.segment org)) with
  | Error `Not_found -> absent ()
  | Error f -> fail_listing acc (sprintf "GitHub org `%s` lookup" org) f
  | Ok j when Jsonx.str "login" j = None -> absent ()
  | Ok _ -> (
      let path = sprintf "orgs/%s/repos?type=all&per_page=100" (Commands.segment org) in
      match get_list ~call (Commands.gh ~paginate:true path) with
      | Error f -> fail_listing acc (sprintf "GitHub listing of org `%s`" org) f
      | Ok [] ->
          info acc
            (sprintf
               "GitHub org `%s` exists but lists 0 repos visible to %s (not a member)."
               org
               (Option.value id.gh_user ~default:"the authenticated user"))
      | Ok l -> add_gh acc ("gh-org:" ^ org) l)

let member_orgs acc ~call =
  match get_list ~call (Commands.gh ~paginate:true "user/memberships/orgs") with
  | Error f -> fail_listing acc "GitHub listing `user/memberships/orgs`" f
  | Ok l ->
      global acc "gh_member_orgs"
        (`List
           (List.filter_map
              (fun m ->
                Option.map
                  (fun s -> `String s)
                  (Jsonx.str "login" (Jsonx.get "organization" m)))
              l))

(* One GitHub search, page by page (C-20). *)
let search_names ~call query extract =
  let rec page n names =
    if n > search_pages then Ok names
    else
      let path = sprintf "%s&per_page=100&page=%d" query n in
      match get_one ~call Search (Commands.gh path) with
      | Error f -> Error f
      | Ok j ->
          let items = match Jsonx.get "items" j with `List l -> l | _ -> [] in
          let names = names @ List.filter_map extract items in
          if List.length items < 100 then Ok names else page (n + 1) names
  in
  page 1 []

let commit_repo item = Jsonx.str "full_name" (Jsonx.get "repository" item)

let pr_repo item =
  let prefix = "https://api.github.com/repos/" in
  match Jsonx.str "repository_url" item with
  | Some u when String.starts_with ~prefix u ->
      Some (String.sub u (String.length prefix) (String.length u - String.length prefix))
  | _ -> None

let search acc ~call ~global_key query extract =
  match search_names ~call query extract with
  | Ok names ->
      let counts = count_search names in
      global acc global_key (`Assoc counts);
      List.map fst counts
  | Error f ->
      fail_listing acc (sprintf "GitHub search `%s`" (path_only query)) f;
      []

let participation acc ~call names =
  List.iter
    (fun fn ->
      if not (Hashtbl.mem acc.repos ("github:" ^ fn)) then
        match get_one ~call Metadata (Commands.gh (Commands.gh_repo_path fn)) with
        | Ok j -> Option.iter (add acc "gh-participation") (Norm.gh j)
        | Error `Not_found -> ()
        | Error f ->
            fail_listing acc "GitHub metadata of a participation-only repository" f)
    (List.sort_uniq compare names)

let gh_search acc ~call ~sleep author =
  let commits =
    search acc ~call ~global_key:"gh_search_commits"
      ("search/commits?q=author:" ^ Commands.segment author)
      commit_repo
  in
  sleep 3.;
  let prs =
    search acc ~call ~global_key:"gh_search_prs"
      ("search/issues?q=author:" ^ Commands.segment author ^ "+type:pr")
      pr_repo
  in
  info acc
    "GitHub commit/PR search caps at 1000 results and indexes default branches only; \
     counts are lower bounds.";
  participation acc ~call (commits @ prs)

(* Listing order of the prototype: user, affiliation, organisations, collaborators,
   memberships, then the two searches; each step runs only when its source is declared. *)
let github acc ~(id : Identity.t) ~call ~sleep =
  Option.iter
    (fun u ->
      gh_listing acc ~call "gh-user"
        (sprintf "users/%s/repos?type=all&per_page=100" (Commands.segment u)))
    id.gh_user;
  if id.gh_affiliation then
    gh_listing acc ~call "gh-affiliation"
      "user/repos?affiliation=owner,collaborator,organization_member&per_page=100";
  List.iter (fun (o : Identity.org) -> gh_org acc ~id ~call o.org) id.gh_orgs;
  List.iter
    (fun c ->
      gh_listing acc ~call "gh-collab"
        (sprintf "users/%s/repos?type=owner&per_page=100" (Commands.segment c)))
    id.gh_collabs;
  if id.gh_memberships then member_orgs acc ~call;
  Option.iter (gh_search acc ~call ~sleep) id.gh_search_author

(* ------------------------------------------------------------ GitLab *)

let gl_listing acc ~call src path =
  match get_list ~call (Commands.glab ~paginate:true path) with
  | Ok l -> add_gl acc src l
  | Error f -> fail_listing acc (sprintf "GitLab listing `%s`" (path_only path)) f

let gl_id_of row = match Jsonx.get "gl_id" row with `Int n -> Some n | _ -> None

let key_of_gl_id acc pid =
  Hashtbl.fold
    (fun k (e, _) found ->
      if found = None && gl_id_of e.row = Some pid then Some k else found)
    acc.repos None

let mr_counts mrs =
  List.fold_left
    (fun acc m ->
      match Jsonx.get "project_id" m with
      | `Int pid when List.mem_assoc pid acc ->
          List.map (fun (p, c) -> if p = pid then (p, c + 1) else (p, c)) acc
      | `Int pid -> acc @ [ (pid, 1) ]
      | _ -> acc)
    [] mrs

let merge_requests acc ~call uid =
  let path = sprintf "merge_requests?author_id=%d&scope=all&state=all&per_page=100" uid in
  match get_list ~call (Commands.glab ~paginate:true path) with
  | Error f -> fail_listing acc "GitLab listing `merge_requests`" f
  | Ok mrs ->
      List.iter
        (fun (pid, n) ->
          (if key_of_gl_id acc pid = None then
             match get_one ~call Metadata (Commands.glab (sprintf "projects/%d" pid)) with
             | Ok j ->
                 Option.iter
                   (add acc "gl-mr" ?readme_url:(Norm.gl_readme_url j))
                   (Norm.gl j)
             | Error `Not_found -> ()
             | Error f -> fail_listing acc "GitLab metadata of an MR-only project" f);
          Option.iter
            (fun k -> update acc k (Jsonx.set "mrs" (`Int n)))
            (key_of_gl_id acc pid))
        (mr_counts mrs);
      global acc "gl_mr_total" (`Int (List.length mrs))

let gl_groups acc ~(id : Identity.t) ~call =
  List.iter
    (fun (g, subgroups) ->
      gl_listing acc ~call ("gl-group:" ^ g)
        (sprintf "groups/%s/projects?include_subgroups=%b&per_page=100"
           (Commands.gl_encode g) subgroups))
    id.gl_groups

let gitlab_user acc ~id ~call uid =
  gl_listing acc ~call "gl-user" (sprintf "users/%d/projects?per_page=100" uid);
  gl_listing acc ~call "gl-contributed"
    (sprintf "users/%d/contributed_projects?per_page=100" uid);
  gl_groups acc ~id ~call;
  let contributed =
    Hashtbl.fold
      (fun k (_, srcs) l -> if List.mem "gl-contributed" srcs then k :: l else l)
      acc.repos []
  in
  merge_requests acc ~call uid;
  List.iter (fun k -> update acc k (Jsonx.set "gl_contributed" (`Bool true))) contributed

let user_id = function
  | `List (u :: _) -> ( match Jsonx.get "id" u with `Int n -> Some n | _ -> None)
  | _ -> None

(* With a GitLab user, its listings precede the groups and its MRs follow them (the
   prototype's order); without one, only the groups are listed. *)
let gitlab acc ~(id : Identity.t) ~call =
  let not_found () = failure acc "GitLab user not found / glab not authenticated." in
  match id.gl_user with
  | None -> gl_groups acc ~id ~call
  | Some u -> (
      match
        get_one ~call Listing (Commands.glab ("users?username=" ^ Commands.segment u))
      with
      | Ok j -> (
          match user_id j with
          | Some uid -> gitlab_user acc ~id ~call uid
          | None -> not_found ())
      | Error `Not_found -> not_found ()
      | Error f -> fail_listing acc "GitLab user lookup" f)

(* ------------------------------------------------------------ add_local_extras *)

let remote_metadata acc ~call key =
  let host, path =
    match String.index_opt key ':' with
    | Some i -> (String.sub key 0 i, String.sub key (i + 1) (String.length key - i - 1))
    | None -> ("", key)
  in
  let argv, norm =
    if host = "github" then (Commands.gh (Commands.gh_repo_path path), Norm.gh)
    else (Commands.glab ("projects/" ^ Commands.gl_encode path), Norm.gl)
  in
  match get_one ~call Metadata argv with
  | Ok j -> Option.map (fun row -> (row, Norm.gl_readme_url j)) (norm j)
  | Error `Not_found -> None
  | Error f ->
      fail_listing acc "Metadata of a local checkout's remote" f;
      None

let local_row (c : Local_scan.checkout) ~unreachable =
  let name = Filename.basename c.path in
  `Assoc
    [
      ("key", `String ("local:" ^ String.lowercase_ascii name));
      ("host", `String "local");
      ("full_name", `String name);
      ("owner", `String "(local)");
      ("name", `String name);
      ("url", `String ("file://" ^ c.path));
      ("private", `Bool true);
      ("fork", `Bool false);
      ("archived", `Bool false);
      ("language", `Null);
      ("description", `String "");
      ("default_branch", `Null);
      ("pushed_at", Jsonx.of_string_opt c.last_commit);
      ("updated_at", Jsonx.of_string_opt c.last_commit);
      ("parent", `Null);
      ("remote_url_hidden", `Bool unreachable);
      ("last_commit_local", Jsonx.of_string_opt c.last_commit);
    ]

(* Local checkouts with his commits whose remote the listings missed, and local-only
   repositories he authored, become entries ([refresh.py:329-362]). *)
let add_local_extras acc ~call local =
  List.iter
    (fun (c : Local_scan.checkout) ->
      if (not c.worktree) && c.my_commits > 0 then
        let fetched =
          match c.remote_key with
          | Some k when not (Hashtbl.mem acc.repos k) -> remote_metadata acc ~call k
          | _ -> None
        in
        match (fetched, c.remote_key) with
        | Some (row, readme_url), _ ->
            Option.iter
              (fun k ->
                Hashtbl.replace acc.repos k ({ row; readme_url }, [ "local-remote" ]))
              (Norm.key row)
        | None, Some k when Hashtbl.mem acc.repos k -> ()
        | None, k ->
            let row = local_row c ~unreachable:(k <> None) in
            let src = if k = None then "local-only" else "local-unreachable-remote" in
            Option.iter
              (fun key ->
                Hashtbl.replace acc.repos key ({ row; readme_url = None }, [ src ]))
              (Norm.key row))
    local

(* ------------------------------------------------------------ collect *)

let collect ~id ~call ~sleep ~dev_root =
  let acc =
    { repos = Hashtbl.create 1024; gaps = []; failures = []; ok = true; globals = [] }
  in
  github acc ~id ~call ~sleep;
  gitlab acc ~id ~call;
  let local = match dev_root with Some d -> Local_scan.scan ~id ~call d | None -> [] in
  add_local_extras acc ~call local;
  let entries =
    Hashtbl.fold
      (fun _ (e, srcs) l ->
        let srcs = `List (List.map (fun s -> `String s) (List.sort compare srcs)) in
        { e with row = Jsonx.set "sources" srcs e.row } :: l)
      acc.repos []
    |> List.sort (fun a b -> compare (Norm.key a.row) (Norm.key b.row))
  in
  {
    entries =
      List.map2
        (fun e row -> { e with row })
        entries
        (Local_scan.enrich ~call local (List.map (fun e -> e.row) entries));
    local = List.map Local_scan.to_json local;
    gaps = List.rev acc.gaps;
    failures = List.rev acc.failures;
    listings_ok = acc.ok;
    globals = acc.globals;
  }

let labels (s : Config.source) =
  match s.kind with
  | Github_user -> [ "gh-user" ]
  | Github_affiliation -> [ "gh-affiliation" ]
  | Github_org -> [ "gh-org:" ^ s.name ]
  | Github_collab -> [ "gh-collab" ]
  | Github_memberships -> []
  | Github_search -> [ "gh-participation" ]
  | Gitlab_user -> [ "gl-user"; "gl-contributed"; "gl-mr" ]
  | Gitlab_group -> [ "gl-group:" ^ s.name ]
  | Local_root -> [ "local-remote"; "local-only"; "local-unreachable-remote" ]
