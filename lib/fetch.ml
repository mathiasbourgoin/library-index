type call = Commands.t -> (string, Proc.failure) result

type outcome = {
  fields : (string * Yojson.Safe.t) list;
  readme : string option option;
  detail_done : bool;
  failures : (Commands.purpose * Proc.failure) list;
}

let detail_fields =
  [
    "synopsis";
    "last_commit";
    "contrib";
    "parent";
    "parent_pushed_at";
    "parent_archived";
    "language";
  ]

let readme_cap = 256 * 1024
let marker = "\n\n[README cut at 256 KiB by library-index]\n"

let prepare_readme s =
  let s = Redact.text s in
  if String.length s <= readme_cap then s
  else Utf8.truncate_bytes s (readme_cap - String.length marker) ^ marker

let round3 x = Float.round (x *. 1000.) /. 1000.

(* ------------------------------------------------------------ manifests and READMEs *)

let unquote line =
  match (String.index_opt line '"', String.rindex_opt line '"') with
  | Some i, Some j when j > i + 1 -> Some (String.sub line (i + 1) (j - i - 1))
  | _ -> None

let first_line_value ~prefix ~sep content =
  String.split_on_char '\n' content
  |> List.find_map (fun l ->
      let l = String.trim l in
      if not (String.starts_with ~prefix l) then None
      else
        let rest =
          String.trim
            (String.sub l (String.length prefix) (String.length l - String.length prefix))
        in
        if String.starts_with ~prefix:sep rest then unquote rest else None)

let nonempty = function
  | Some s when String.trim s <> "" -> Some (Utf8.take s 1000)
  | _ -> None

let synopsis_of_manifest ~name content =
  nonempty
    (if Filename.check_suffix name ".opam" then
       Option.bind (first_line_value ~prefix:"synopsis" ~sep:":" content) Option.some
     else if name = "package.json" then
       match Jsonx.parse content with Some j -> Jsonx.str "description" j | None -> None
     else if name = "Cargo.toml" || name = "pyproject.toml" then
       first_line_value ~prefix:"description" ~sep:"=" content
     else None)

let read_file path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Some (really_input_string ic (in_channel_length ic)))
  with Sys_error _ | End_of_file -> None

let is_file p =
  try Sys.file_exists p && not (Sys.is_directory p) with Sys_error _ -> false

(* Manifest file names of a directory listing, in precedence order. *)
let manifest_candidates names =
  let opams =
    List.sort compare (List.filter (fun n -> Filename.check_suffix n ".opam") names)
  in
  opams
  @ List.filter
      (fun n -> List.mem n names)
      [ "package.json"; "Cargo.toml"; "pyproject.toml" ]

let local_synopsis dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> None
  | entries ->
      let files =
        List.filter (fun n -> is_file (Filename.concat dir n)) (Array.to_list entries)
      in
      List.find_map
        (fun n ->
          Option.bind (read_file (Filename.concat dir n)) (synopsis_of_manifest ~name:n))
        (manifest_candidates files)

let local_readme dir =
  List.find_map
    (fun n ->
      let p = Filename.concat dir n in
      if is_file p then read_file p else None)
    [ "README.md"; "README.org"; "README"; "readme.md" ]

(* ------------------------------------------------------------ contributors *)

let contrib_record ~commits ~rank ~share ~n =
  `Assoc
    [
      ("commits", `Int commits);
      ("rank", match rank with Some r -> `Int r | None -> `Null);
      ("share", `Float (round3 share));
      ("n_contributors", `Int n);
    ]

let gh_contrib ~(id : Identity.t) s =
  match Jsonx.parse s with
  | Some (`List l) ->
      let total =
        match List.fold_left (fun a c -> a + Jsonx.int "contributions" c) 0 l with
        | 0 -> 1
        | t -> t
      in
      let n = List.length l in
      let rec find rank = function
        | [] -> `Assoc [ ("commits", `Int 0); ("n_contributors", `Int n) ]
        | c :: rest ->
            let login = Option.value (Jsonx.str "login" c) ~default:"" in
            if
              Option.map String.lowercase_ascii id.gh_user
              = Some (String.lowercase_ascii login)
            then
              let commits = Jsonx.int "contributions" c in
              contrib_record ~commits ~rank:(Some rank)
                ~share:(Float.of_int commits /. Float.of_int total)
                ~n
            else find (rank + 1) rest
      in
      Some (find 1 l)
  | _ -> None

let is_me ~(id : Identity.t) c =
  let email = String.lowercase_ascii (Option.value (Jsonx.str "email" c) ~default:"") in
  List.mem
    (String.lowercase_ascii (Option.value (Jsonx.str "name" c) ~default:""))
    id.me_names
  || List.exists (fun part -> Re.execp (Re.compile (Re.str part)) email) id.me_email_parts

let gl_contrib ~id s =
  match Jsonx.parse s with
  | Some (`List l) ->
      let total =
        match List.fold_left (fun a c -> a + Jsonx.int "commits" c) 0 l with
        | 0 -> 1
        | t -> t
      in
      let mine = List.filter (is_me ~id) l in
      let commits = List.fold_left (fun a c -> a + Jsonx.int "commits" c) 0 mine in
      let rank = List.find_index (is_me ~id) l |> Option.map (fun i -> i + 1) in
      Some
        (contrib_record ~commits ~rank
           ~share:(Float.of_int commits /. Float.of_int total)
           ~n:(List.length l))
  | _ -> None

(* ------------------------------------------------------------ parsers per purpose *)

let top_language s =
  match Jsonx.parse s with
  | Some (`Assoc (_ :: _ as m)) ->
      List.fold_left
        (fun best (k, v) ->
          match (best, Jsonx.number v) with
          | None, Some x -> Some (k, x)
          | Some (_, b), Some x when x > b -> Some (k, x)
          | _ -> best)
        None m
      |> Option.map (fun (k, _) -> [ ("language", `String k) ])
  | _ -> None

let parent_fields s =
  match Option.bind (Jsonx.parse s) (Jsonx.member "parent") with
  | Some (`Assoc _ as p) ->
      Some
        [
          ("parent", Jsonx.of_string_opt (Jsonx.str "full_name" p));
          ("parent_pushed_at", Jsonx.of_string_opt (Jsonx.str "pushed_at" p));
          ("parent_archived", `Bool (Jsonx.bool "archived" p));
        ]
  | _ -> None

let last_commit_of ~host s =
  match Jsonx.parse s with
  | Some (`List (c :: _)) ->
      let date =
        if host = "github" then
          Option.bind (Jsonx.member "commit" c) (fun x ->
              Option.bind (Jsonx.member "committer" x) (Jsonx.str "date"))
        else Jsonx.str "committed_date" c
      in
      Some [ ("last_commit", Jsonx.of_string_opt date) ]
  | Some (`List []) -> Some [ ("last_commit", `Null) ]
  | _ -> None

let names_of_listing s =
  match Jsonx.parse s with
  | Some (`List l) ->
      Some
        (List.filter_map
           (fun e ->
             match (Jsonx.str "name" e, Jsonx.str "type" e) with
             | Some n, Some ("file" | "blob") -> Some n
             | _ -> None)
           l)
  | _ -> None

(* ------------------------------------------------------------ the run *)

let repo_of_row ?readme_url row =
  {
    Commands.host = Option.value (Jsonx.str "host" row) ~default:"";
    owner = Option.value (Jsonx.str "owner" row) ~default:"";
    full_name = Option.value (Jsonx.str "full_name" row) ~default:"";
    gl_id = Jsonx.int_opt "gl_id" row;
    default_branch = Jsonx.str "default_branch" row;
    readme_url;
    fork = Jsonx.bool "fork" row;
    archived = Jsonx.bool "archived" row;
    sources =
      (match Jsonx.member "sources" row with
      | Some (`List l) -> Jsonx.strings l
      | _ -> []);
    gl_contributed = Jsonx.bool "gl_contributed" row;
    mrs = Jsonx.int "mrs" row;
  }

(* Mutable accumulator local to one [run]. *)
type acc = {
  mutable fields : (string * Yojson.Safe.t) list;
  mutable failures : (Commands.purpose * Proc.failure) list;
}

let set acc kvs =
  acc.fields <- List.filter (fun (k, _) -> not (List.mem_assoc k kvs)) acc.fields @ kvs

(* Runs [c]; [`Not_found] is [Ok None]; any other failure is recorded. *)
let exec ~call acc (c : Commands.t) =
  match call c with
  | Ok s -> Some (Some s)
  | Error `Not_found -> Some None
  | Error f ->
      acc.failures <- acc.failures @ [ (c.purpose, f) ];
      None

let apply_metadata ~id ~call acc repo (c : Commands.t) =
  match exec ~call acc c with
  | None | Some None -> ()
  | Some (Some out) -> (
      let parsed =
        match c.purpose with
        | Parent -> parent_fields out
        | Languages -> top_language out
        | Contributors ->
            let f =
              if repo.Commands.host = "github" then gh_contrib ~id else gl_contrib ~id
            in
            Option.map (fun j -> [ ("contrib", j) ]) (f out)
        | _ -> None
      in
      match parsed with Some kvs -> set acc kvs | None -> ())

let merged row acc =
  match row with
  | `Assoc m ->
      `Assoc (List.filter (fun (k, _) -> not (List.mem_assoc k acc.fields)) m @ acc.fields)
  | j -> j

let api_synopsis ~call acc repo listing_out =
  match names_of_listing listing_out with
  | None -> None
  | Some names ->
      List.find_map
        (fun n ->
          match exec ~call acc (Commands.manifest repo n) with
          | Some (Some content) -> synopsis_of_manifest ~name:n content
          | _ -> None)
        (manifest_candidates names)

let local_dir row =
  match Jsonx.member "local_paths" row with
  | Some (`List (`String p :: _)) -> Some p
  | _ -> None

(* Synopsis precedence (C-19): the local checkout's manifests, else the API root
   listing's manifests, else the README's first paragraph (the checkout's README file
   first, then the fetched one). *)
let synopsis_of ~call acc repo ~dir ~listing ~readme =
  let fetched_para () =
    match readme with Some (Some t) -> Classify.first_para t | _ -> None
  in
  List.find_map
    (fun f -> f ())
    [
      (fun () -> Option.bind dir local_synopsis);
      (fun () -> Option.bind listing (api_synopsis ~call acc repo));
      (fun () -> Option.bind (Option.bind dir local_readme) Classify.first_para);
      fetched_para;
    ]

type content = {
  mutable readme_out : string option option;
  mutable listing : string option;
  mutable skipped : bool;  (** Due, but nothing could be read (checkout gone). *)
}

let apply_content ~call acc repo st (c : Commands.t) =
  match (c.purpose, exec ~call acc c) with
  | Last_commit, Some (Some out) ->
      Option.iter (set acc) (last_commit_of ~host:repo.Commands.host out)
  | Last_commit, Some None -> set acc [ ("last_commit", `Null) ]
  | Readme, Some r -> st.readme_out <- Some (Option.map prepare_readme r)
  | Root_listing, Some r -> st.listing <- r
  | _ -> ()

(* [local:] rows: README and last commit come from the checkout on disk. A row whose
   checkout is gone is skipped, never read as "no README" (AM-18). *)
let local_content acc row st ~selected (d : Decision.t) =
  (if d.readme then
     match local_dir row with
     | None -> st.skipped <- true
     | Some dir -> st.readme_out <- Some (Option.map prepare_readme (local_readme dir)));
  if d.detail && selected then
    set acc
      [
        ("last_commit", Option.value (Jsonx.member "last_commit_local" row) ~default:`Null);
      ]

let detail_purposes = Commands.[ Parent; Contributors; Languages; Last_commit ]
let content_purposes = Commands.[ Readme; Root_listing; Manifest ]

let content_failed acc =
  List.exists (fun (p, _) -> List.mem p content_purposes) acc.failures

(* AM-18: the synopsis is replaced only by a complete content fetch; a failed or skipped
   README, root-listing or manifest fetch keeps the stored one. A campaign row never
   carries one (FR-071). *)
let update_synopsis ~call acc repo row st ~campaign ~selected (d : Decision.t) =
  if campaign then set acc [ ("synopsis", `Null) ]
  else if d.readme && selected && (not st.skipped) && not (content_failed acc) then
    let s =
      synopsis_of ~call acc repo ~dir:(local_dir row) ~listing:st.listing
        ~readme:st.readme_out
    in
    if not (content_failed acc) then set acc [ ("synopsis", Jsonx.of_string_opt s) ]

let run ~id ~call ~row ?readme_url ~search:(search_commits, search_prs) (d : Decision.t) =
  let repo = repo_of_row ?readme_url row in
  let acc = { fields = []; failures = [] } in
  (* The row carries the flag {!Campaign.decide} set before the fetch (INV-D). *)
  let campaign = Jsonx.truthy (Jsonx.member "campaign" row) in
  List.iter (apply_metadata ~id ~call acc repo) (Commands.metadata_plan ~id repo d);
  let derived =
    Classify.derive ~id
      (Classify.facts_of_raw ~search_commits ~search_prs (merged row acc))
  in
  (* An explicitly registered repository gets its content whatever its tier
     ("Ownership kind": third-party libraries registered for use). *)
  let registered =
    match Jsonx.str "key" row with Some k -> Identity.is_registered id k | None -> false
  in
  let content_tier = if registered then "1" else derived.tier in
  let selected = List.mem content_tier Commands.content_tiers in
  let st = { readme_out = None; listing = None; skipped = false } in
  List.iter
    (apply_content ~call acc repo st)
    (Commands.content_plan repo ~tier:(Some content_tier) ~campaign d);
  if repo.host = "local" && not campaign then local_content acc row st ~selected d;
  (* Due, no command possible (GitLab project without readme_url): "no README". *)
  if
    d.readme && (not campaign) && (not st.skipped) && st.readme_out = None
    && not (List.mem_assoc Commands.Readme acc.failures)
  then st.readme_out <- Some None;
  update_synopsis ~call acc repo row st ~campaign ~selected d;
  let detail_failed =
    List.exists (fun (p, _) -> List.mem p detail_purposes) acc.failures
  in
  {
    fields = acc.fields;
    readme = st.readme_out;
    detail_done = d.detail && not detail_failed;
    failures = acc.failures;
  }
