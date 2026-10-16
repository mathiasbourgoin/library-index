(* Ports of refresh.py@b70f23d: participation (:426-459), classify (:472-498),
   is_campaign (:501-503), what (:510-511), fork_note (:514-525), campaign redaction
   (:529-532), first_para (:319-326), norm_remote (:269-273), EXT_LANG/local_language
   (:365-377). *)

type contrib = { commits : int; rank : int option; share : float; n_contributors : int }

type facts = {
  host : string;
  owner : string;
  name : string;
  full_name : string;
  fork : bool;
  description : string;
  contrib : contrib option;
  search_commits : int;
  search_prs : int;
  mrs : int;
  local_my_commits : int;
  gl_contributed : bool;
}

type derived = {
  role : string option;
  evidence : string;
  tier : string;
  domain : string option;
}

let lower = String.lowercase_ascii

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let contains ~sub s =
  let n = String.length s and k = String.length sub in
  let rec go i = i + k <= n && (String.sub s i k = sub || go (i + 1)) in
  go 0

(* ------------------------------------------------------------ JSON readers *)

let contrib_of_json = function
  | `Assoc _ as v ->
      Some
        {
          commits = Option.value (Jsonx.int_opt "commits" v) ~default:0;
          rank = Jsonx.int_opt "rank" v;
          share = Option.value (Jsonx.float_opt "share" v) ~default:0.;
          n_contributors = Option.value (Jsonx.int_opt "n_contributors" v) ~default:0;
        }
  | _ -> None

let string_field k v = Option.value (Jsonx.str k v) ~default:""

let facts_of_raw ~search_commits ~search_prs row =
  {
    host = string_field "host" row;
    owner = string_field "owner" row;
    name = string_field "name" row;
    full_name = string_field "full_name" row;
    fork = Jsonx.truthy (Jsonx.member "fork" row);
    description = string_field "description" row;
    contrib = Option.bind (Jsonx.member "contrib" row) contrib_of_json;
    search_commits;
    search_prs;
    mrs = Option.value (Jsonx.int_opt "mrs" row) ~default:0;
    local_my_commits = Option.value (Jsonx.int_opt "local_my_commits" row) ~default:0;
    gl_contributed = Jsonx.truthy (Jsonx.member "gl_contributed" row);
  }

(* ------------------------------------------------------------ participation *)

let evidence_of f c commits pr =
  let rank_part =
    match c with
    | Some { rank = Some rank; n_contributors; share; _ } when rank <> 0 ->
        Printf.sprintf " (rank %d/%d, %d%%)" rank n_contributors
          (truncate (share *. 100.))
    | _ -> ""
  in
  List.filter_map Fun.id
    [
      (if commits <> 0 then Some (Printf.sprintf "%d commits%s" commits rank_part)
       else None);
      (if pr <> 0 then Some (Printf.sprintf "%d PRs/MRs" pr) else None);
      (if f.gl_contributed && commits = 0 then Some "GitLab contributed-project" else None);
    ]
  |> String.concat ", "

let is_mirror ~(id : Identity.t) f =
  f.fork
  || contains ~sub:"mirror" (lower f.description)
  || match id.mirror_name with Some re -> Re.execp re (lower f.name) | None -> false

(* Roles for repos with some evidence of involvement (prototype :448-459). *)
let involved_role ~(id : Identity.t) f ~commits ~pr ~evs =
  let share, rank =
    match f.contrib with Some c -> (c.share, c.rank) | None -> (0., None)
  in
  if
    is_mirror ~id f && pr = 0 && share < 0.02
    && not (List.mem (lower f.owner) id.mirror_exempt_owners)
  then (Some "inherited", evs ^ " — historical commits carried by a fork/mirror")
  else if
    rank = Some 1 || share >= 0.4
    || (List.mem f.owner id.main_author_owners && commits + pr >= id.main_author_min)
  then (Some "main author", evs)
  else if
    List.mem f.owner id.major_contributor_owners && pr >= id.major_contributor_min_prs
  then (Some "contributor (major)", evs)
  else if commits >= 10 || pr >= 3 || share >= 0.05 then (Some "contributor", evs)
  else (Some "occasional", evs)

let participation ~id f =
  let own_user = Identity.is_user id f.owner || f.host = "local" in
  let c_commits = match f.contrib with Some c -> c.commits | None -> 0 in
  let commits = max c_commits (max f.search_commits f.local_my_commits) in
  let pr = f.search_prs + f.mrs in
  let evs = evidence_of f f.contrib commits pr in
  if own_user && not f.fork then (Some "owner", if evs = "" then "owned" else evs)
  else if not (commits <> 0 || pr <> 0 || f.gl_contributed) then
    if List.mem f.owner id.Identity.org_member_owners then
      (Some "org member", "repo in his org")
    else (None, "")
  else involved_role ~id f ~commits ~pr ~evs

(* ------------------------------------------------------------ tier / domain *)

let tier ~id f ~role =
  let t =
    if Identity.is_collab id f.owner then "collab"
    else
      match role with
      | Some
          ("owner" | "main author" | "contributor" | "contributor (major)" | "org member")
        ->
          if f.fork && role = Some "owner" then "fork" else "1"
      | _ when f.fork && Identity.is_user id f.owner -> "fork"
      | Some "occasional" -> "1"
      | Some "inherited" -> "inherited"
      | _ -> "2"
  in
  if t = "fork" && not f.fork then "1" else t

let computed_domain ~(id : Identity.t) ~owner =
  let o = lower owner in
  List.find_map
    (fun (d : Config.derived_domain) ->
      let rule = lower d.owner in
      if (d.prefix && starts_with ~prefix:rule o) || o = rule then Some d.domain else None)
    id.derived_domains

let derive ~id f =
  let role, evidence = participation ~id f in
  { role; evidence; tier = tier ~id f ~role; domain = computed_domain ~id ~owner:f.owner }

(* ------------------------------------------------------------ confidentiality *)

(* The campaign rules themselves live in {!Campaign} (INV-D). *)

let withheld_description = "security-research campaign repo (details withheld)"

let campaign_redaction ~(id : Identity.t) ~campaign ~name ~description ~synopsis =
  if campaign && not (List.mem (lower name) id.campaign_exempt_names) then
    (Some withheld_description, None)
  else (description, synopsis)

(* ------------------------------------------------------------ rendering helpers *)

let non_empty = function Some s when s <> "" -> Some s | _ -> None

let what_fallback ~what ~synopsis ~description =
  match List.find_map non_empty [ what; synopsis; description ] with
  | Some s -> s
  | None -> "—"

let fork_note ~fork ~parent ~maintained_side ~last_commit ~pushed_at ~parent_pushed_at
    ~parent_archived =
  if not fork then None
  else
    let p = Option.value (non_empty parent) ~default:"?" in
    let side =
      match non_empty maintained_side with
      | Some s -> s
      | None ->
          let mine =
            Option.value ~default:"" (List.find_map non_empty [ last_commit; pushed_at ])
          in
          let theirs = Option.value (non_empty parent_pushed_at) ~default:"" in
          if parent_archived then "fork (upstream archived)"
          else if String.compare mine theirs > 0 then "fork (his)"
          else "upstream"
    in
    Some (Printf.sprintf "fork of %s; maintained: %s" p side)

(* ------------------------------------------------------------ text helpers *)

(* Python's str.isspace / \s restricted to ASCII (READMEs are split on ASCII blank lines). *)
let ws = "[ \t\n\r\x0b\x0c\x1c-\x1f]"
let blank_line = Re.compile (Re.Perl.re ("\n" ^ ws ^ "*\n"))
let markup = Re.compile (Re.Perl.re "[*_`]|\\[([^\\]]*)\\]\\([^)]*\\)")
let spaces = Re.compile (Re.Perl.re (ws ^ "+"))

let is_space c =
  match c with
  | ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' | '\x1c' .. '\x1f' -> true
  | _ -> false

let strip s =
  let n = String.length s in
  let rec l i = if i < n && is_space s.[i] then l (i + 1) else i in
  let rec r j = if j > 0 && is_space s.[j - 1] then r (j - 1) else j in
  let i = l 0 in
  let j = r n in
  if j <= i then "" else String.sub s i (j - i)

let skipped_prefixes = [ "#"; "<"; "[!"; "!["; "```"; "|"; "---"; "=" ]

let first_para txt =
  let clean b =
    let b =
      Re.replace markup b ~f:(fun g -> Option.value (Re.Group.get_opt g 1) ~default:"")
    in
    Utf8.take (Re.replace_string spaces ~by:" " b) 220
  in
  List.find_map
    (fun block ->
      let b = strip block in
      if
        b = ""
        || List.exists (fun prefix -> starts_with ~prefix b) skipped_prefixes
        || Utf8.length b < 25
      then None
      else Some (clean b))
    (Re.split_delim blank_line txt)

let norm_remote = Remote.norm_remote

let ext_lang =
  [
    (".ml", "OCaml");
    (".mli", "OCaml");
    (".py", "Python");
    (".rs", "Rust");
    (".go", "Go");
    (".ts", "TypeScript");
    (".tsx", "TypeScript");
    (".js", "JavaScript");
    (".sh", "Shell");
    (".v", "Rocq");
    (".qnt", "Quint");
    (".tex", "TeX");
    (".md", "Markdown");
    (".sol", "Solidity");
    (".c", "C");
    (".cpp", "C++");
  ]

(* os.path.splitext: the extension of the basename, ignoring leading dots. *)
let extension path =
  let base =
    match String.rindex_opt path '/' with
    | Some i -> String.sub path (i + 1) (String.length path - i - 1)
    | None -> path
  in
  let n = String.length base in
  let rec lead i = if i < n && base.[i] = '.' then lead (i + 1) else i in
  let start = lead 0 in
  match String.rindex_opt base '.' with
  | Some i when i > start -> String.sub base i (n - i)
  | _ -> ""

let language_of_files files =
  let counts =
    List.fold_left
      (fun acc f ->
        match List.assoc_opt (extension f) ext_lang with
        | None -> acc
        | Some l when List.mem_assoc l acc ->
            List.map (fun (k, v) -> if k = l then (k, v + 1) else (k, v)) acc
        | Some l -> acc @ [ (l, 1) ])
      [] files
  in
  let code = List.filter (fun (k, _) -> k <> "Markdown") counts in
  let code = if code = [] then counts else code in
  List.fold_left
    (fun best (k, v) ->
      match best with Some (_, bv) when bv >= v -> best | _ -> Some (k, v))
    None code
  |> Option.map fst
