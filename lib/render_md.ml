(* Port of refresh.py@b70f23d render_md (:555-662). Each [a] call is one element of the
   prototype's list [L], joined with "\n" and terminated by "\n". *)

open Render_data

let md_esc s = replace_all ~sub:"\n" ~by:" " (replace_all ~sub:"|" ~by:"\\|" s)
let lock (r : repo) = if r.private_ then " 🔒" else ""
let archived_mark (r : repo) = if r.archived then " (archived)" else ""
let lang (r : repo) = Option.value (first_set [ r.language ]) ~default:"—"
let url (r : repo) = Option.value r.url ~default:"None"
let opt s = Option.value s ~default:""

let local_cell ~home (r : repo) =
  match r.local_paths with p :: _ -> "`" ^ tilde ~home p ^ "`" | [] -> ""

let code s = "`" ^ s ^ "`"
let codes l = String.concat ", " (List.map code l)

(* The "Sources:" line, built from the configured sources in the prototype's wording. *)
let sources_line data =
  let id = data.settings.identity in
  let gh =
    List.filter
      (fun s -> s <> "")
      [
        (match id.gh_user with
        | None -> ""
        | Some u ->
            "GitHub user " ^ code u
            ^
            if id.gh_affiliation then
              " + all repos visible to him (owner/collaborator/org member)"
            else "");
        (match Identity.listed_orgs id with [] -> "" | l -> "orgs " ^ codes l);
        (match id.gh_collabs with [] -> "" | l -> "collaborator " ^ codes l);
      ]
  in
  let groups = List.map fst id.gl_groups in
  let gl =
    match (id.gl_user, groups) with
    | None, [] -> ""
    | Some u, [] -> Printf.sprintf "GitLab user %s (owned, contributed, MRs)" (code u)
    | user, gs ->
        (match user with
          | Some u ->
              Printf.sprintf "GitLab user %s (owned, contributed, MRs) and " (code u)
          | None -> "GitLab ")
        ^ "groups " ^ codes gs
        ^ if List.exists snd id.gl_groups then " (with subgroups)" else ""
  in
  let parts =
    List.filter
      (fun s -> s <> "")
      [
        String.concat ", " gh;
        (if id.gh_memberships then
           Printf.sprintf "member orgs reached through affiliation (%s)"
             (String.concat ", " data.member_orgs)
         else "");
        gl;
        (match id.local_roots with [] -> "" | l -> "local " ^ codes l);
      ]
  in
  match parts with
  | [] -> "Sources: none configured.\n"
  | _ -> "Sources: " ^ String.concat "; " parts ^ ".\n"

let header data a =
  let id = data.settings.identity in
  a (Printf.sprintf "# %s\n" data.settings.title);
  a
    (Printf.sprintf
       "_Generated %s by `library-index render` from the SQLite index (`%s`: read-only \
        API metadata%s, updated by `library-index refresh`)._  "
       data.generated data.settings.db_display
       (match id.local_roots with [] -> "" | l -> " + local " ^ codes l ^ " checkouts"));
  a (sources_line data);
  a
    ("**Counts:** "
    ^ String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) data.counts)
    ^ "\n");
  a ("**Gaps:**\n" ^ String.concat "\n" (List.map (fun g -> "- " ^ g) data.gaps) ^ "\n")

let how_to_use data a =
  if data.settings.how_to <> [] then (
    a "## How to use this index\n";
    a (String.concat "\n" data.settings.how_to ^ "\n"))

(* ------------------------------------------------------------ section 1 *)

let part_rows repos =
  List.filter (fun r -> r.tier = "1" && r.role <> None && r.ownership <> "external") repos

let section1 ~home data a =
  let part = part_rows data.repos in
  a "## 1. Repos I participate in\n";
  a
    "Every repo with evidence of his involvement (owned, or commits/PRs/MRs found). \
     Role: owner / main author (rank-1 contributor or ≥40 % of commits) / contributor / \
     org member (repo in his own org, no commits found) / occasional. Evidence is a \
     lower bound. Libraries and tools are detailed by domain in section 2.\n";
  a "| Repo | Role | Evidence | Lang | Last commit | Status | Local |";
  a "|---|---|---|---|---|---|---|";
  stable_sort_by (fun r -> (role_rank r.role, -date_number (last_date r))) compare part
  |> List.iter (fun r ->
      if not (is_campaign r) then
        a
          (Printf.sprintf "| [%s](%s)%s | %s | %s | %s | %s | %s | %s |" r.full_name
             (url r) (lock r) (opt r.role) (md_esc r.evidence) (lang r)
             (d10 (last_date r))
             r.status (local_cell ~home r)));
  let sec = List.filter is_campaign part in
  if sec <> [] then
    a
      (Printf.sprintf
         "\n\
          **Security-research workspaces (%d, private audit mirrors — not reusable \
          libraries):** %s\n"
         (List.length sec)
         (String.concat ", "
            (List.map
               (fun r -> r.full_name)
               (stable_sort_by (fun r -> r.full_name) compare sec))));
  let inh = List.filter (fun r -> r.tier = "inherited") data.repos in
  let first_piece ev =
    match Re.split_delim (Re.compile (Re.str " — ")) ev with p :: _ -> p | [] -> ev
  in
  if inh <> [] then
    a
      (Printf.sprintf
         "\n\
          **Inherited history (%d forks/mirrors that merely carry his upstream \
          commits%s):** %s\n"
         (List.length inh)
         (if data.settings.inherited_examples = "" then ""
          else ", e.g. " ^ data.settings.inherited_examples)
         (String.concat ", "
            (List.map
               (fun r -> Printf.sprintf "%s (%s)" r.full_name (first_piece r.evidence))
               (stable_sort_by (fun r -> String.lowercase_ascii r.full_name) compare inh))))

(* ------------------------------------------------------------ section 2 *)

(* EC-21: a legacy domain outside DOMAIN_ORDER renders under "Other". *)
let domain_of ~domains r =
  match first_set [ r.domain ] with Some d when List.mem d domains -> d | _ -> "Other"

let section2 ~home data a =
  let domains = data.settings.identity.domains in
  a "\n## 2. Tier 1 — authored / maintained, by domain\n";
  let t1 =
    List.filter
      (fun r -> r.tier = "1" && r.ownership <> "external" && not (is_campaign r))
      data.repos
  in
  List.iter
    (fun dom ->
      let rows = List.filter (fun r -> domain_of ~domains r = dom) t1 in
      if rows <> [] then (
        a (Printf.sprintf "### %s\n" dom);
        a "| Repo | What it is | Lang | Last commit | Status | Interest / reuse | Local |";
        a "|---|---|---|---|---|---|---|";
        stable_sort_by
          (fun r -> (r.status <> "active", String.lowercase_ascii r.full_name))
          compare rows
        |> List.iter (fun r ->
            a
              (Printf.sprintf "| [%s](%s)%s%s | %s | %s | %s | %s | %s | %s |" r.full_name
                 (url r) (lock r) (archived_mark r)
                 (md_esc (what r))
                 (lang r)
                 (d10 (last_date r))
                 r.status
                 (md_esc (opt r.interest))
                 (local_cell ~home r)));
        a ""))
    domains

(* ------------------------------------------------------------ sections 3-5 *)

let side_of note =
  match note with
  | None -> ""
  | Some fn -> (
      match List.rev (Re.split_delim (Re.compile (Re.str "maintained: ")) fn) with
      | last :: _ -> last
      | [] -> fn)

let section3 data a =
  a "## 3. Forks (fork → maintained side)\n";
  a "| Fork | Parent | Maintained side | Lang | Fork last push | Note |";
  a "|---|---|---|---|---|---|";
  List.filter (fun r -> r.tier = "fork") data.repos
  |> stable_sort_by (fun r -> String.lowercase_ascii r.full_name) compare
  |> List.iter (fun r ->
      let parent = Option.value (first_set [ r.parent ]) ~default:"?" in
      a
        (Printf.sprintf "| [%s](%s)%s | %s | %s | %s | %s | %s |" r.full_name (url r)
           (lock r) parent (side_of r.fork_note) (lang r) (d10 r.pushed_at)
           (md_esc (Option.value (first_set [ r.what; Some r.description ]) ~default:""))))

let section4 ~home data a =
  a
    "\n\
     ## 4. Local-only repos (no remote) and local checkouts of repos outside the sources\n";
  a "| Path | Remote | Last commit | His commits / total |";
  a "|---|---|---|---|";
  List.iter
    (fun (l : local) ->
      a
        (Printf.sprintf "| `%s` | %s | %s | %d / %d |" (tilde ~home l.path)
           (Option.value (first_set [ l.remote_key ]) ~default:"local only, no remote")
           (d10 l.last_commit) l.my_commits l.total_commits))
    data.section4

let section5 data a =
  a
    (Printf.sprintf "\n## 5. %s — tools and libraries (collaborator)\n"
       (match data.settings.identity.gh_collabs with
       | [] -> "Collaborators"
       | l -> String.concat ", " l));
  a
    (Printf.sprintf
       "| Repo | What it is | Lang | Last commit | Fork | Interest / reuse | %s |"
       data.settings.contributes_column);
  a "|---|---|---|---|---|---|---|";
  List.filter (fun r -> r.tier = "collab") data.repos
  |> stable_sort_by (fun r -> (r.fork, String.lowercase_ascii r.full_name)) compare
  |> List.iter (fun r ->
      a
        (Printf.sprintf "| [%s](%s) | %s | %s | %s | %s | %s | %s |" r.full_name (url r)
           (md_esc (what r))
           (lang r)
           (d10 (last_date r))
           (Option.value (first_set [ r.fork_note ]) ~default:"no")
           (md_esc (opt r.interest))
           (if r.role <> None then r.evidence else "")))

(* ------------------------------------------------------------ sections 6-7 *)

let section6 data a =
  a "\n## 6. whitehat — internal tools (by campaign)\n";
  a
    (Printf.sprintf
       "Tools built inside security-research campaigns%s. Described generically on \
        purpose: no targets' internals, findings or exploit details. Finding-specific \
        PoCs/harnesses are collapsed as *details withheld*. Hand-curated in \
        `whitehat_tools.json`.\n"
       (if data.settings.whitehat_where = "" then ""
        else " (" ^ data.settings.whitehat_where ^ ")"));
  a
    "| Campaign repo | Tool path | What it does | Lang | Last commit | Reuse / \
     generalisation |";
  a "|---|---|---|---|---|---|";
  List.iter
    (fun t ->
      a
        (Printf.sprintf "| %s | `%s` | %s | %s | %s | %s |" (md_esc t.campaign)
           (md_esc t.tool_path) (md_esc t.tool_what) (md_esc t.tool_language)
           (md_esc t.tool_last_commit) (md_esc t.reuse)))
    data.tools

let section7 data a =
  a "\n## 7. Appendix — Tier 2 org repos without his involvement\n";
  let t2 = List.filter in_section7 data.repos in
  let orgs =
    List.sort_uniq compare (List.map (fun r -> r.owner) t2)
    |> stable_sort_by String.lowercase_ascii compare
  in
  List.iter
    (fun org ->
      let rows =
        List.filter (fun r -> r.owner = org) t2
        |> stable_sort_by (fun r -> (r.archived, -date_number r.pushed_at)) compare
      in
      a (Printf.sprintf "\n### %s (%d)\n" org (List.length rows));
      a "| Repo | Lang | Last push | Description |";
      a "|---|---|---|---|";
      List.iter
        (fun r ->
          a
            (Printf.sprintf "| [%s](%s)%s%s%s | %s | %s | %s |" r.name (url r) (lock r)
               (archived_mark r)
               (if r.fork then " (fork)" else "")
               (lang r) (d10 r.pushed_at)
               (Utf8.take (md_esc r.description) 140)))
        rows)
    orgs

(* Section 8: third-party libraries registered for use; omitted when there is none. *)
let section8 data a =
  match external_registered data with
  | [] -> ()
  | rows ->
      a "\n## 8. External libraries registered for use\n";
      a
        "Third-party libraries registered with `register-repo` (ownership `external`); \
         not counted as his.\n";
      a "| Key | What it is | Lang | Last commit |";
      a "|---|---|---|---|";
      List.iter
        (fun r ->
          a
            (Printf.sprintf "| `%s` | %s | %s | %s |" r.key
               (md_esc (what r))
               (lang r)
               (d10 (last_date r))))
        rows

let render ~home data =
  let b = Buffer.create 65536 in
  let a s =
    Buffer.add_string b s;
    Buffer.add_char b '\n'
  in
  header data a;
  how_to_use data a;
  section1 ~home data a;
  section2 ~home data a;
  section3 data a;
  section4 ~home data a;
  section5 data a;
  section6 data a;
  section7 data a;
  section8 data a;
  Buffer.contents b
