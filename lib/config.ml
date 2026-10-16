type kind =
  | Github_user
  | Github_org
  | Github_affiliation
  | Github_collab
  | Github_memberships
  | Github_search
  | Gitlab_user
  | Gitlab_group
  | Local_root

let kind_names =
  [
    ("github-user", Github_user);
    ("github-org", Github_org);
    ("github-affiliation", Github_affiliation);
    ("github-collab", Github_collab);
    ("github-memberships", Github_memberships);
    ("github-search", Github_search);
    ("gitlab-user", Gitlab_user);
    ("gitlab-group", Gitlab_group);
    ("local-root", Local_root);
  ]

type confidential = Generic | Exclude_from_page | Not_confidential

let confidential_names =
  [
    ("generic", Generic);
    ("exclude-from-page", Exclude_from_page);
    ("none", Not_confidential);
  ]

let confidential_name c = fst (List.find (fun (_, v) -> v = c) confidential_names)

type ownership = Own | Participant | External

let ownership_names =
  [ ("own", Own); ("participant", Participant); ("external", External) ]

let ownership_name o = fst (List.find (fun (_, v) -> v = o) ownership_names)

type source = {
  kind : kind;
  name : string;
  host : string option;
  own : bool;
  probe : bool;
  include_subgroups : bool;
  confidential : confidential;
  campaign_prefix : bool;
}

type repo = {
  target : string;
  key : string option;
  ownership : ownership option;
  own : bool;
  confidential : confidential;
  campaign : bool;
  campaign_prefix : bool;
  deep : bool;
}

type owner = {
  display_name : string;
  author_regex : string;
  gitlab_me_names : string list;
  gitlab_email_fragments : string list;
}

type derived_domain = { owner : string; prefix : bool; domain : string }

type classify = {
  mirror_name_regex : string;
  mirror_exempt_owners : string list;
  major_contributor_owners : string list;
  major_contributor_min_prs : int;
  org_member_owners : string list;
  main_author_owners : string list;
  main_author_min : int;
  derived_domains : derived_domain list;
  campaign_owner_prefixes : string list;
  campaign_exempt_names : string list;
  domains : string list option;
}

type render = {
  title : string;
  notes_dir : string option;
  index_md : string option;
  how_to : string list option;
  inherited_examples : string;
  contributes_column : string;
  whitehat_where : string;
  fixed_gaps : string list option;
}

type t = {
  owner : owner;
  render : render;
  sources : source list;
  repos : repo list;
  classify : classify;
}

let empty_owner =
  {
    display_name = "";
    author_regex = "";
    gitlab_me_names = [];
    gitlab_email_fragments = [];
  }

let empty_render =
  {
    title = "Repository & library index";
    notes_dir = None;
    index_md = None;
    how_to = None;
    inherited_examples = "";
    contributes_column = "Owner contributes";
    whitehat_where = "";
    fixed_gaps = None;
  }

let empty_classify =
  {
    mirror_name_regex = "";
    mirror_exempt_owners = [];
    major_contributor_owners = [];
    major_contributor_min_prs = 20;
    org_member_owners = [];
    main_author_owners = [];
    main_author_min = 10;
    derived_domains = [];
    campaign_owner_prefixes = [];
    campaign_exempt_names = [];
    domains = None;
  }

let empty =
  {
    owner = empty_owner;
    render = empty_render;
    sources = [];
    repos = [];
    classify = empty_classify;
  }

let file_name = "sources.toml"

(* ------------------------------------------------------------ typed readers *)

let ( let* ) = Result.bind

type table = (string * Otoml.t) list

let type_error path what = Error (Printf.sprintf "%s: expected %s" path what)

let check_keys path allowed (tbl : table) =
  match List.find_opt (fun (k, _) -> not (List.mem k allowed)) tbl with
  | None -> Ok ()
  | Some (k, _) ->
      Error
        (Printf.sprintf "%s: unknown key '%s' (allowed: %s)" path k
           (String.concat ", " allowed))

let as_table path = function
  | Otoml.TomlTable t | Otoml.TomlInlineTable t -> Ok t
  | _ -> type_error path "a table"

let field path tbl k conv default =
  match List.assoc_opt k tbl with None -> Ok default | Some v -> conv (path ^ "." ^ k) v

let str path = function Otoml.TomlString s -> Ok s | _ -> type_error path "a string"
let bool path = function Otoml.TomlBoolean b -> Ok b | _ -> type_error path "a boolean"
let int path = function Otoml.TomlInteger n -> Ok n | _ -> type_error path "an integer"
let some conv path v = Result.map Option.some (conv path v)

let list conv path = function
  | Otoml.TomlArray l | Otoml.TomlTableArray l ->
      List.fold_right
        (fun (i, v) acc ->
          let* rest = acc in
          let* x = conv (Printf.sprintf "%s[%d]" path i) v in
          Ok (x :: rest))
        (List.mapi (fun i v -> (i, v)) l)
        (Ok [])
  | _ -> type_error path "an array"

let strings = list str

(* ------------------------------------------------------------ sections *)

let owner_of path v =
  let* t = as_table path v in
  let* () =
    check_keys path
      [ "display_name"; "author_regex"; "gitlab_me_names"; "gitlab_email_fragments" ]
      t
  in
  let* display_name = field path t "display_name" str "" in
  let* author_regex = field path t "author_regex" str "" in
  let* gitlab_me_names = field path t "gitlab_me_names" strings [] in
  let* gitlab_email_fragments = field path t "gitlab_email_fragments" strings [] in
  Ok { display_name; author_regex; gitlab_me_names; gitlab_email_fragments }

let render_of path v =
  let* t = as_table path v in
  let* () =
    check_keys path
      [
        "title";
        "notes_dir";
        "index_md";
        "how_to";
        "inherited_examples";
        "contributes_column";
        "whitehat_where";
        "fixed_gaps";
      ]
      t
  in
  let d = empty_render in
  let* title = field path t "title" str d.title in
  let* notes_dir = field path t "notes_dir" (some str) None in
  let* index_md = field path t "index_md" (some str) None in
  let* how_to = field path t "how_to" (some strings) None in
  let* inherited_examples = field path t "inherited_examples" str "" in
  let* contributes_column = field path t "contributes_column" str d.contributes_column in
  let* whitehat_where = field path t "whitehat_where" str "" in
  let* fixed_gaps = field path t "fixed_gaps" (some strings) None in
  Ok
    {
      title;
      notes_dir;
      index_md;
      how_to;
      inherited_examples;
      contributes_column;
      whitehat_where;
      fixed_gaps;
    }

let closed_of names path = function
  | Otoml.TomlString s when List.mem_assoc s names -> Ok (List.assoc s names)
  | _ -> type_error path ("one of " ^ String.concat ", " (List.map fst names))

let confidential_of = closed_of confidential_names
let ownership_of = closed_of ownership_names

let kind_of path = function
  | Otoml.TomlString s -> (
      match List.assoc_opt s kind_names with
      | Some k -> Ok k
      | None ->
          Error
            (Printf.sprintf "%s: unknown source kind '%s' (allowed: %s)" path s
               (String.concat ", " (List.map fst kind_names))))
  | _ -> type_error path "a string"

let source_keys =
  [
    "kind";
    "name";
    "host";
    "own";
    "probe";
    "include_subgroups";
    "confidential";
    "campaign_prefix";
  ]

let kind_name k = fst (List.find (fun (_, v) -> v = k) kind_names)
let nameless = [ Github_affiliation; Github_memberships; Github_search ]

let source_of path v =
  let* t = as_table path v in
  let* () = check_keys path source_keys t in
  let* kind =
    match List.assoc_opt "kind" t with
    | None -> Error (path ^ ": missing key 'kind'")
    | Some k -> kind_of (path ^ ".kind") k
  in
  let* name = field path t "name" str "" in
  let* () =
    if name = "" && not (List.mem kind nameless) then Error (path ^ ": missing key 'name'")
    else Ok ()
  in
  let* host = field path t "host" (some str) None in
  let* own = field path t "own" bool false in
  let* probe = field path t "probe" bool false in
  let* include_subgroups = field path t "include_subgroups" bool true in
  let* confidential = field path t "confidential" confidential_of Not_confidential in
  let* campaign_prefix = field path t "campaign_prefix" bool false in
  (* Confidentiality applies through an owner name or a local root's checkouts. *)
  let* () =
    if (confidential <> Not_confidential || campaign_prefix) && List.mem kind nameless
    then Error (path ^ ": confidential / campaign_prefix need an owner or a local root")
    else Ok ()
  in
  Ok { kind; name; host; own; probe; include_subgroups; confidential; campaign_prefix }

let repo_keys =
  [
    "target";
    "key";
    "ownership";
    "own";
    "confidential";
    "campaign";
    "campaign_prefix";
    "deep";
  ]

let repo_of path v =
  let* t = as_table path v in
  let* () = check_keys path repo_keys t in
  let* target =
    match List.assoc_opt "target" t with
    | None -> Error (path ^ ": missing key 'target'")
    | Some x -> str (path ^ ".target") x
  in
  let* key = field path t "key" (some str) None in
  let* ownership = field path t "ownership" (some ownership_of) None in
  let* own = field path t "own" bool false in
  let* confidential = field path t "confidential" confidential_of Not_confidential in
  let* campaign = field path t "campaign" bool false in
  let* campaign_prefix = field path t "campaign_prefix" bool false in
  let* deep = field path t "deep" bool false in
  Ok { target; key; ownership; own; confidential; campaign; campaign_prefix; deep }

let derived_domain_of path v =
  let* t = as_table path v in
  let* () = check_keys path [ "owner"; "prefix"; "domain" ] t in
  let* owner = field path t "owner" str "" in
  let* prefix = field path t "prefix" bool false in
  let* domain = field path t "domain" str "" in
  if owner = "" || domain = "" then Error (path ^ ": 'owner' and 'domain' are required")
  else Ok { owner; prefix; domain }

let classify_of path v =
  let* t = as_table path v in
  let* () =
    check_keys path
      [
        "mirror_name_regex";
        "mirror_exempt_owners";
        "major_contributor_owners";
        "major_contributor_min_prs";
        "org_member_owners";
        "main_author_owners";
        "main_author_min";
        "derived_domains";
        "campaign_owner_prefixes";
        "campaign_exempt_names";
        "domains";
      ]
      t
  in
  let d = empty_classify in
  let* mirror_name_regex = field path t "mirror_name_regex" str "" in
  let* () =
    match Re.Perl.re mirror_name_regex |> Re.compile with
    | _ -> Ok ()
    | exception _ -> Error (path ^ ".mirror_name_regex: invalid regular expression")
  in
  let* mirror_exempt_owners = field path t "mirror_exempt_owners" strings [] in
  let* major_contributor_owners = field path t "major_contributor_owners" strings [] in
  let* major_contributor_min_prs =
    field path t "major_contributor_min_prs" int d.major_contributor_min_prs
  in
  let* org_member_owners = field path t "org_member_owners" strings [] in
  let* main_author_owners = field path t "main_author_owners" strings [] in
  let* main_author_min = field path t "main_author_min" int d.main_author_min in
  let* derived_domains = field path t "derived_domains" (list derived_domain_of) [] in
  let* campaign_owner_prefixes = field path t "campaign_owner_prefixes" strings [] in
  let* campaign_exempt_names = field path t "campaign_exempt_names" strings [] in
  let* domains = field path t "domains" (some strings) None in
  Ok
    {
      mirror_name_regex;
      mirror_exempt_owners;
      major_contributor_owners;
      major_contributor_min_prs;
      org_member_owners;
      main_author_owners;
      main_author_min;
      derived_domains;
      campaign_owner_prefixes;
      campaign_exempt_names;
      domains;
    }

let at_most_one kind label sources =
  if List.length (List.filter (fun s -> s.kind = kind) sources) > 1 then
    Error (Printf.sprintf "at most one %s source is supported" label)
  else Ok ()

let of_toml = function
  | Otoml.TomlTable top ->
      let* () =
        check_keys "sources.toml" [ "owner"; "render"; "source"; "repo"; "classify" ] top
      in
      let* owner = field "" top "owner" owner_of empty_owner in
      let* render = field "" top "render" render_of empty_render in
      let* sources = field "" top "source" (list source_of) [] in
      let* repos = field "" top "repo" (list repo_of) [] in
      let* classify = field "" top "classify" classify_of empty_classify in
      let* () = at_most_one Github_user "github-user" sources in
      let* () = at_most_one Gitlab_user "gitlab-user" sources in
      let* () = at_most_one Github_search "github-search" sources in
      Ok { owner; render; sources; repos; classify }
  | _ -> Error "sources.toml: expected a table"

(* Error paths built by [field] start with "." at top level. *)
let tidy e =
  if String.length e > 0 && e.[0] = '.' then String.sub e 1 (String.length e - 1) else e

let of_string s =
  match Otoml.Parser.from_string_result s with
  | Error e -> Error ("sources.toml: " ^ e)
  | Ok toml -> Result.map_error tidy (of_toml toml)

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error _ -> Error (Printf.sprintf "cannot read %s" path)
  | s -> Result.map_error (fun e -> path ^ ": " ^ e) (of_string s)

let load_dir dir =
  let path = Filename.concat dir file_name in
  if Sys.file_exists path then load path else Ok empty

(* ------------------------------------------------------------ names *)

let repo_name target =
  let t =
    if Filename.check_suffix target ".git" then Filename.chop_suffix target ".git"
    else target
  in
  let t =
    if String.length t > 1 && t.[String.length t - 1] = '/' then
      String.sub t 0 (String.length t - 1)
    else t
  in
  Filename.basename t

(* The owner path segments and the name of a key ([host:a/b/name]). *)
let key_parts key =
  match String.index_opt key ':' with
  | None -> []
  | Some i ->
      String.split_on_char '/' (String.sub key (i + 1) (String.length key - i - 1))
      |> List.filter (( <> ) "")

(* A [[repo]] entry's names: its target's last segment, plus the owners and name of its
   keys when confidential (a public library's owner is no private name). *)
let repo_names (r : repo) =
  let confidential =
    r.campaign || r.campaign_prefix || r.confidential <> Not_confidential
  in
  repo_name r.target
  ::
  (if confidential then
     List.concat_map key_parts
       (Option.to_list r.key @ Option.to_list (Remote.norm_remote r.target))
   else [])

let names t =
  let c = t.classify in
  let source_names =
    List.filter_map
      (fun s -> if s.kind = Local_root || s.name = "" then None else Some s.name)
      t.sources
  in
  List.concat
    [
      (if t.owner.display_name = "" then [] else [ t.owner.display_name ]);
      t.owner.gitlab_me_names;
      t.owner.gitlab_email_fragments;
      source_names;
      List.concat_map repo_names t.repos;
      c.mirror_exempt_owners;
      c.major_contributor_owners;
      c.org_member_owners;
      c.main_author_owners;
      List.map (fun (d : derived_domain) -> d.owner) c.derived_domains;
      c.campaign_owner_prefixes;
      c.campaign_exempt_names;
    ]
  |> List.filter (fun s -> String.trim s <> "")
  |> List.sort_uniq compare

let expand_home ~home p =
  if p = "~" then home
  else if String.length p >= 2 && String.sub p 0 2 = "~/" then
    Filename.concat home (String.sub p 2 (String.length p - 2))
  else p
