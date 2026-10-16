type org = { org : string; own : bool; probe : bool }

type registration = {
  key : string;
  ownership : string option;
  campaign : bool;
  exclude_from_page : bool;
  deep : bool;
}

type t = {
  gh_user : string option;
  gh_affiliation : bool;
  gh_orgs : org list;
  gh_collabs : string list;
  gh_memberships : bool;
  gh_search_author : string option;
  gl_user : string option;
  gl_groups : (string * bool) list;
  local_roots : string list;
  me_names : string list;
  me_email_parts : string list;
  author_regex : string option;
  mirror_name : Re.re option;
  mirror_exempt_owners : string list;
  major_contributor_owners : string list;
  major_contributor_min_prs : int;
  org_member_owners : string list;
  main_author_owners : string list;
  main_author_min : int;
  derived_domains : Config.derived_domain list;
  campaign_prefixes : string list;
  campaign_exempt_names : string list;
  domains : string list;
  registrations : registration list;
  confidential_roots : string list;
}

let lower = String.lowercase_ascii
let non_empty s = if String.trim s = "" then None else Some s

let domains_of = function
  | None -> Vocab.default_domains
  | Some l -> List.filter (fun d -> d <> "" && d <> "Other") l @ [ "Other" ]

let campaign_prefixes (c : Config.t) =
  c.classify.campaign_owner_prefixes
  @ List.filter_map
      (fun (s : Config.source) ->
        if
          s.kind <> Config.Local_root
          && (s.campaign_prefix || s.confidential <> Config.Not_confidential)
        then Some s.name
        else None)
      c.sources
  |> List.map lower
  |> List.filter (( <> ) "")

let registration_key (r : Config.repo) =
  match r.key with
  | Some k when String.trim k <> "" -> Some (Key.normalize k)
  | _ -> Remote.norm_remote r.target

let registrations (c : Config.t) =
  List.filter_map
    (fun (r : Config.repo) ->
      Option.map
        (fun key ->
          {
            key;
            ownership = Option.map Config.ownership_name r.ownership;
            campaign = r.campaign || r.campaign_prefix || r.confidential = Config.Generic;
            exclude_from_page = r.confidential = Config.Exclude_from_page;
            deep = r.deep;
          })
        (registration_key r))
    c.repos

let of_config (c : Config.t) =
  let of_kind k = List.filter (fun (s : Config.source) -> s.kind = k) c.sources in
  let names k = List.map (fun (s : Config.source) -> s.name) (of_kind k) in
  let first k = match names k with n :: _ -> non_empty n | [] -> None in
  let gh_user = first Github_user in
  let cl = c.classify in
  {
    gh_user;
    gh_affiliation = of_kind Github_affiliation <> [];
    gh_orgs =
      List.map
        (fun (s : Config.source) -> { org = s.name; own = s.own; probe = s.probe })
        (of_kind Github_org);
    gh_collabs = names Github_collab;
    gh_memberships = of_kind Github_memberships <> [];
    gh_search_author =
      (match of_kind Github_search with
      | [] -> None
      | s :: _ -> ( match non_empty s.name with Some n -> Some n | None -> gh_user));
    gl_user = first Gitlab_user;
    gl_groups =
      List.map
        (fun (s : Config.source) -> (s.name, s.include_subgroups))
        (of_kind Gitlab_group);
    local_roots = names Local_root;
    me_names = List.map lower c.owner.gitlab_me_names;
    me_email_parts =
      List.filter (( <> ) "") (List.map lower c.owner.gitlab_email_fragments);
    author_regex = non_empty c.owner.author_regex;
    mirror_name =
      Option.map (fun r -> Re.compile (Re.Perl.re r)) (non_empty cl.mirror_name_regex);
    mirror_exempt_owners = List.map lower cl.mirror_exempt_owners;
    major_contributor_owners = cl.major_contributor_owners;
    major_contributor_min_prs = cl.major_contributor_min_prs;
    org_member_owners = cl.org_member_owners;
    main_author_owners = cl.main_author_owners;
    main_author_min = cl.main_author_min;
    derived_domains = cl.derived_domains;
    campaign_prefixes = campaign_prefixes c;
    campaign_exempt_names = List.map lower cl.campaign_exempt_names;
    domains = domains_of cl.domains;
    registrations = registrations c;
    confidential_roots =
      List.filter_map
        (fun (s : Config.source) ->
          if
            s.kind = Config.Local_root
            && (s.campaign_prefix || s.confidential <> Config.Not_confidential)
          then Some s.name
          else None)
        c.sources;
  }

let empty = of_config Config.empty

let for_source t (src : Config.source) =
  let only = of_config { Config.empty with sources = [ src ] } in
  {
    t with
    gh_user = only.gh_user;
    gh_affiliation = only.gh_affiliation;
    gh_orgs = only.gh_orgs;
    gh_collabs = only.gh_collabs;
    gh_memberships = only.gh_memberships;
    gh_search_author =
      (match (src.kind, only.gh_search_author) with
      | Github_search, None -> t.gh_user
      | _, a -> a);
    gl_user = only.gl_user;
    gl_groups = only.gl_groups;
    local_roots = only.local_roots;
  }

let own_orgs t = List.filter_map (fun o -> if o.own then Some o.org else None) t.gh_orgs

let listed_orgs t =
  List.filter_map (fun o -> if o.probe then None else Some o.org) t.gh_orgs

let is_user t owner =
  let o = lower owner in
  List.exists (fun u -> Option.map lower u = Some o) [ t.gh_user; t.gl_user ]

let is_collab t owner = List.mem (lower owner) (List.map lower t.gh_collabs)

let registration t key =
  let key = Key.normalize key in
  List.find_opt (fun r -> r.key = key) t.registrations

let is_registered t key = registration t key <> None

let is_registered_campaign t key =
  match registration t key with Some r -> r.campaign | None -> false
