(** The data directory's [sources.toml]: who is indexed, which sources are listed, the
    classification rules and the rendering texts (spec FR-100..FR-103, "Sources as
    configuration").

    Nothing about a real person, organisation or repository lives in the code: every such
    value comes from this file, and {!empty} (the configuration of a missing file) holds
    none. The file is parsed strictly: an unknown table, key or source kind, or a value of
    the wrong type, is an error naming it.

    Layout (every table and key is optional):
    {[
      [owner]
      display_name = "Example Owner"
      author_regex = "example"          # git rev-list --regexp-ignore-case --author
      gitlab_me_names = ["example owner"]
      gitlab_email_fragments = ["example"]

      [render]
      title = "Example Owner — repository & library index"
      notes_dir = "~/notes"             # the dated HTML page; none when absent
      index_md = "~/index/INDEX.md"     # default: <data dir>/INDEX.md
      how_to = ["- first bullet", "- last bullet\n"]
      inherited_examples = "old upstream forks"
      contributes_column = "Owner contributes"
      whitehat_where = "GitLab group `example-campaigns`"
      fixed_gaps = ["a gap line rendered after the stored ones"]

      [[source]]
      kind = "github-org"               # see {!kind}
      name = "example-org"
      own = true
      probe = false
      include_subgroups = true
      confidential = "none"             # generic | exclude-from-page | none
      campaign_prefix = false

      [[repo]]
      target = "https://github.com/example-org/tool"
      key = "github:example-org/tool"   # written by registration
      ownership = "external"            # own | participant | external (else inferred)
      confidential = "none"
      campaign = false
      deep = false

      [classify]
      mirror_name_regex = "upstream|vendor"
      mirror_exempt_owners = ["example-upstream"]
      major_contributor_owners = ["example-upstream"]
      major_contributor_min_prs = 20
      org_member_owners = ["example-org"]
      main_author_owners = ["example-org"]
      main_author_min = 10
      derived_domains = [{ owner = "example-campaigns", prefix = true, domain = "Security research" }]
      campaign_owner_prefixes = ["example-audits"]
      campaign_exempt_names = ["shared-tools"]
      domains = ["Libraries", "Other"]
    ]} *)

type kind =
  | Github_user  (** A GitHub user's repositories ([users/<name>/repos?type=all]). *)
  | Github_org  (** A GitHub organisation's repositories. *)
  | Github_affiliation
      (** Every repository visible to the authenticated GitHub user (no name). *)
  | Github_collab
      (** A collaborator whose own repositories form the [collab] tier (section 5). *)
  | Github_memberships  (** The authenticated user's organisation memberships. *)
  | Github_search
      (** Commit and PR search by an author (the name, else the GitHub user). *)
  | Gitlab_user  (** A GitLab user: owned, contributed and MR projects. *)
  | Gitlab_group  (** A GitLab group's projects. *)
  | Local_root  (** A directory scanned for local git checkouts. *)

val kind_names : (string * kind) list
(** The spelling of each kind in [sources.toml], e.g. [("github-org", Github_org)]. *)

(** Confidentiality of a source or repo entry. For a [[[repo]]] entry, [Generic] makes the
    repository a campaign row (described generically, no README or documents) and
    [Exclude_from_page] keeps it out of the rendered views; for a [[[source]]] the
    campaign rules key on [campaign_prefix] and [[classify].campaign_owner_prefixes]. *)
type confidential =
  | Generic  (** Described generically. *)
  | Exclude_from_page  (** Kept out of the rendered page. *)
  | Not_confidential  (** ["none"]. *)

val confidential_names : (string * confidential) list
(** [generic], [exclude-from-page], [none]. *)

val confidential_name : confidential -> string
(** The spelling of a confidentiality in [sources.toml]. *)

(** Whose code a repository is (spec "Ownership kind"). *)
type ownership =
  | Own  (** The owner's own repository (role owner or main author). *)
  | Participant  (** Another role: contributor, org member, occasional, … *)
  | External  (** No participation evidence: a third-party library registered for use. *)

val ownership_names : (string * ownership) list
(** [own], [participant], [external]. *)

val ownership_name : ownership -> string
(** The spelling of an ownership in [sources.toml] and the DB. *)

val kind_name : kind -> string
(** The spelling of a source kind, e.g. ["github-org"]. *)

type source = {
  kind : kind;
  name : string;  (** User, organisation, group or path; [""] for nameless kinds. *)
  host : string option;
  own : bool;  (** One of the owner's organisations (contributor targets, rendering). *)
  probe : bool;  (** Requested but possibly absent: not listed in the sources line. *)
  include_subgroups : bool;  (** GitLab groups: list subgroups (default [true]). *)
  confidential : confidential;
  campaign_prefix : bool;  (** The name is a campaign owner prefix. *)
}
(** One [[[source]]] entry. *)

type repo = {
  target : string;  (** Forge URL or local path, as registered. *)
  key : string option;
      (** The index key registration resolved [target] to ([github:o/r], [local:dir]);
          [None] in a hand-written entry, where a forge URL still yields its key. *)
  ownership : ownership option;  (** Overrides the inferred ownership. *)
  own : bool;
  confidential : confidential;
  campaign : bool;  (** A campaign repository (withheld description, no documents). *)
  campaign_prefix : bool;
  deep : bool;  (** Deep analysis requested: registration runs {!Analysis.analyze}. *)
}
(** One [[[repo]]] entry: an explicitly registered repository ([register]). *)

type owner = {
  display_name : string;
  author_regex : string;  (** [""]: local commits are not attributed. *)
  gitlab_me_names : string list;
  gitlab_email_fragments : string list;
}
(** The indexed person. *)

type derived_domain = {
  owner : string;
  prefix : bool;  (** Match owners starting with [owner] (else equal), ignoring case. *)
  domain : string;
}
(** A computed-domain rule: repositories of a matching owner get [domain]. *)

type classify = {
  mirror_name_regex : string;  (** [""]: no name marks a mirror. *)
  mirror_exempt_owners : string list;
  major_contributor_owners : string list;
  major_contributor_min_prs : int;
  org_member_owners : string list;
  main_author_owners : string list;
  main_author_min : int;
  derived_domains : derived_domain list;
  campaign_owner_prefixes : string list;
  campaign_exempt_names : string list;
  domains : string list option;  (** [None]: {!Vocab.default_domains}. *)
}
(** The classification rules (formerly the prototype's constants). *)

type render = {
  title : string;
  notes_dir : string option;
  index_md : string option;
  how_to : string list option;  (** [None]: the generic bullets. *)
  inherited_examples : string;
  contributes_column : string;
  whitehat_where : string;
  fixed_gaps : string list option;  (** [None]: the generic GitLab-language line. *)
}
(** Texts and paths of the rendered views. *)

type t = {
  owner : owner;
  render : render;
  sources : source list;  (** In file order. *)
  repos : repo list;
  classify : classify;
}

val empty : t
(** The configuration of a missing file: no name, no source, empty rules, generic texts.
*)

val file_name : string
(** ["sources.toml"]. *)

val of_string : string -> (t, string) result
(** [of_string s] parses a [sources.toml] document. *)

val load : string -> (t, string) result
(** [load path] reads and parses [path]. *)

val load_dir : string -> (t, string) result
(** [load_dir dir] is {!load} of [dir/sources.toml], or [Ok empty] when that file does not
    exist. *)

val names : t -> string list
(** [names t] is every user, organisation, group, owner and repository name the file
    declares (not paths, regexes or texts), deduplicated, for the publishability guard
    (FR-100): for a [[[repo]]] entry, its target's last segment and, when the entry is
    confidential (campaign, [campaign_prefix] or any [confidential] level), every owner
    segment and name of its [key] and of its URL target's key. *)

val expand_home : home:string -> string -> string
(** [expand_home ~home p] replaces a leading [~] or [~/] of [p] by [home]. *)
