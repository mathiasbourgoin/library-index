(** Whose library this is and how its repositories are classified: the account names,
    organisations, groups and rules every module reads (spec FR-032, FR-033, FR-101).

    An [Identity.t] is built once from the data directory's [sources.toml] ({!of_config});
    the code holds no real name. {!empty} (no configuration) lists nothing and matches
    nobody. Names are compared with ASCII lowercasing, as the prototype did. *)

type org = { org : string; own : bool; probe : bool }
(** A GitHub organisation, in listing order. *)

type registration = {
  key : string;  (** The registered repository's index key (lowercased). *)
  ownership : string option;  (** [own] | [participant] | [external] override. *)
  campaign : bool;
      (** [campaign = true], [campaign_prefix = true] or [confidential = "generic"]. *)
  exclude_from_page : bool;  (** [confidential = "exclude-from-page"]. *)
  deep : bool;  (** Deep analysis requested. *)
}
(** One [[[repo]]] entry whose key is known: its [key] member, else the key of a
    GitHub/GitLab [target] URL ({!Remote.norm_remote}). *)

type t = {
  gh_user : string option;  (** The [github-user] source, as written. *)
  gh_affiliation : bool;  (** A [github-affiliation] source is declared. *)
  gh_orgs : org list;  (** [github-org] sources in file order. *)
  gh_collabs : string list;  (** [github-collab] names, as written. *)
  gh_memberships : bool;  (** A [github-memberships] source is declared. *)
  gh_search_author : string option;
      (** The [github-search] author: its name, else {!gh_user}; [None] without such a
          source. *)
  gl_user : string option;  (** The [gitlab-user] source, as written. *)
  gl_groups : (string * bool) list;  (** [gitlab-group] names and [include_subgroups]. *)
  local_roots : string list;  (** [local-root] paths, as written ([~] not expanded). *)
  me_names : string list;  (** Lowercased GitLab contributor names that are the owner. *)
  me_email_parts : string list;
      (** Lowercased fragments: a contributor e-mail containing one is the owner's. *)
  author_regex : string option;
      (** The [git rev-list --regexp-ignore-case --author] pattern; [None] when unset. *)
  mirror_name : Re.re option;  (** Compiled [mirror_name_regex] (lowercased names). *)
  mirror_exempt_owners : string list;  (** Lowercased. *)
  major_contributor_owners : string list;
  major_contributor_min_prs : int;
  org_member_owners : string list;
  main_author_owners : string list;
  main_author_min : int;
  derived_domains : Config.derived_domain list;
  campaign_prefixes : string list;
      (** Lowercased owner prefixes of campaign repositories: the [[classify]] list then
          every non-local-root source flagged [campaign_prefix] or any [confidential]
          level (its name is then an owner prefix; [exclude-from-page] on a source is
          enforced as [generic]; {!Config} refuses either on a source naming no owner). *)
  campaign_exempt_names : string list;  (** Lowercased. *)
  domains : string list;
      (** The closed domain vocabulary in rendering order, always ending with ["Other"].
      *)
  registrations : registration list;  (** Registered repositories, in file order. *)
  confidential_roots : string list;
      (** [local-root] sources flagged [campaign_prefix] or any [confidential] level, as
          written: every repository with a checkout under one is a campaign repository
          ({!Local_keys}). *)
}

val empty : t
(** The identity of an empty configuration. *)

val of_config : Config.t -> t
(** [of_config c] derives the identity from a parsed [sources.toml]. *)

val for_source : t -> Config.source -> t
(** [for_source t src] is [t] restricted to listing [src] alone: the listing fields
    ([gh_user] … [local_roots]) are those of a configuration declaring only [src], and
    every other field (author, contributor names, classification rules, campaign prefixes
    and exemptions of the whole configuration, registrations) is [t]'s, so that a row
    listed through one source is classified and kept confidential exactly as a full
    refresh would (spec AM-17, FR-071, FR-104). A [github-search] source without a name
    searches as [t]'s GitHub user. *)

val own_orgs : t -> string list
(** [own_orgs t] is the organisations flagged [own], in order. *)

val listed_orgs : t -> string list
(** [listed_orgs t] is the non-probe organisations, in order (the rendered sources line).
*)

val is_user : t -> string -> bool
(** [is_user t owner] is [true] when the lowercased [owner] is the GitHub or GitLab user.
*)

val is_collab : t -> string -> bool
(** [is_collab t owner] is [true] when the lowercased [owner] is a collaborator. *)

val registration : t -> string -> registration option
(** [registration t key] is the registration of [key] (normalised), if any. *)

val is_registered : t -> string -> bool
(** [is_registered t key] is [true] when [key] has a [[[repo]]] entry. *)

val is_registered_campaign : t -> string -> bool
(** [is_registered_campaign t key] is [true] when [key]'s registration makes it a campaign
    repository ([campaign = true] or [confidential = "generic"]). *)
