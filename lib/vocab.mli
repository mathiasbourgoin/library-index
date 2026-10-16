(** Closed vocabularies shared by filters, validation and rendering (spec C-13, C-24). *)

val default_domains : string list
(** The generic domain vocabulary in rendering order, used when [sources.toml] declares no
    [[classify].domains]; it ends with ["Other"]. The configured vocabulary is
    {!Identity.t.domains}. *)

val tiers : string list
(** [1, 2, fork, collab, inherited, whitehat]. *)

val statuses : string list
(** [active, dormant, stale, archived, unknown]. *)

val hosts : string list
(** [github, gitlab, local, whitehat]. *)

val curated_hosts : string list
(** Key prefixes allowed in [curated.json]: [github, gitlab, local]. *)

val canonical : string list -> string -> string option
(** [canonical allowed v] is the member of [allowed] equal to [v] ignoring ASCII case. *)

val invalid_value : what:string -> string -> string list -> string
(** [invalid_value ~what v allowed] is the uniform message
    ["invalid <what> '<v>'; allowed values: a, b, …"]. *)

val ownerships : string list
(** [own, participant, external] (spec "Ownership kind"). *)
