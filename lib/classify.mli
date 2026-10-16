(** Derived repository facts: exact ports of the prototype's pure classification and text
    helpers (spec FR-033, AC-10, AM-2, FR-071; [refresh.py] at b70f23d).

    Every function here is pure. String comparisons that Python performs with [str.lower]
    use ASCII lowercasing (all owners and names are ASCII); code-point slices use
    {!Utf8.take}. *)

(** {2 Inputs} *)

type contrib = {
  commits : int;  (** [contrib.commits], 0 when absent. *)
  rank : int option;  (** 1-based rank among contributors; [None] when absent or null. *)
  share : float;  (** Share of all commits, 0 when absent. *)
  n_contributors : int;  (** Number of contributors listed, 0 when absent. *)
}
(** Contributor statistics as stored in the [contrib] JSON column. *)

type facts = {
  host : string;
  owner : string;
  name : string;
  full_name : string;
  fork : bool;
  description : string;  (** [""] when absent. *)
  contrib : contrib option;
  search_commits : int;  (** GitHub commit-search hits for this repo (0 elsewhere). *)
  search_prs : int;  (** GitHub PR-search hits for this repo (0 elsewhere). *)
  mrs : int;  (** GitLab merge requests authored, 0 when absent. *)
  local_my_commits : int;
      (** His commits in the primary local checkout, 0 when absent. *)
  gl_contributed : bool;  (** Listed by GitLab [contributed_projects]. *)
}
(** Everything {!participation} and {!tier} read from a repository. *)

val contrib_of_json : Yojson.Safe.t -> contrib option
(** [contrib_of_json v] reads a [contrib] object ([commits], [rank], [share],
    [n_contributors], each optional); [None] for anything but an object. *)

val facts_of_raw : search_commits:int -> search_prs:int -> Yojson.Safe.t -> facts
(** [facts_of_raw ~search_commits ~search_prs row] reads a [cache/raw.json] repo row (the
    prototype's [gh_norm]/[gl_norm] shape plus enrichment fields). Missing fields take the
    prototype defaults. *)

(** {2 Classification} *)

val participation : id:Identity.t -> facts -> string option * string
(** [participation ~id f] is the prototype's [(role, evidence)]: role one of [owner],
    [main author], [contributor (major)], [contributor], [org member], [occasional],
    [inherited], or [None] with evidence [""]. Evidence strings are byte-identical to the
    prototype's, e.g. ["12 commits (rank 1/3, 80%)"], ["3 PRs/MRs"],
    ["GitLab contributed-project"] and the
    [" — historical commits carried by a fork/mirror"] suffix. The owner names and
    thresholds come from [id] (user, organisation, mirror and major-contributor rules). *)

val tier : id:Identity.t -> facts -> role:string option -> string
(** [tier ~id f ~role] is the prototype tier: [collab], [1], [fork], [inherited] or [2].
*)

val computed_domain : id:Identity.t -> owner:string -> string option
(** [computed_domain ~id ~owner] is the domain of the first [id.derived_domains] rule
    whose owner equals the lowercased [owner] (or prefixes it, for a prefix rule), else
    [None] (AM-2: written to [computed_domain], never to the curated [domain]). *)

type derived = {
  role : string option;
  evidence : string;
  tier : string;
  domain : string option;  (** The computed domain, see {!computed_domain}. *)
}
(** The derived columns stored on a repo row. *)

val derive : id:Identity.t -> facts -> derived
(** [derive ~id f] combines {!participation}, {!tier} and {!computed_domain}. *)

(** {2 Confidentiality} *)

val withheld_description : string
(** ["security-research campaign repo (details withheld)"]. *)

val campaign_redaction :
  id:Identity.t ->
  campaign:bool ->
  name:string ->
  description:string option ->
  synopsis:string option ->
  string option * string option
(** [campaign_redaction ~id ~campaign ~name ~description ~synopsis] applies the
    prototype's confidentiality rule (FR-071) to a row whose campaign flag is [campaign]:
    unless its lowercased name is one of [id.campaign_exempt_names], the description
    becomes {!withheld_description} and the synopsis [None]; other repos are returned
    unchanged. *)

(** {2 Rendering helpers} *)

val what_fallback :
  what:string option -> synopsis:string option -> description:string option -> string
(** [what_fallback] is the first non-empty of [what], [synopsis], [description], else
    ["—"]. *)

val fork_note :
  fork:bool ->
  parent:string option ->
  maintained_side:string option ->
  last_commit:string option ->
  pushed_at:string option ->
  parent_pushed_at:string option ->
  parent_archived:bool ->
  string option
(** [fork_note] is [None] for a non-fork, else ["fork of <parent|?>; maintained: <side>"],
    where side is the curated [maintained_side] or, when unset, ["fork (his)"] /
    ["upstream"] by comparing the fork's last commit (else push) with the parent's push as
    strings, overridden by ["fork (upstream archived)"]. *)

(** {2 Text helpers} *)

val first_para : string -> string option
(** [first_para md] is the prototype's README summary: the first blank-line-separated
    block that is at least 25 code points and does not start with [#], [<], [\[!], [!\[],
    [```], [|], [---] or [=], with [*], [_], backquotes and link syntax removed,
    whitespace collapsed, cut to 220 code points. *)

val norm_remote : string -> string option
(** [norm_remote url] maps a [git@], [ssh://git@] or [https://] GitHub/GitLab remote to
    its lowercased key ([github:o/r], [gitlab:g/sub/p]), dropping userinfo, a trailing
    [.git] and [/]; [None] for other URLs. *)

val language_of_files : string list -> string option
(** [language_of_files paths] is the prototype's [local_language] over [git ls-files]
    output: the most frequent language by extension, ignoring Markdown unless it is the
    only one; ties go to the language seen first. *)
