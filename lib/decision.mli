(** The refresh decision (spec FR-031, US-4.1/4.2, C-15..C-17, EC-12): which per-repo
    fetches a refresh performs. Pure. *)

type t = {
  readme : bool;  (** Fetch the README and the synopsis. *)
  detail : bool;
      (** Fetch the other per-repo detail: last commit, contributors, languages, fork
          parent. *)
}

val max_detail_age_days : int
(** [30]: non-README detail older than this is refetched even without a push. *)

val decide :
  stored_pushed_at:string option ->
  listed_pushed_at:string option ->
  readme_stored:bool ->
  detail_age_days:int option ->
  full:bool ->
  t
(** [decide ~stored_pushed_at ~listed_pushed_at ~readme_stored ~detail_age_days ~full]:

    - the push is {e changed} when there is no stored value or when the stored and listed
      values differ in any direction (a stored value newer than the listed one counts as
      changed, EC-12);
    - [readme = full || changed || not readme_stored]. The README is never refetched on
      age alone;
    - [detail = readme || detail_age_days = None || detail_age_days >= 30].

    [readme_stored] means "a README fetch outcome is recorded" ([readme_fetched_at] set,
    including the outcome "this repo has no README"). Callers pass [true] for campaign
    repos, whose README is never fetched (FR-071). [detail_age_days = None] means detail
    was never fetched. *)

val age_days : now:Ptime.t -> string option -> int option
(** [age_days ~now ts] is the number of whole days from the timestamp [ts] (RFC 3339 or
    [YYYY-MM-DD]) to [now]; [None] when [ts] is absent or unparsable. Negative ages count
    as 0. *)
