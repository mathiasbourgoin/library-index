(** The one campaign decision of every write path (spec "Minimal-freeze", INV-D, FR-131;
    AM-17, FR-051, FR-071, FR-124). It is the write-side twin of {!Eligibility}: every
    predicate that can make a row a campaign row (owner prefix, a registration flagged
    campaign, the stored sticky flag, the remotes of the repository's local checkouts, a
    checkout whose remotes are unreadable or that lies under a confidential local root) is
    composed here and only here; [checks/single-campaign-decision.js] fails when another
    module composes them.

    Marking a row campaign takes effect at once (INV-C): its README, synopsis, documents,
    API items and auto pre-filled [what]/[interest] are dropped, its description withheld
    (unless its name is exempt) and every stored gap line naming it is masked. The flag is
    sticky: no write clears it. *)

val key_owner : string -> string option
(** [key_owner "gitlab:group/x"] is [Some "group"], the owner path of a remote key; [None]
    for a key without one ([local:x]). *)

val of_owner : id:Identity.t -> string -> bool
(** [of_owner ~id owner] is the owner rule alone: [owner] starts with a configured
    campaign prefix (case-insensitive). Used for rows that carry nothing but an owner (a
    curated stub); a later write applies {!decide}. *)

val decide :
  id:Identity.t ->
  call:Fetch.call ->
  local:Local_keys.t ->
  ?stored:Yojson.Safe.t ->
  ?checkout:Local_scan.checkout ->
  Yojson.Safe.t ->
  bool
(** [decide ~id ~call ~local ?stored ?checkout row] is the campaign decision for the
    raw-shape [row] (and its [stored] counterpart): the row's own rule (its [campaign]
    flag, owner prefix, campaign registration of its key), the stored flag, and the
    checkouts of the repository: [checkout] (a registration target; its remotes, and
    whether they could not be read), the row's [checkout_remote_keys], every checkout
    [local] ties to it, and {!Local_keys.forced_row}. Fail closed. *)

val of_raw_cache :
  id:Identity.t ->
  Db.t ->
  local:Yojson.Safe.t list ->
  Yojson.Safe.t list ->
  Yojson.Safe.t list
(** [of_raw_cache ~id db ~local repos] is [repos] (rows of [cache/raw.json]) each with its
    [campaign] member decided: the row's rule, the stored sticky flag, and the remotes of
    the cached checkouts [local] at its [local_paths]. *)

val persisted : Db.t -> Yojson.Safe.t -> bool
(** [persisted db row] is the flag an upsert of [row] stores: [row]'s decided flag, or the
    stored flag of its key (sticky). *)

val mask_gaps : Db.t -> string list -> string list
(** [mask_gaps db lines] replaces, in each line, every whole-token occurrence of a
    campaign row's key, full name or name with ["a campaign repository"]. Every stored gap
    line goes through it. *)

val gap_name : Db.t -> id:Identity.t -> string -> string
(** [gap_name db ~id key] is how a gap line names [key]: ["a campaign repository"] when
    the stored row or its registration is campaign, else [key]. *)

val clear_auto_prefill : Db.t -> string -> unit
(** [clear_auto_prefill db key] clears [what] and [interest] when their provenance is
    [auto] (a pre-fill may quote the description or README); a value a person wrote stays.
*)

val stored_campaign_row : Db.t -> string -> unit
(** [stored_campaign_row db key] completes an upsert that stored [key] as a campaign row:
    {!clear_auto_prefill}, and every stored gap line naming it (written before the row
    existed or was campaign) masked. Inside the caller's transaction. *)

val mark : Db.t -> Identity.t -> string -> unit
(** [mark db id key] makes the stored row [key] a campaign row now, inside the caller's
    transaction: flag set, README and synopsis dropped, description withheld unless the
    name is exempt, auto pre-fill cleared, documents and API items of ineligible rows
    purged, stored gaps masked. No-op for an unknown key. *)

val mark_atomic : Db.t -> Identity.t -> string -> (unit, 'e) result
(** [mark_atomic db id key] is {!mark} in its own transaction ({!Db.with_txn}). *)

val reclassify :
  Db.t -> id:Identity.t -> call:Fetch.call -> local:Local_keys.t -> string list
(** [reclassify db ~id ~call ~local] applies {!decide} to every stored non-campaign,
    non-whitehat row and {!mark}s each one that is now campaign, inside the caller's
    transaction; the keys marked. Run after every configuration change (INV-C: immediate
    effect, also for rows the change did not list). *)

val reclassify_now :
  Ctx.t ->
  call:Fetch.call ->
  ?local:Local_keys.t Lazy.t ->
  unit ->
  (string list, string) result
(** [reclassify_now ctx ~call ?local ()] is {!reclassify} with the configured checkouts
    ({!Local_keys.configured} unless [local]), in one transaction, after
    {!Local_keys.check} (an unreadable checkout is an error, nothing written). *)

val apply_now :
  Ctx.t ->
  call:Fetch.call ->
  ?local:Local_keys.t Lazy.t ->
  string ->
  (bool, string) result
(** [apply_now ctx ~call ?local key] decides the stored row [key] now and {!mark_atomic}s
    it when campaign; [Ok true] when it is (now) campaign by the rule. *)
