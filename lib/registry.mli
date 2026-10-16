(** Registered repositories in the DB and in the full refresh (spec "Registration",
    "Ownership kind"). The [[[repo]]] entries of [sources.toml]
    ({!Identity.registrations}) are mirrored on their rows as [registered = 1],
    [ownership_override] and [exclude_from_page]. *)

val sync : Db.t -> Identity.t -> unit
(** [sync db id] makes the registration columns of every row match [id]: rows of
    registered keys get [registered = 1] and their overrides, every other row
    [registered = 0] and no override; every row a registration makes a campaign repository
    is marked at once ({!Campaign.mark}). Must run inside a transaction. *)

val missing_entries :
  Db.t ->
  id:Identity.t ->
  call:Fetch.call ->
  listed:string list ->
  Sources.entry list * string list
(** [missing_entries db ~id ~call ~listed] is, for every registered key that no source
    listed ([listed] keys) and that has a stored row, its current metadata as a listing
    entry with an empty [sources] list (it is listed by nothing), plus one classified gap
    line per registered key whose metadata could not be read (its row is kept as is),
    naming it through {!Campaign.gap_name} (a campaign row is never named). *)

val keys : Identity.t -> string list
(** [keys id] is every registered key. *)
