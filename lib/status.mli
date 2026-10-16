(** Effective repository status with an injected clock (spec C-12, FR-022).

    Computed status ports the prototype's [status_of]: [archived] wins; otherwise the age
    in whole UTC days (floored) of [last_commit], else [pushed_at], gives [active] (≤ 90),
    [dormant] (≤ 730) or [stale]; [unknown] when neither date is present or parseable. The
    effective status is the curated [status_override] when set. *)

val parse_time : string -> Ptime.t option
(** [parse_time s] parses RFC 3339 (fractional seconds and offsets allowed) or a bare
    [YYYY-MM-DD] date, read as 00:00:00 UTC. *)

val age_days : now:Ptime.t -> Ptime.t -> int
(** [age_days ~now t] is [floor ((now - t) / 86400 s)], negative for future dates. *)

val computed :
  now:Ptime.t ->
  archived:bool ->
  last_commit:string option ->
  pushed_at:string option ->
  string
(** [computed] is the prototype status (one of {!Vocab.statuses}). Empty strings count as
    absent, as in Python's [or]. *)

val effective :
  now:Ptime.t ->
  override:string option ->
  archived:bool ->
  last_commit:string option ->
  pushed_at:string option ->
  string
(** [effective] is [override] when it is set and non-empty, else {!computed}. *)
