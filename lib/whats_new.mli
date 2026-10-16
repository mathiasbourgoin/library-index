(** The [whats-new] tool: what changed in the index since a date (spec US-12, FR-091,
    AC-28). Read-only; dates are compared as instants, whatever their offset. *)

val default_limit : int
(** [20] repositories. *)

val max_limit : int
(** [100] repositories. *)

val parse_since : string -> (Ptime.t, string) result
(** [parse_since s] reads an RFC 3339 time or a [YYYY-MM-DD] date (00:00:00 UTC);
    otherwise ["invalid since '<s>' (expected RFC 3339 or YYYY-MM-DD)"]. *)

val whats_new :
  Ctx.t -> ?ownership:string -> since:Ptime.t -> limit:int option -> unit -> Yojson.Safe.t
(** [whats_new ctx ?ownership ~since ~limit ()] is [{since, total, count, repos}]: every
    non-whitehat (and, with [ownership], only of that canonical {!Vocab.ownerships} value)
    repository whose [pushed_at] or [last_commit] is on or after [since], or which has a
    document whose [fetched_at] (its last content change) is on or after [since]. Each
    entry is [{key, name, what, uri, latest, changes}], where [latest] is the most recent
    of those instants (RFC 3339, UTC) and [changes] has [metadata]
    ([{pushed_at, last_commit}], those on or after [since]) and/or [docs]
    ([{count, paths}], at most 10 paths, sorted). Entries are ordered newest [latest]
    first, then by key; [total] counts every match, [count] those returned ([limit]
    defaults to {!default_limit}, clamped to [1..max_limit]). *)
