(** Curation writes: [annotate] and [record_usage] (spec US-5, FR-040, FR-041, FR-042,
    C-22, C-24, EC-17..EC-20, AM-10).

    Every check runs before any write; a validation or resolution error writes nothing. A
    successful write commits in one transaction, then rewrites the exports
    ({!Export.after_write}); an export failure returns ["DB updated, export failed: …"]
    and leaves [export_dirty] set. All stored text goes through {!Redact.text}. *)

val annotatable : string list
(** [what, interest, domain, consumers, status_override, maintained_side, notes, rules].
*)

val whitehat_annotatable : string list
(** [what, interest]: the only curated fields [whitehat_tools.json] can carry ([interest]
    is exported as [reuse]). *)

val max_chars : int
(** 4000: maximum length in code points of any annotated string or usage text. *)

val annotate :
  Ctx.t ->
  repo:string ->
  fields:(string * Yojson.Safe.t) list ->
  (Yojson.Safe.t, string) result
(** [annotate ctx ~repo ~fields] sets each named field; JSON [null] clears it. Strings for
    [what], [interest], [maintained_side], [notes], [rules]; [domain] ∈ the configured
    domains ({!Identity.t.domains}); [status_override] ∈ {!Vocab.statuses}; [consumers] an
    array of non-empty strings (duplicates dropped) that replaces the stored list. Errors:
    ["no fields to annotate"], ["field '<f>' cannot be annotated; allowed fields: …"],
    invalid values listing the allowed ones, over-long text, unknown/ambiguous repo.
    Returns [{"key", "updated": [fields], "exported": true}]. *)

val record_usage :
  Ctx.t ->
  repo:string ->
  consumer:string ->
  note:string option ->
  (Yojson.Safe.t, string) result
(** [record_usage ctx ~repo ~consumer ~note] appends a usage row stamped with the injected
    clock (a repeated call appends again, EC-17) and adds [consumer] to [consumers] when
    absent (set semantics). Not available on whitehat rows. Returns
    [{"key", "consumer", "consumers", "usage_count", "exported": true}]. *)
