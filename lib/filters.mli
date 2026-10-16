(** Search/list filters with closed-domain validation (spec C-13, FR-011, FR-021).

    [tier], [status], [host], [ownership] and [domain] must belong to {!Vocab}; matching
    is case-insensitive and the canonical value is used. [owner] and [language] are free
    and compared case-insensitively. [private] is a boolean ([true]/[false] strings
    accepted). [status] is the effective status, applied after the SQL query with the
    injected clock (C-12). A JSON [null] value means "no filter". *)

type t = {
  host : string option;
  owner : string option;
  language : string option;
  tier : string option;
  status : string option;
  domain : string option;
  private_ : bool option;
  maintained_only : bool option;
      (** [Some true]: only rows that are their own maintained copy and not archived
          (US-11, {!Copies}). *)
  has_docs : bool option;  (** Rows with ([true]) or without ([false]) documents. *)
  ownership : string option;
      (** [own] | [participant] | [external] ({!Vocab.ownerships}). *)
}

val keys : string list
(** [host, owner, language, tier, status, domain, private, maintained_only, has_docs,
     ownership]. *)

val empty : t
(** No filter. *)

val of_assoc :
  ?domains:string list ->
  ?extra_keys:string list ->
  (string * Yojson.Safe.t) list ->
  (t, string) result
(** [of_assoc ?domains members] validates filter members; [domain] must belong to
    [domains] (default {!Vocab.default_domains}; callers pass the configured
    {!Identity.t.domains}). An unknown key yields
    ["unknown filter key '<k>'; allowed keys: …"] (listing [extra_keys] too, for callers
    whose arguments mix filters with other keys); an invalid value yields
    {!Vocab.invalid_value}'s message or ["filter '<k>' must be a …"]. *)

val where : alias:string -> t -> string * Sqlite3.Data.t list
(** [where ~alias f] is a SQL fragment of [" AND …"] conditions on table alias [alias]
    (empty when no SQL-side filter) and its parameters. [status] and [maintained_only] are
    not included ({!keep_status}, {!keep_maintained}). *)

val keep_status : t -> string -> bool
(** [keep_status f s] is [true] iff the effective status [s] passes [f.status]. *)

val maintained_active : t -> bool
(** [maintained_active f] is [true] iff [f.maintained_only = Some true]. *)

val keep_maintained : t -> archived:bool -> maintained_copy:string option -> bool
(** [keep_maintained f ~archived ~maintained_copy] is [true] when [maintained_only] is not
    requested, or when the row is not archived and has no other maintained copy. *)
