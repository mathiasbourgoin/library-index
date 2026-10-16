(** Copies of a library and the maintained one (spec US-10.1, FR-086, US-11
    [maintained_only], US-12 [copies] via {!Copies_view}).

    Two rows are direct copies when they have the same name (case-insensitive) on
    different hosts (a mirror or a [local:] copy), or when one is the fork of the other
    (the parent being in the index). The copies of a row R are the other rows of its
    connected component over these links. Whitehat rows are never copies.

    R's maintained copy (its canonical row) is, in this order:
    + the row named by a curated [maintained_side] of R or of one of its copies: an index
      key, a host ([github], [gitlab], [local]: the same-name copy on that host),
      [upstream]/[parent] (the fork parent) or [fork]/[his]/[this]/[self]/[mine]/[here]
      (the row carrying the field);
    + among R and its copies that share R's name, when there is at least one: the most
      recent ([last_commit], else [pushed_at]) non-archived row (archived rows only when
      all are archived); ties go to GitHub, then GitLab, then local, then the key;
    + R's fork parent, when R is a fork and the parent is in the index and not archived;
    + R itself. *)

type row = {
  id : int;
  key : string;
  host : string;
  owner : string;
  name : string;
  full_name : string;
  archived : bool;
  fork : bool;
  parent : string option;  (** Parent [owner/name] of a fork. *)
  recency : string;  (** [last_commit], else [pushed_at], else [""]. *)
  maintained_side : string option;
}

type t
(** An index of every non-whitehat row, loaded once per call. *)

val load : Db.t -> t
(** [load db] reads every non-whitehat row. *)

val find : t -> string -> row option
(** [find idx key] is the row of [key]. *)

val group : t -> string -> row list
(** [group idx key] is the row of [key] followed by its copies sorted by key ([[]] for an
    unknown key): its connected component. *)

val copies : t -> string -> row list
(** [copies idx key] is the copies of [key] (not [key] itself), sorted by key. *)

val canonical : t -> string -> string option
(** [canonical idx key] is the key of the maintained copy (possibly [key] itself); [None]
    for an unknown key. *)

val maintained_copy : t -> string -> string option
(** [maintained_copy idx key] is {!canonical} when it differs from [key] (the row is a
    mirror, fork or stale copy), else [None]. *)

val host_rank : string -> int
(** [0] for GitHub, [1] for GitLab, [2] for local, [3] otherwise. *)
