(** Browsing the index with filters (spec US-3.3, FR-021, EC-9, AM-10). Named [Listing] so
    the library never shadows [Stdlib.List]. *)

val default_limit : int
(** 100. *)

val max_limit : int
(** 1000. *)

val list :
  Ctx.t -> filters:Filters.t -> limit:int option -> offset:int option -> Yojson.Safe.t
(** [list ctx ~filters ~limit ~offset] is
    [{"total", "limit", "offset", "count", "repos": [...]}] with rows sorted by key.
    [total] counts every row passing the filters (the effective status is computed with
    the injected clock, after SQL); [limit] is clamped to [1..1000], [offset] to [≥ 0].
    Each row has
    [key, host, owner, full_name, language, tier, status, domain (effective), private,
     what (fallback to synopsis/description), url, pushed_at, fetched, missing_since].
    ["export_dirty": true] is added while the export is dirty. *)
