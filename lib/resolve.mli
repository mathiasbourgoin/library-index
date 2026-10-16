(** The single repository resolver used by get, annotate, record_usage and refresh (spec
    C-14, FR-020).

    Lookup order: exact key (case-insensitive), then [owner/name] against [full_name]
    (case-insensitive, only for inputs containing ['/']), then the bare [name]. The first
    step with any match decides; more than one match is an error listing the keys. *)

type t = { id : int; key : string }
(** A resolved repository row. *)

val resolve : Db.t -> string -> (t, string) result
(** [resolve db ident] resolves [ident]. Errors: ["repo must be a non-empty string"],
    ["unknown repo '<ident>'"] or
    ["ambiguous repo '<ident>': matches <k1>, <k2>, …; use a full key"]. *)
