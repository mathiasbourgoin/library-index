(** Provenance of curated fields (spec FR-112): the [field_meta] table.

    [Auto] marks a value pre-filled deterministically ({!Prefill}, FR-111): a refresh or a
    re-registration may overwrite it, and the [curated.json] export omits it. [Curated]
    marks a value a person wrote ([annotate], [record_usage], an imported [curated.json]);
    nothing automatic ever overwrites it. A field without a row is treated as curated.
    Callers run these inside their own transaction. *)

type provenance = Auto | Curated

val provenance_name : provenance -> string
(** ["auto"] or ["curated"]. *)

val set : Db.t -> repo_id:int -> field:string -> provenance -> now:string -> unit
(** [set db ~repo_id ~field p ~now] upserts the provenance of one field. *)

val mark_curated : Db.t -> repo_id:int -> fields:string list -> now:string -> unit
(** [mark_curated db ~repo_id ~fields ~now] sets every field of [fields] to [Curated]. *)

val get : Db.t -> repo_id:int -> field:string -> provenance option
(** [get db ~repo_id ~field] is the stored provenance, [None] without a row. *)

val of_repo : Db.t -> repo_id:int -> (string * string * string) list
(** [of_repo db ~repo_id] is every [(field, provenance, updated_at)] of the repository, by
    field name. *)

val auto_fields : Db.t -> repo_id:int -> field:string -> bool
(** [auto_fields db] snapshots the [Auto] fields; the returned predicate tells whether a
    field of a repository is [Auto] (the export filter). *)
