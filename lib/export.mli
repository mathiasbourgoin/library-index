(** Writing the committed exports [curated.json] and [whitehat_tools.json] (spec C-22,
    C-23, FR-042, AM-5, AM-10).

    Under one exclusive [lockf] on [<dir>/.export.lock] ({!Lockfile}): the DB snapshot is
    read, each file is written to a temporary file in [dir] and renamed over the target,
    and [meta.export_dirty] is updated. So the last export to take the lock always writes
    the newest committed state. A failed export after a DB write sets
    [meta.export_dirty = 1]; the next curated write or export rewrites both files and
    clears it. *)

val lock_file : string
(** [".export.lock"], in the export directory. *)

val curated_file : string
(** ["curated.json"]. *)

val whitehat_file : string
(** ["whitehat_tools.json"]. *)

val write_files :
  Db.t -> dir:string -> curated:bool -> whitehat:bool -> (unit, string) result
(** [write_files db ~dir ~curated ~whitehat] writes the selected exports into [dir]. The
    error is a classified I/O message. It does not touch [export_dirty]. *)

val export : Ctx.t -> dir:string -> (unit, string) result
(** [export ctx ~dir] writes both files into [dir]. When [dir] is the context's export
    directory it also clears or sets [export_dirty] according to the outcome. *)

val after_write : Ctx.t -> whitehat_changed:bool -> (unit, string) result
(** [after_write ctx ~whitehat_changed] runs after a committed curated write: it rewrites
    [curated.json], plus [whitehat_tools.json] when a whitehat row changed or the export
    was dirty. On failure it sets [export_dirty] and returns
    ["DB updated, export failed: <reason>"]. *)

val retry_if_dirty : Ctx.t -> (unit, string) result
(** [retry_if_dirty ctx] re-exports both files when [export_dirty] is set (refresh calls
    it first), with the same failure handling as {!after_write}. *)
