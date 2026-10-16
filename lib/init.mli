(** [init] and [import] (spec FR-001, FR-003, C-4, EC-3). *)

val init :
  id:Identity.t ->
  db_path:string ->
  from_dir:string ->
  force:bool ->
  ?discard_unexported:bool ->
  unit ->
  (Curated_io.summary, string) result
(** [init ~id ~db_path ~from_dir ~force] creates a DB at [db_path] and imports
    [<from_dir>/curated.json] and [<from_dir>/whitehat_tools.json] in one transaction. The
    directory of [db_path] is created when missing ({!Ctx.ensure_dir}); [from_dir] must
    exist, and an absent file in it is imported as empty (a fresh data directory). An
    existing DB is an error unless [force], which deletes it (and its WAL/SHM files)
    first. Both files are parsed before anything is touched. On any failure the new file
    is removed.

    INV-K (FR-129): with [force], a DB whose curated data was never exported
    ([meta.export_dirty = 1]) is not deleted: the result is an error naming
    [--discard-unexported], unless [discard_unexported] is [true]. *)

val import :
  id:Identity.t ->
  Db.t ->
  from_dir:string ->
  ?prefer_file:bool ->
  unit ->
  (Curated_io.summary, string) result
(** [import ~id db ~from_dir ()] upserts curated fields and whitehat rows from the two
    files and replaces the usage of every library listed in [curated.json] with the file's
    ({!Curated_io.import}); it deletes no repository row and no fetched data (C-4), and a
    curated field a file entry does not carry keeps the database's value.

    INV-K (FR-129): while [meta.export_dirty = 1] (the database may hold curated values
    the files lack) it refuses with an error naming [--prefer-file], unless [prefer_file]
    is [true], and writes nothing ({!Curated_io.import_files}). *)
