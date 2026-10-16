(** The [render] command: writes [INDEX.md] and the dated HTML page from the DB (spec
    US-6, FR-050..052, C-26, EC-22, EC-23, AM-6, AM-7).

    Both files are written through a temporary file in the target directory and renamed.
    After the HTML page is written, older pages in the notes directory are deleted, but
    only files whose name matches [^[0-9]{4}-[0-9]{2}-[0-9]{2}-library-index\.html$] and
    whose content contains [<title>Library index</title>], and never the file just
    written. *)

type outcome = {
  index_md : string;  (** Path written. *)
  notes_html : string option;
      (** Path written, [None] when no notes dir is configured or it was unusable. *)
  deleted : string list;  (** Older pages removed (file names). *)
  warnings : string list;
      (** Non-fatal problems: missing or unwritable notes dir, a stale page that could not
          be deleted. *)
}
(** What a render did. A non-empty [warnings] means exit code 2. *)

val notes_file_name : Ptime.t -> string
(** [notes_file_name now] is [<local YYYY-MM-DD>-library-index.html]. *)

val is_stale_page : name:string -> content:string -> bool
(** [is_stale_page ~name ~content] is the AM-7 deletion predicate (name pattern and
    title), without the "not the file just written" exclusion. *)

val delete_stale :
  ?remove:(string -> unit) ->
  notes_dir:string ->
  keep:string ->
  unit ->
  string list * string list
(** [delete_stale ?remove ~notes_dir ~keep ()] removes the stale pages of [notes_dir]
    (AM-7: name pattern and title; never [keep]) and returns the deleted names and one
    warning per page that could not be removed ([remove], default [Sys.remove], raised
    [Sys_error]). An unreadable directory deletes nothing. *)

val run :
  Db.t ->
  settings:Render_data.settings ->
  out:string ->
  notes_dir:string option ->
  now:Ptime.t ->
  home:string ->
  (outcome, string) result
(** [run db ~settings ~out ~notes_dir ~now ~home] renders both views at clock [now] with
    the configured texts, replacing [home] by [~] in local paths. Failing to write [out]
    is an error; a missing or unwritable [notes_dir] is a warning (EC-22); without a notes
    dir only [INDEX.md] is written. *)
