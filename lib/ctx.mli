(** Runtime context shared by the CLI and the MCP server, and the data-directory layout
    (spec FR-100..FR-103).

    The data directory holds everything that is the user's, never the code's:
    [sources.toml], [library-index.db], the [curated.json] / [whitehat_tools.json] exports
    and the rendered [INDEX.md]. *)

type t = {
  db : Db.t;
  db_path : string;
  data_dir : string;  (** Where [sources.toml] is read. *)
  export_dir : string;  (** Where [curated.json] / [whitehat_tools.json] are written. *)
  config : Config.t;  (** The parsed [sources.toml] ({!Config.empty} when absent). *)
  identity : Identity.t;  (** [Identity.of_config config]. *)
  now : unit -> Ptime.t;  (** Injected clock (C-12); tests freeze it. *)
}

val home : unit -> (string, string) result
(** [home ()] is [$HOME], or [Error "HOME is not set"] when it is unset or blank. The only
    place the CLI and the library read [HOME]. *)

val db_file : string
(** ["library-index.db"]. *)

val xdg_data_dir : unit -> (string, string) result
(** [xdg_data_dir ()] is [$XDG_DATA_HOME/library-index], else
    [$HOME/.local/share/library-index]. *)

val resolve_data_dir :
  data_dir:string option -> db:string option -> (string, string) result
(** [resolve_data_dir ~data_dir ~db] is, in order: the [--data-dir] flag; a non-empty
    [$LIBRARY_INDEX_DATA]; the directory of an explicit database ([--db], else a non-empty
    [$LIBRARY_INDEX_DB]), so that a database given alone keeps its exports beside it; else
    {!xdg_data_dir}. *)

val resolve_db_path : db:string option -> data_dir:string -> string
(** [resolve_db_path ~db ~data_dir] is the [--db] flag, else a non-empty
    [$LIBRARY_INDEX_DB], else [<data_dir>/library-index.db]. *)

val load_config : string -> (Config.t * Identity.t, string) result
(** [load_config data_dir] parses [<data_dir>/sources.toml] ({!Config.load_dir}) and
    derives its identity. *)

val reload : t -> (t, string) result
(** [reload t] is [t] with [sources.toml] read again from [t.data_dir] (registration edits
    it; a long-running MCP server must not act on a stale copy); [t] unchanged when the
    data directory holds no [sources.toml]. *)

val ensure_dir : string -> (unit, string) result
(** [ensure_dir dir] creates [dir] and its missing parents (mode [0700]); [Ok ()] when it
    already is a directory, else an error naming the path that could not be created. *)

val system_clock : unit -> Ptime.t
(** [system_clock ()] is the current UTC time. *)

val rfc3339 : Ptime.t -> string
(** [rfc3339 t] is [t] as [YYYY-MM-DDTHH:MM:SSZ]. *)
