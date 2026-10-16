(** Helpers shared by registration ({!Register}, {!Source_reg}) and unregistration
    ({!Unregister}): the result type, closed-vocabulary arguments and the locked
    read-modify-write of the data directory's [sources.toml]. *)

type outcome = {
  json : Yojson.Safe.t;  (** The structured result. *)
  gaps : string list;  (** Classified fetch failures (empty when complete). *)
  complete : bool;  (** Every fetch succeeded: exit 0 / no [isError]. *)
}

val closed : string -> (string * 'a) list -> string option -> ('a option, string) result
(** [closed what names v] maps an optional spelled value through [names]
    (case-insensitive, trimmed), or the uniform invalid-value message. *)

val parse_kind : string -> (Config.kind, string) result
(** [parse_kind s] is the source kind spelled [s] (case-insensitive), or the uniform
    invalid-value message listing the kinds. *)

val config_path : Ctx.t -> string
(** [config_path ctx] is [<data_dir>/sources.toml]. *)

val edit_config :
  Ctx.t ->
  Config_edit.table ->
  pred:([ `Repo of Config.repo | `Source of Config.source ] -> bool) ->
  block:string ->
  (Config_edit.outcome, string) result
(** [edit_config ctx table ~pred ~block] upserts one entry ({!Config_edit.upsert}) and
    writes the file atomically, under {!Config_edit.with_lock} on the data directory. *)

val created_name : Config_edit.outcome -> string
(** ["created"] or ["updated"]. *)

val home : unit -> string option
(** [home ()] is [$HOME] when set. *)

val local_roots : Ctx.t -> string list
(** [local_roots ctx] is every configured [local-root] that exists ([~] expanded), sorted
    and deduplicated. *)

val configured_checkouts : Ctx.t -> call:Fetch.call -> Local_scan.checkout list
(** [configured_checkouts ctx ~call] is the git checkouts of every {!local_roots}, as a
    full refresh scans them ({!Local_scan.scan}, read-only git). *)

val with_field : string -> Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t
(** [with_field k v obj] is [obj] with member [k] replaced by (or set to) [v]. *)

val source_json : Config.source -> Yojson.Safe.t
(** [source_json s] is the JSON view of a source entry used in results (every string value
    redacted). *)
