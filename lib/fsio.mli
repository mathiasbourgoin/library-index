(** Crash-safe file replacement shared by the exports ({!Export}), the rendered pages
    ({!Render}) and [sources.toml] edits ({!Config_edit}). *)

val atomic_write : ?perm:int -> string -> string -> unit
(** [atomic_write ?perm path contents] writes [contents] to a temporary file beside
    [path], flushes it to disk ([fsync]), then renames it over [path]: a reader sees the
    old file or the new one, never a partial one, and a crash after the rename keeps the
    new contents. The temporary file is created with mode [0o666] (masked by the umask),
    or [0o600] then set to [perm] before the rename when [perm] is given (its contents are
    never readable more widely than [perm] allows). On failure the temporary file is
    removed and the exception ([Sys_error] or [Unix.Unix_error]) re-raised; [path] is
    untouched. [path] itself is replaced: callers resolve symlinks first. *)
