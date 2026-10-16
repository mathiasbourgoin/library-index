(** Minimal editor of the [[[repo]]] and [[[source]]] entries of [sources.toml] (spec
    "Registration": the tools edit the file atomically and preserve the other entries).

    The file is edited as text, never re-serialised: every line outside the edited entry
    (other tables, comments, blank lines, key order and spelling) is kept byte for byte.
    An entry is the block from its [[[repo]]] / [[[source]]] header to its last key line
    before the next table header; trailing comments and blank lines belong to what
    follows. A block is identified by parsing it alone with {!Config.of_string}. The
    edited document is validated with {!Config.of_string} before it is returned. *)

type table = Repo | Source
type outcome = Created | Updated

val render_repo : Config.repo -> string
(** [render_repo r] is the [[[repo]]] block of [r] (no trailing newline): [target], then
    [key] and [ownership] when set, [own] when true, [confidential], and [campaign],
    [campaign_prefix], [deep] when true. Every string value goes through {!Redact.text}
    (also in {!render_source}): no credential is ever written. *)

val render_source : Config.source -> string
(** [render_source s] is the [[[source]]] block of [s] (no trailing newline). *)

val upsert :
  string ->
  table ->
  pred:([ `Repo of Config.repo | `Source of Config.source ] -> bool) ->
  block:string ->
  (string * outcome, string) result
(** [upsert text table ~pred ~block] updates the first [table] block whose parsed entry
    satisfies [pred] to the entry [block] renders ([Updated]): its header, comments, blank
    lines and unchanged key lines are kept byte for byte, a changed key line is rewritten
    in place, a key [block] lacks is removed and a new key is appended after the block's
    last key line; when the block holds a line that is not [key = value] (or the merge
    would not parse to [block]'s entry) the whole block is replaced by [block]. Without a
    match, [block] is appended at the end of [text] after one blank line ([Created]).
    [Error] when the result is not a valid [sources.toml]. *)

val remove :
  string ->
  table ->
  pred:([ `Repo of Config.repo | `Source of Config.source ] -> bool) ->
  (string option, string) result
(** [remove text table ~pred] deletes the first matching block, plus one blank line before
    it when a blank line (or the end of the file) follows it, so that removing an appended
    entry restores the previous text. [Ok None] when nothing matches. *)

val read : string -> (string, string) result
(** [read path] is the file's content, [""] when it does not exist. *)

val write_atomic : string -> string -> (unit, string) result
(** [write_atomic path text] writes [text] to a temporary file, flushes it to disk and
    renames it over [path] ({!Fsio.atomic_write}). A symlinked [path] is resolved first,
    through every link of the chain and also when the last link dangles: the links are
    kept and their target is written (temporary file beside the target). The target's
    permissions are kept, else [0o600]. *)

val lock_file : string
(** [".sources.toml.lock"], in the data directory. *)

val with_lock : string -> (unit -> ('a, string) result) -> ('a, string) result
(** [with_lock data_dir f] creates [data_dir] when missing ({!Ctx.ensure_dir}) and runs
    [f] under an exclusive blocking lock on [<data_dir>/.sources.toml.lock]: every
    read-modify-write of [sources.toml] (registration, unregistration) holds it, whatever
    the export directory. *)
