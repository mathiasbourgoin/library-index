(** Exclusive advisory locks on a lock file ([lockf], spec C-23, AM-11). The descriptor is
    opened with [O_CLOEXEC] so it never leaks into the [gh]/[glab]/[git] children spawned
    while the lock is held. *)

val with_lock :
  blocking:bool ->
  busy:string ->
  string ->
  (unit -> ('a, string) result) ->
  ('a, string) result
(** [with_lock ~blocking ~busy path f] opens (creating, mode [0o600]) [path], takes an
    exclusive lock on it, runs [f] and releases the lock. With [~blocking:false] a lock
    held by another process gives [Error busy] at once; with [~blocking:true] it waits.
    Failing to open or lock the file is a classified [Error]; an exception raised by [f]
    propagates after the lock is released. *)
