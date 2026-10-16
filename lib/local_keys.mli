(** The remotes of a repository's local checkouts, for the campaign rule of every write
    path (spec AM-17, FR-071): a repository is a campaign repository when any remote of
    any of its checkouts is under a campaign prefix, whatever path (full refresh,
    [refresh --repo], registration) fetches it. *)

type t
(** Remote keys indexed by repository key and by checkout path. *)

val empty : t
(** No checkout known. *)

val of_checkouts : ?confidential_roots:string list -> Local_scan.checkout list -> t
(** [of_checkouts ?confidential_roots cs] indexes every checkout under each of its remote
    keys and its path. A checkout whose remotes could not be read, or that lies under one
    of [confidential_roots] (expanded paths), is {e forced}: see {!forced_row}. *)

val local_roots : Ctx.t -> string list
(** [local_roots ctx] is every configured [local-root] that exists ([~] expanded), sorted
    and deduplicated. *)

val configured_checkouts : Ctx.t -> call:Fetch.call -> Local_scan.checkout list
(** [configured_checkouts ctx ~call] is the git checkouts of every {!local_roots}, as a
    full refresh scans them ({!Local_scan.scan}, read-only git). *)

val confidential_roots : Ctx.t -> string list
(** [confidential_roots ctx] is {!Identity.t.confidential_roots} with [~] expanded (both
    as written and resolved through symlinks). *)

val index : Ctx.t -> Local_scan.checkout list -> t
(** [index ctx cs] is {!of_checkouts} of [cs] with the configured confidential local roots
    ({!confidential_roots}). *)

val configured : Ctx.t -> call:Fetch.call -> t
(** [configured ctx ~call] indexes {!configured_checkouts}, with the configured
    confidential local roots ({!Identity.t.confidential_roots}). *)

val for_row : t -> id:Identity.t -> call:Fetch.call -> Yojson.Safe.t list -> string list
(** [for_row t ~id ~call rows] is every remote key of every checkout of the repository the
    raw-shape [rows] describe (the listed row and the stored one): the indexed checkouts
    that have its key among their remotes, and the checkouts at its [local_paths] (read
    with read-only git when not indexed). *)

val forced_row : t -> id:Identity.t -> call:Fetch.call -> Yojson.Safe.t list -> bool
(** [forced_row t ~id ~call rows] is [true] when the repository the [rows] describe is
    held by a forced checkout (remotes unreadable, or under a confidential local root):
    matched by one of its remote keys, by its [local_paths], or by the checkout
    directory's name equal to the row's name (a checkout whose remotes are unknown cannot
    be matched otherwise). Such a repository is a campaign repository (fail closed). *)

val check : t -> (unit, string) result
(** [check t] is an error naming every indexed checkout whose remotes could not be read at
    all (neither git nor its config file): such a checkout cannot be tied to a repository,
    so no fetch may run while it is there (fail closed, nothing written). *)
