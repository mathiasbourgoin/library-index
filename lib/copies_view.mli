(** The [copies] tool: every copy of a library and which one is maintained (spec US-12,
    FR-091, AC-28).

    The library is looked up by name: every non-whitehat row whose name or [owner/name]
    (case-insensitive), index key, or one of whose package [aliases] equals the argument
    is a seed. Each seed's {!Copies.group} (same name on another host, fork/parent pairs,
    [local:] copies) is one group; seeds of the same group give it once. The maintained
    member is {!Copies.canonical} of the group's first seed (curated [maintained_side],
    else the most recent non-archived same-name row, else the fork parent). Read-only. *)

val copies : Ctx.t -> ?ownership:string -> string -> Yojson.Safe.t
(** [copies ctx ?ownership name] is
    [{name, count, groups: [{maintained, members}], hint?}]. A member is
    [{key, host, archived, last_commit, pushed_at, local_paths, maintained, uri}]
    ([local_paths]: the local checkouts, [[]] when none). Members are ordered maintained
    first, then by key; groups by their maintained key. [count] is the number of members
    over all groups. With [ownership] (a canonical {!Vocab.ownerships} value), only
    members of that ownership are listed and a group left empty is dropped ([maintained]
    still names the group's maintained copy). No match: [groups = []] and a [hint] naming
    [search] and [resolve-library-id]. *)
