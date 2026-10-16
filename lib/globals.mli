(** The global refresh inputs kept in [meta] (spec AM-4, AM-9): GitHub member orgs, the
    commit/PR search-count maps and the GitLab MR total. Written by a full refresh and by
    [import-raw]; read back by the single-repo refresh and the renderer. *)

val keys : string list
(** Meta keys: [gh_member_orgs], [gh_search_commits], [gh_search_prs], [gl_mr_total]. *)

val set : Db.t -> string -> Yojson.Safe.t -> unit
(** [set db k v] stores a global input as compact JSON (strings redacted). *)

val counts_of_json : Yojson.Safe.t -> (string * int) list
(** [counts_of_json v] reads a search-count object (lowercased [owner/name] → hits),
    ignoring non-integer members; [[]] for a non-object. *)

val search_counts : Db.t -> string -> (string * int) list
(** [search_counts db k] reads the stored [gh_search_commits] / [gh_search_prs] map; [[]]
    when absent or malformed. *)

val member_orgs : Db.t -> string list
(** [member_orgs db] is the stored [gh_member_orgs] list. *)
