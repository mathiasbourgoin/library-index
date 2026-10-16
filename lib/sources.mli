(** The global listings of a full refresh (spec FR-032, C-18, C-20, C-21, EC-11, EC-16):
    ports of the prototype's [fetch_github], [fetch_gitlab], [fetch_local],
    [add_local_extras] and [enrich] ([refresh.py:120-396]) over GET-only commands.

    Differences from the prototype, all within the spec: GitHub search goes through
    [gh api -X GET search/...] page by page (≤ 10 pages of 100, C-20); a repository seen
    in several listings keeps the most recent [pushed_at] (EC-11, the prototype kept the
    first); and every failing listing records a classified gap and clears {!t.listings_ok}
    instead of being silently ignored. *)

type entry = {
  row : Yojson.Safe.t;  (** Raw-shape row ({!Norm}) with [sources] and enrichment. *)
  readme_url : string option;  (** GitLab [readme_url], not stored. *)
}

type t = {
  entries : entry list;  (** Every listed repository, sorted by key. *)
  local : Yojson.Safe.t list;
      (** Local-root checkouts in scan order, in the shape
          {!Import_raw.write_local_checkouts} takes. *)
  gaps : string list;
      (** Gap lines in prototype order: informational ones (org 404, org with 0 visible
          repos, the search cap) and classified failures. *)
  failures : string list;  (** The failure subset of [gaps]. *)
  listings_ok : bool;
      (** Every global listing succeeded, so unseen repositories may be marked missing
          (FR-035). *)
  globals : (string * Yojson.Safe.t) list;
      (** The global inputs fetched successfully: [gh_member_orgs], [gh_search_commits],
          [gh_search_prs], [gl_mr_total]. *)
}

val search_pages : int
(** [10]: GitHub search returns at most 1000 results (C-20). *)

val parse_paginated : string -> Yojson.Safe.t list option
(** [parse_paginated out] parses [--paginate] output (concatenated JSON documents): the
    elements of every array document, and every non-array document itself. [None] on
    malformed JSON. *)

val count_search : string list -> (string * Yojson.Safe.t) list
(** [count_search names] counts lowercased repository names, in first-seen order, as a
    JSON object member list. *)

val collect :
  id:Identity.t -> call:Fetch.call -> sleep:(float -> unit) -> dev_root:string option -> t
(** [collect ~id ~call ~sleep ~dev_root] runs the listing of every source [id] declares
    (in the prototype's order), the 3 s pause between the two searches, the scan of
    [dev_root] (none when [None]), [add_local_extras] and [enrich]. *)

val labels : Config.source -> string list
(** [labels s] is the provenance labels the listing of [s] writes into a row's [sources]
    ([gh-org:<name>], [gl-group:<name>], [gh-user], …): what unregistering [s] removes.
    The collaborator label [gh-collab] is shared by every [github-collab] source. *)
