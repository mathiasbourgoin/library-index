(** Local git checkouts under the dev root (spec C-18; ports of the prototype's
    [fetch_local], [local_language] and [enrich], [refresh.py:276-396]). Every git
    invocation is a read-only {!Commands.git} command run through the injected [call]. *)

type checkout = {
  path : string;
  worktree : bool;  (** [.git] is a file (a linked worktree). *)
  remote_url : string option;  (** [origin], else the first remote. *)
  remote_key : string option;  (** {!Classify.norm_remote} of [remote_url]. *)
  all_remote_keys : string list;  (** Sorted keys of every GitHub/GitLab remote. *)
  last_commit : string option;
  my_commits : int;
      (** Commits whose author matches the configured [owner.author_regex], ignoring case
          (0 when none is configured). *)
  total_commits : int;
  remotes_unreadable : bool;
      (** Neither [git remote -v] (git refuses some checkouts, e.g. another user's) nor
          the checkout's own config file could be read: its remotes are unknown, so the
          campaign rule treats the repositories it holds as campaign (fail closed). When
          git refuses but the config file is readable, the remotes come from the file. *)
}

val scan : id:Identity.t -> call:Fetch.call -> string -> checkout list
(** [scan ~id ~call dev_root] lists every directory of [dev_root] (name order) that holds
    [.git]. Failing git commands yield empty values, as in the prototype. An unreadable
    [dev_root] yields [[]]. *)

val checkout : id:Identity.t -> call:Fetch.call -> string -> checkout
(** [checkout ~id ~call dir] reads one checkout (remotes, last commit, commit counts) with
    the read-only git commands of {!scan}. *)

val to_json : checkout -> Yojson.Safe.t
(** [to_json c] is the record shape {!Import_raw.write_local_checkouts} takes. *)

val enrich : call:Fetch.call -> checkout list -> Yojson.Safe.t list -> Yojson.Safe.t list
(** [enrich ~call local rows] sets, on every raw-shape row, [local_paths] (non-worktree
    checkouts whose remotes include the row's key, else the first one), [local_worktrees],
    and [local_my_commits] / [local_total_commits] (maxima over the primary checkouts,
    [null] when none). A [local:] row matches its checkout by directory name and, when it
    has no language, gets {!Classify.language_of_files} of [git ls-files]. Every row also
    gets [checkout_remote_keys], every [remote_key] / [all_remote_keys] of its matched
    checkouts, which {!Campaign.decide} reads (AM-17, INV-D: no campaign decision is taken
    here). *)
