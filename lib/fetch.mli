(** Detail fetch for one repository (spec FR-030, FR-031, FR-036, FR-071, C-17..C-19,
    AM-13, EC-15).

    Every remote command comes from {!Commands} and goes through an injected [call]
    function, so this module neither spawns processes nor touches the DB. [local:]
    repositories read their README and manifests from disk. Results are returned as field
    overrides for the stored row; a failed command leaves its fields out, so the stored
    values are kept (FR-034). A [`Not_found] answer means "no such data" (no README, empty
    repository, contributor list unavailable) and is not a failure. *)

type call = Commands.t -> (string, Proc.failure) result
(** Runs one command (the caller adds timeouts, deadlines and rate-limit stops). *)

type outcome = {
  fields : (string * Yojson.Safe.t) list;
      (** Detail columns fetched in this run, in raw-row shape (see {!detail_fields}).
          [`Null] means "fetched: no value". *)
  readme : string option option;
      (** [None]: README not fetched (not due, campaign repo, or failed); [Some None]: the
          repo has no README; [Some (Some text)]: the prepared README ({!prepare_readme}).
      *)
  detail_done : bool;
      (** Detail was due and every detail command succeeded (sets [detail_fetched_at]). *)
  failures : (Commands.purpose * Proc.failure) list;  (** Classified failures, in order. *)
}

val detail_fields : string list
(** Columns a detail fetch may override: [synopsis], [last_commit], [contrib], [parent],
    [parent_pushed_at], [parent_archived], [language]. *)

val readme_cap : int
(** [262144] bytes (256 KiB, FR-036), marker included. *)

val prepare_readme : string -> string
(** [prepare_readme s] redacts [s] ({!Redact.text}), then, if it exceeds {!readme_cap}
    bytes, cuts it on a UTF-8 boundary and appends a truncation marker so that the result
    is at most {!readme_cap} bytes. *)

val synopsis_of_manifest : name:string -> string -> string option
(** [synopsis_of_manifest ~name content] reads the [synopsis: "…"] line of an [*.opam]
    file, the [description] string of a [package.json], or the first [description = "…"]
    line of a [Cargo.toml] / [pyproject.toml]; [None] when absent or empty. *)

val local_synopsis : string -> string option
(** [local_synopsis dir] applies the manifest precedence to the files of the local
    checkout [dir] ([*.opam] in name order, [package.json], [Cargo.toml],
    [pyproject.toml]); [None] when none yields a synopsis or [dir] is unreadable. *)

val local_readme : string -> string option
(** [local_readme dir] is the content of the first of [README.md], [README.org], [README],
    [readme.md] in [dir] (the prototype's list). *)

val gh_contrib : id:Identity.t -> string -> Yojson.Safe.t option
(** [gh_contrib ~id json] is the prototype's GitHub contributor record for the GitHub user
    of [id] (login compared ignoring case) ([commits], [rank], [share] rounded to 3
    places, [n_contributors]), or [{commits: 0, n_contributors}] when he is absent; [None]
    when [json] is not a list. *)

val gl_contrib : id:Identity.t -> string -> Yojson.Safe.t option
(** [gl_contrib ~id json] is the prototype's GitLab contributor record: the owner's
    commits summed over entries whose lowercased name is one of [id.me_names] or whose
    lowercased email contains one of [id.me_email_parts], the best rank ([null] when
    absent), share and count. *)

val repo_of_row : ?readme_url:string -> Yojson.Safe.t -> Commands.repo
(** [repo_of_row ?readme_url row] reads the plan inputs from a raw-shape row. *)

val run :
  id:Identity.t ->
  call:call ->
  row:Yojson.Safe.t ->
  ?readme_url:string ->
  search:int * int ->
  Decision.t ->
  outcome
(** [run ~id ~call ~row ?readme_url ~search decision] performs the due fetches for [row]
    (a merged raw-shape row; [search] is its GitHub (commit, PR) search hits). The tier
    gating last commit and synopsis is derived after the contributor fetch, with
    {!Classify.derive}; a repository registered in [id] ({!Identity.is_registered}) is
    treated as tier ["1"] for that gating. Campaign repos get no README, synopsis or
    manifest fetch; their outcome sets [synopsis] to [`Null]. *)
