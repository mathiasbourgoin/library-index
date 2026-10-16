(** The prototype's listing normalisers [gh_norm] / [gl_norm] ([refresh.py:89-117]):
    GitHub repository and GitLab project JSON to the raw-row shape
    {!Import_raw.upsert_fetched} accepts, with lowercased keys. *)

val gh : Yojson.Safe.t -> Yojson.Safe.t option
(** [gh repo] maps a GitHub repository object: [key = "github:" ^ lowercase full_name],
    [host], [full_name], [owner] (= [owner.login]), [name], [url] (= [html_url]),
    [private], [fork], [archived] (default [false]), [language], [description] (default
    [""]), [default_branch], [pushed_at], [updated_at], [parent] (= [parent.full_name]).
    [None] when [full_name], [owner.login] or [name] is missing. *)

val gl : Yojson.Safe.t -> Yojson.Safe.t option
(** [gl project] maps a GitLab project object:
    [key = "gitlab:" ^ lowercase path_with_namespace], [owner] (= [namespace.full_path]),
    [name] (= [path]), [url] (= [web_url]), [private] (= visibility is not [public]),
    [fork] (= has [forked_from_project]), [archived], [language] (= [null]: filled by the
    languages detail), [description], [default_branch], [pushed_at] and [updated_at] (=
    [last_activity_at]), [parent] (= [forked_from_project.path_with_namespace]) and
    [gl_id]. [None] when [path_with_namespace], [namespace.full_path], [path] or [id] is
    missing. *)

val gl_readme_url : Yojson.Safe.t -> string option
(** [gl_readme_url project] is the project's [readme_url], used only to build the README
    command (AM-13); it is not stored. *)

val key : Yojson.Safe.t -> string option
(** [key row] is the [key] member of a normalised row. *)
