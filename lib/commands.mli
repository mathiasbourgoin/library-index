(** The only commands refresh may run (spec FR-030, FR-071, US-4.4, C-19, C-20, AM-13).

    Remote commands are always [gh api -X GET <path>] or [glab api -X GET <path>],
    optionally followed by [--paginate] and, for GitHub raw content, [-H] and
    [Accept: application/vnd.github.raw+json]. Local commands are read-only [git -C <dir>]
    invocations: [remote -v], [log -1 --format=%cI], [rev-list --count HEAD] (optionally
    restricted to [--regexp-ignore-case --author=<owner.author_regex>]) and [ls-files]
    (optionally [-z]). Every path segment taken from data is percent-encoded, so no data
    can introduce a flag, a query parameter or a [..] segment. *)

type purpose =
  | Listing  (** A global repository or membership listing. *)
  | Search  (** GitHub commit/PR search page. *)
  | Metadata  (** One repository's metadata ([repos/o/r], [projects/<id>]). *)
  | Readme  (** README raw content. *)
  | Root_listing  (** Root directory listing used to find a manifest. *)
  | Manifest  (** A manifest's raw content. *)
  | Last_commit
  | Contributors
  | Languages
  | Parent  (** GitHub fork parent ([repos/o/r] of a fork). *)
  | Local_git  (** A read-only local [git] query. *)
  | Doc_tree  (** A repository tree listing for document selection (FR-082). *)
  | Doc_file  (** One document's raw content (FR-082). *)

type t = { purpose : purpose; argv : string array }
(** A command and why it runs. *)

val purpose_name : purpose -> string
(** [purpose_name p] is a short lowercase label (["readme"], ["last-commit"], …) used in
    gap messages. *)

(** {2 Builders} *)

val gh : ?paginate:bool -> ?raw:bool -> string -> string array
(** [gh ?paginate ?raw path] is [gh api -X GET path [--paginate] [-H Accept: raw]]. *)

val glab : ?paginate:bool -> string -> string array
(** [glab ?paginate path] is [glab api -X GET path [--paginate]]. *)

val segment : string -> string
(** [segment s] percent-encodes every byte of [s] outside [A-Za-z0-9._~-]; ["."] and
    [".."] become [%2E] / [%2E%2E]. *)

val gl_encode : string -> string
(** [gl_encode s] percent-encodes every byte outside [A-Za-z0-9_~-], so [/] is [%2F] and
    [.] is [%2E] (AM-13: GitLab file paths and project paths). *)

val gh_repo_path : string -> string
(** [gh_repo_path "o/r"] is ["repos/o/r"] with each segment encoded by {!segment}. *)

val gl_project_path : id:int option -> full_name:string -> string
(** [gl_project_path ~id ~full_name] is ["projects/<id>"], or
    ["projects/<gl_encode full_name>"] when the id is unknown. *)

val gl_readme_file : default_branch:string option -> string -> string option
(** [gl_readme_file ~default_branch readme_url] is the repository-relative README path:
    the part of [readme_url] after [/-/blob/<ref>/] (directories kept, AM-13). When
    [default_branch] is known and matches the URL, the ref is exactly that branch (which
    may contain [/]); otherwise the ref is one path segment. [None] when the URL has no
    [/-/blob/] part or an empty path. *)

val git : string -> string list -> string array
(** [git dir args] is [git -C dir args]. Only the read-only argument lists below are used:
    {!git_remotes}, {!git_last_commit}, {!git_count}, {!git_count_mine}, {!git_ls_files},
    {!git_ls_files_z}. *)

val git_remotes : string -> string array
val git_last_commit : string -> string array
val git_count : string -> string array

val git_count_mine : author:string -> string -> string array
(** [git_count_mine ~author dir] counts the commits whose author matches [author] (the
    configured [owner.author_regex]), ignoring case. *)

val git_ls_files : string -> string array

val git_ls_files_z : string -> string array
(** [git -C dir ls-files -z]: tracked paths, NUL-separated and unquoted (documents). *)

(** {2 Documents (FR-082)} *)

val gh_doc_tree : full_name:string -> branch:string option -> string array
(** [repos/o/r/git/trees/<branch>?recursive=1] ([HEAD] when the branch is unknown). *)

val gh_doc_file : full_name:string -> string -> string array
(** [repos/o/r/contents/<path>] with the raw media type; every path segment is encoded
    with {!segment}. *)

val gl_doc_tree : id:int option -> full_name:string -> string array
(** [projects/<id>/repository/tree?recursive=true&per_page=100] with [--paginate]. *)

val gl_doc_file :
  id:int option -> full_name:string -> branch:string option -> string -> string array
(** [projects/<id>/repository/files/<gl_encode path>/raw?ref=<branch or HEAD>]. *)

(** {2 Per-repository plan} *)

type repo = {
  host : string;  (** [github] or [gitlab] ([local] and [whitehat] get no commands). *)
  owner : string;
  full_name : string;
  gl_id : int option;
  default_branch : string option;
  readme_url : string option;  (** GitLab [readme_url] from the project metadata. *)
  fork : bool;
  archived : bool;
  sources : string list;
  gl_contributed : bool;
  mrs : int;
}
(** What the plan reads from a repository. *)

val content_tiers : string list
(** [["1"; "collab"; "fork"]]: tiers whose last commit and synopsis are fetched (the
    prototype's selection, C-19). *)

val gh_contrib_target : id:Identity.t -> repo -> bool
(** The prototype's GitHub contributor targets: owners other than the GitHub user and the
    collaborators when not archived, archived repos of the owner's own organisations, and
    the collaborators' repos. *)

val gl_contrib_target : repo -> bool
(** The prototype's GitLab contributor targets: contributed projects, projects with his
    MRs, and projects from his user listing. *)

val metadata_plan : id:Identity.t -> repo -> Decision.t -> t list
(** Commands that only need the listing (gated by [detail]): GitHub fork parent and
    contributors, GitLab languages and contributors. *)

val content_plan : repo -> tier:string option -> campaign:bool -> Decision.t -> t list
(** Commands that need the tier: last commit (when [detail] and the tier is in
    {!content_tiers}), README (when [readme], not a campaign repo; GitLab only with a
    usable [readme_url]) and the root listing for manifest discovery (when [readme], the
    tier is in {!content_tiers}, not a campaign repo). A campaign repo never gets a
    README, root-listing or manifest command (FR-071). *)

val plan :
  id:Identity.t -> repo -> tier:string option -> campaign:bool -> Decision.t -> t list
(** [plan] is {!metadata_plan} followed by {!content_plan}. *)

val manifest_names : string list
(** Manifest precedence (C-19): [*.opam] (matched by suffix), [package.json],
    [Cargo.toml], [pyproject.toml]. *)

val manifest : repo -> string -> t
(** [manifest repo file] fetches a root-level manifest's raw content. Never called for
    campaign repos (the content plan has no root listing for them). *)
