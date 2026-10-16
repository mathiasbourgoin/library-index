(** The operations shared by the CLI and the MCP tools (AM-8). Each takes the raw JSON
    arguments, validates them with {!Args} and the store modules, and never raises: any
    exception becomes a classified error ({!Errors.guard}). *)

val get : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [get ctx {"repo"}]: see {!Repo_view.get}. *)

val search : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [search ctx {"query", "limit"?, "filters"?, "detail"?, "budget_chars"?, "kind"?}]: see
    {!Search.search}; [filters] is an object validated by {!Filters.of_assoc}, the other
    optional parameters by {!Opts}. *)

val resolve_library_id : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [resolve_library_id ctx {"libraryName", "query"?, "filters"?, "detail"?,
     "budget_chars"?}]: see {!Library_id.resolve}; the budget cuts [candidates]. *)

val query_docs : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [query_docs ctx {"libraryId", "query", "limit"?, "kind"?, "filters"?, "detail"?,
     "budget_chars"?}]: see {!Docs_query.query}; the budget cuts [excerpts]. *)

val find_capability : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [find_capability ctx {"query", "limit"?, "filters"?, "detail"?, "budget_chars"?}]: see
    {!Capability.find}; the budget cuts [results]. *)

val copies : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [copies ctx {"name", "ownership"?}]: see {!Copies_view.copies}; [ownership] is
    validated against {!Vocab.ownerships} (case-insensitive). *)

val explain : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [explain ctx {"repo"}]: see {!Explain.explain}. *)

val whats_new : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [whats_new ctx {"since", "limit"?, "ownership"?}]: see {!Whats_new.whats_new}; [since]
    is parsed by {!Whats_new.parse_since}, [ownership] validated against
    {!Vocab.ownerships}. *)

val list : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [list ctx {filter keys..., "limit"?, "offset"?}]: filters are flat members validated
    by {!Filters.of_assoc}; see {!Listing.list}. *)

val annotate : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [annotate ctx {"repo", "fields": {...}}]: see {!Curate.annotate}. *)

val record_usage : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [record_usage ctx {"repo", "consumer", "note"?}]: see {!Curate.record_usage}. *)

(** {2 Refresh} *)

type refresh_outcome = {
  json : Yojson.Safe.t;
      (** The report: {!Repo_refresh.report} JSON for one repo, {!Refresh.summary_json}
          for a full run. *)
  gaps : string list;  (** Classified failure lines (empty when complete). *)
  lines : string list;  (** Human summary lines for the CLI's stderr (full run only). *)
  complete : bool;  (** Exit 0 / plain result when [true]; exit 2 / [isError] otherwise. *)
}
(** The typed outcome both front ends render: the CLI prints [json] and exits 2 when
    incomplete; the MCP tool returns [json] as structured content and, when incomplete, an
    [isError] result naming the [gaps]. *)

val mcp_refresh_timeout : float
(** [45.] seconds: the overall deadline of an MCP [refresh] (AM-9). *)

val refresh :
  Ctx.t ->
  Repo_refresh.env ->
  ?timeout:float ->
  ?dev_root:string ->
  ?progress:(float -> string -> unit) ->
  allow_full:bool ->
  Yojson.Safe.t ->
  (refresh_outcome, string) result
(** [refresh ctx env ?timeout ?dev_root ~allow_full {"repo"?, "full"?}] refreshes one
    entry ({!Repo_refresh.refresh_one}, with an overall deadline of [timeout] seconds when
    given) or, without [repo] and with [~allow_full:true] (CLI only), runs a full refresh
    ({!Refresh.run}) scanning [dev_root] (default: the first [local-root] source of
    [sources.toml], [~] expanded, an error when it needs [HOME] and [HOME] is unset; no
    local scan when none is configured). Without [repo] and [~allow_full:false] (MCP) it
    is ["missing required argument 'repo'"]. [full] must be a boolean. [progress] is
    passed to {!Repo_refresh.refresh_one} (one repository only). [sources.toml] is read
    again first ({!Ctx.reload}). *)

(** {2 Registration} *)

val register_timeout : float
(** [120.] seconds: the overall deadline of an MCP [register-repo]. *)

val register_repo :
  Ctx.t ->
  Repo_refresh.env ->
  ?timeout:float ->
  ?progress:(float -> string -> unit) ->
  Yojson.Safe.t ->
  (Register.outcome, string) result
(** [register_repo ctx env ?timeout ?progress args]: see {!Register.register_repo}; the
    fetch stops at [timeout] seconds when given. *)

val register_source :
  Ctx.t ->
  Repo_refresh.env ->
  ?timeout:float ->
  ?progress:(float -> string -> unit) ->
  Yojson.Safe.t ->
  (Register.outcome, string) result
(** See {!Source_reg.register_source}; the fetch stops at [timeout] seconds when given
    (the MCP tool passes {!register_timeout}), returning a partial result. *)

val unregister_repo : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** See {!Unregister.unregister_repo}. *)

val unregister_source : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** See {!Unregister.unregister_source}. *)

val list_sources : Ctx.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** See {!Register.list_sources}. *)

(** {2 Analysis} *)

val analyze_repo :
  Ctx.t -> Repo_refresh.env -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
(** [analyze_repo ctx env {"repo", "allow_build"?}] runs {!Analysis.analyze} with
    [env.analyze] under the refresh lock ([allow_build] defaults to [false]). *)
