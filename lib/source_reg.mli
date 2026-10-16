(** [register-source]: upsert one [[[source]]] entry of [sources.toml], list that source
    and fetch its repositories at once (spec "Registration", FR-104, AC-32). *)

type outcome = Reg_common.outcome

val register_source :
  Ctx.t ->
  Repo_refresh.env ->
  ?progress:(float -> string -> unit) ->
  ?deadline:float ->
  Yojson.Safe.t ->
  (outcome, string) result
(** [register_source ctx env ?progress ?deadline {"kind", "name"?, "host"?, "own"?,
     "probe"?, "include_subgroups"?, "confidential"?, "campaign_prefix"?}] upserts a
    [[[source]]] entry matched by kind and name (case-insensitive). Every flag is
    tri-state: an absent flag keeps the existing entry's value (a new entry takes the
    defaults: not own, no probe, subgroups included, not confidential, no campaign
    prefix); the block is edited in place ({!Config_edit.upsert}). A [local-root] whose
    name does not expand to an existing directory is an error and registers nothing.

    Only this source is listed ({!Sources.collect} with {!Identity.for_source}: its
    listing alone, but the author, classification rules, campaign prefixes and exemptions
    of the whole configuration), so a row reached through it is flagged campaign exactly
    as a full refresh would flag it (AM-17); then every listed repository is fetched and
    written incrementally (the README rule, caps and pre-fill of a refresh; [sources]
    labels from other sources are kept) and their documents are ingested. After [deadline]
    (MCP: 120 s) the remaining calls fail as gaps, no further document is ingested and the
    gap "… continue with `library-index refresh`" is added (a partial result). Result:
    [{source, registered, complete, listed, repos_added, repos_updated, docs, gaps}]
    ([repos_added] counts repositories not fetched before). [progress] receives [1.] then
    [1.] to [2.] across the repositories. *)
