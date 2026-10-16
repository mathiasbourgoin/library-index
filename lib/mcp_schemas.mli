(** Input and output JSON schemas of the MCP tools (spec AM-8, FR-086, FR-092, US-11).

    Input schemas state types only: value validation (closed vocabularies, ranges,
    required arguments) lives in {!Ops}, so the CLI and MCP give identical messages
    (AM-8). Output schemas describe the [structuredContent] of a successful result;
    mcp-kit validates every successful result against them. *)

val search_in : Mcp_kit.Schema.t
val search_out : Mcp_kit.Schema.t
val get_in : Mcp_kit.Schema.t
val get_out : Mcp_kit.Schema.t
val list_in : Mcp_kit.Schema.t
val list_out : Mcp_kit.Schema.t
val resolve_in : Mcp_kit.Schema.t
val resolve_out : Mcp_kit.Schema.t
val query_docs_in : Mcp_kit.Schema.t
val query_docs_out : Mcp_kit.Schema.t
val annotate_in : Mcp_kit.Schema.t
val annotate_out : Mcp_kit.Schema.t
val usage_in : Mcp_kit.Schema.t
val usage_out : Mcp_kit.Schema.t
val refresh_in : Mcp_kit.Schema.t
val refresh_out : Mcp_kit.Schema.t
val register_repo_in : Mcp_kit.Schema.t
val register_repo_out : Mcp_kit.Schema.t
val register_source_in : Mcp_kit.Schema.t
val register_source_out : Mcp_kit.Schema.t
val unregister_repo_in : Mcp_kit.Schema.t
val unregister_repo_out : Mcp_kit.Schema.t
val unregister_source_in : Mcp_kit.Schema.t
val unregister_source_out : Mcp_kit.Schema.t
val list_sources_in : Mcp_kit.Schema.t
val list_sources_out : Mcp_kit.Schema.t
val find_capability_in : Mcp_kit.Schema.t
val find_capability_out : Mcp_kit.Schema.t
val copies_in : Mcp_kit.Schema.t
val copies_out : Mcp_kit.Schema.t
val explain_in : Mcp_kit.Schema.t
val explain_out : Mcp_kit.Schema.t
val whats_new_in : Mcp_kit.Schema.t
val whats_new_out : Mcp_kit.Schema.t
val analyze_repo_in : Mcp_kit.Schema.t
val analyze_repo_out : Mcp_kit.Schema.t
