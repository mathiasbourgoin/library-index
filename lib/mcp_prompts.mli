(** The MCP prompts (spec US-13 extras, FR-095, AC-30). Their text is generic: it names
    tools, never a repository, person or organisation. Arguments are quoted as data, on
    one line, cut to {!max_argument} characters. *)

val max_argument : int
(** [500] characters per quoted argument. *)

val before_building : Mcp_kit.Prompt.t
(** [before-building(capability)] (required argument): one user message running the
    reuse-first procedure: [find-capability] and [search] / [resolve-library-id] for the
    capability, then [explain] (and [query-docs]) on the candidates, then report the
    decision (reuse, extend or build) with the library and item, and [record_usage] after
    adopting one. *)

val record_usage : Mcp_kit.Prompt.t
(** [record-usage(repo, consumer, note?)] ([repo] and [consumer] required): one user
    message asking to call [record_usage] with those arguments ([resolve-library-id] first
    when the repository is ambiguous or unknown). *)

val all : Mcp_kit.Prompt.t list
(** [[before_building; record_usage]]. *)
