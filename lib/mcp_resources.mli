(** The two resource templates of the MCP server (spec FR-093, US-13).

    - {!Liburi.repo_template} resolves to the repository entry as [application/json] (the
      same JSON as the [get] tool).
    - {!Liburi.doc_template} resolves to the full text of a stored document as
      [text/markdown] (markdown files) or [text/plain].

    A URI that does not parse, an unknown repository or document, a whitehat entry, and
    any document of a campaign row are not found (the server's resource-not-found error).
    Texts were redacted and sanitised at ingest and are sanitised again on output. A DB
    failure is a generic resolver failure, never an exception text. *)

type content = { mime_type : string; text : string }

val read : Ctx.t -> string -> content option
(** [read ctx uri] is what [resources/read] returns for [uri], or [None] (not found). *)

val max_values : int
(** [100]: the completion cap (MCP and mcp-kit's {!Mcp_kit.Completion.valid_result}). *)

val complete : Ctx.t -> vars:string list -> Mcp_kit.Completion.handler
(** [complete ctx ~vars] completes a template variable (spec FR-095, AC-30): [host]
    ([github], [gitlab], [local]), [owner] ([_] for local rows), [name] or [path] (the
    stored documents of the repository the earlier [host] / [owner] / [name] arguments
    designate). Values come from the DB (campaign and whitehat rows never), are
    percent-encoded as in the URI ({!Liburi.encode}) and match the typed prefix
    case-insensitively, encoded or not. At most {!max_values} values, sorted, with [total]
    and [has_more]. A variable or prior argument outside [vars] is [Invalid_params]; a DB
    failure is [Failed]. *)

val templates : Ctx.t -> Mcp_kit.Resource_template.t list
(** [templates ctx] is the repository and document templates, in that order, each with
    {!complete} on its variables. *)
