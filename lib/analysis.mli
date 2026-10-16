(** Optional arch-index analysis (spec "Optional arch-index analysis", FR/AC-34).

    library-index writes no LSP or symbol extraction of its own: it runs the external
    [arch-index] tool on a local checkout, as a subprocess, and imports its documented
    output (the comment_db SQLite contract: [comment_db_meta], [functions], optional
    [calls]) into [api_items] (source [arch-index], [analyzed_head]) and [api_calls]. Each
    run records one [analysis] row: [status ∈ {ok, partial, skipped, failed}], [cause],
    [head_sha], [at], [tool_version].

    Preconditions, checked first and recorded as [skipped] with their cause: not a
    campaign or whitehat row; a local checkout (never cloned); an arch-index binary on
    [PATH] ([arch_index_cli], else the [arch-index] wrapper); a language arch-index
    supports ([ocaml], [typescript], [rust], [go], [python], detected by the {!Extract}
    extractors on the checkout's tracked files); that language's LSP binary on [PATH]
    ([ocamllsp], [typescript-language-server], [rust-analyzer], [gopls], [pylsp]). For
    OCaml the CMT enricher needs a built project ([_build/default] holding [.cmt] files):
    nothing is built unless [allow_build], which runs [dune build --root <checkout>] (at
    most 600 s); without a build arch-index runs with [--no-enrich] and the analysis is
    [partial].

    arch-index runs with [EPURE_ARCH_INDEX_TIMEOUT_S=300] (its LSP phase) under an overall
    timeout of 420 s, writing into a temporary file under [<data dir>/analysis/], removed
    afterwards. A failed run, an unreadable or unsupported output ([schema_version] not
    [1] / [1.x]) or no function at all is [failed] and keeps the previous [arch-index]
    items; functions without any call edge are [partial] with the cause
    ["empty call graph (LSP prepareCallHierarchy)"]. Only exported functions ([exported] /
    [exposed] = 1, when the column exists) become items; call edges between imported items
    become [api_calls] rows (a [calls] table keyed by [caller_id]/[callee_id], or by
    [caller_name]/[caller_file]/[callee_name]/ [callee_file], are both read). Nothing here
    raises, and no failure makes a refresh or an MCP call fail. *)

type runner = Proc.runner
(** Runs [git], [dune] and arch-index ({!Proc.run} in production). *)

val on_path : string -> string option
(** [on_path name] is the absolute path of the first executable [name] in [$PATH]. *)

val lsp_binary : string -> string option
(** [lsp_binary language] is the LSP server arch-index starts for [language]. *)

val arch_language : Api_item.context -> string option
(** [arch_language ctx] is the first language arch-index supports among the extractors
    that detect [ctx], in {!Extract.core} order. *)

val timeout_env : string
(** ["EPURE_ARCH_INDEX_TIMEOUT_S=300"]. *)

val analyze :
  Ctx.t ->
  run:runner option ->
  allow_build:bool ->
  string ->
  (Yojson.Safe.t, string) result
(** [analyze ctx ~run ~allow_build repo] ([run = None]: analysis disabled, recorded as
    [skipped] after the campaign and checkout checks) resolves [repo]
    ({!Resolve.resolve}), analyses it and records the outcome. Before anything runs, the
    campaign decision of every write path is applied with the configured checkouts
    ({!Campaign.apply_now}): a row it makes a campaign repository is marked at once, in
    its own transaction, and [skipped]. The result is
    [{key, status, cause, head_sha, at, tool_version, language, items, calls}]
    ([items]/[calls]: imported in this run). [Error] only for an unknown or ambiguous
    repository. *)

val head : run:runner -> string -> string option
(** [head ~run dir] is [git -C dir rev-parse HEAD], [None] when it fails. *)

type summary = {
  considered : int;  (** Tier-1 [own] rows with a local checkout. *)
  analysed : int;  (** Of which the HEAD changed since the last analysis. *)
  ok : int;
  partial : int;
  skipped : int;
  failed : int;
}

val summary_json : summary -> Yojson.Safe.t
(** [summary_json s] is [s] as a JSON object. *)

val refresh_hook : Ctx.t -> run:runner -> summary
(** [refresh_hook ctx ~run] analyses (without building) every Tier-1 [own] non-campaign
    row that has a local checkout and whose checkout [HEAD] differs from its recorded
    [analysis.head_sha] (or that has no analysis yet). Rows whose [HEAD] cannot be read
    are not analysed. Never raises. *)
