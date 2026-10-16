(** The [explain] tool: one call for planning a reuse decision (spec US-12, FR-091,
    AC-28). Read-only. *)

val explain : Ctx.t -> string -> (Yojson.Safe.t, string) result
(** [explain ctx repo] resolves [repo] ({!Resolve.resolve}) and returns
    [{key, name, full_name, uri, what, interest, status, status_override, rules,
     consumers, usage, domain, tier, ownership, role, language, archived, provenance,
     copies, doc_count, api_item_count, last_commit, pushed_at, analysis}]:
    - [status] is the effective status; [domain] the effective domain;
    - [usage] the usage history ([{consumer, note, at}], oldest first);
    - [provenance] maps each curated field with a value to [auto] or [curated] (FR-112);
    - [copies] is [{count, maintained, keys}] ({!Copies}; [maintained] is this row's
      maintained copy, possibly itself);
    - [api_item_count] is [{scan, arch-index, total}];
    - [analysis] is the last arch-index analysis
      [{status, cause, head_sha, at, tool_version}], or [null] when none was recorded.
      Errors are those of {!Resolve.resolve}. *)
