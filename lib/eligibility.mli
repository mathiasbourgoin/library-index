(** The one confidentiality and documentation-eligibility rule (spec AM-17, FR-071,
    FR-085, EC-15), as SQL fragments over the [repos] table and as an OCaml predicate.

    A row is {e confidential} when it is a campaign repository ([campaign <> 0]) or a
    whitehat tool ([host = 'whitehat']): nothing it holds beyond the withheld identity may
    be stored as text, served, analysed or rendered. A row is {e documentation-eligible}
    when it is not confidential and is Tier 1 or registered. Every module that filters on
    these rules uses this module; no copy of the SQL lives elsewhere.

    Every fragment takes the alias of the [repos] table ([""] for an unqualified column
    list) and only reads columns declared [NOT NULL] or compared through [coalesce], so
    its negation is never [NULL]. *)

val not_confidential : ?alias:string -> unit -> string
(** [not_confidential ~alias ()] holds for a row that is neither a campaign repository nor
    a whitehat tool. *)

val docs_eligible : ?alias:string -> unit -> string
(** [docs_eligible ~alias ()] is {!not_confidential} and (Tier 1 or registered): the rows
    whose documents, API items and analyses may be stored and served. *)

val docs_fetchable : ?alias:string -> unit -> string
(** [docs_fetchable ~alias ()] is {!docs_eligible} restricted to rows a document refresh
    may fetch: a GitHub, GitLab or local row that has been fetched and is not missing. *)

val ineligible_repo_ids : string
(** ["repo_id IN (SELECT id FROM repos WHERE NOT (<docs_eligible>))"]: the condition a
    purge of documents or API items deletes on. *)

val is_confidential : campaign:bool -> host:string -> bool
(** [is_confidential ~campaign ~host] is the OCaml form of the negation of
    {!not_confidential}. *)
