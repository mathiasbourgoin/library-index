(** The optional richer parameters shared by [search], [resolve-library-id] and
    [query-docs] (spec US-11, FR-090). Absent parameters mean the core behaviour; invalid
    values are rejected with the allowed values ({!Vocab.invalid_value}). *)

type detail = Brief | Standard | Full

val details : string list
(** [["brief"; "standard"; "full"]]. *)

val detail : Args.obj -> (detail, string) result
(** [detail o] reads optional ["detail"] (default [Standard]). *)

val min_budget : int
(** [100] characters. *)

val max_budget : int
(** [1_000_000] characters. *)

val budget : Args.obj -> (int option, string) result
(** [budget o] reads optional ["budget_chars"], an integer in [[min_budget, max_budget]].
*)

val kind : Args.obj -> (string option, string) result
(** [kind o] reads optional ["kind"], one of {!Doc_select.kinds}. *)

val apply_budget : int option -> list_key:string -> Yojson.Safe.t -> Yojson.Safe.t
(** [apply_budget budget ~list_key json] is [json] unchanged when [budget] is [None].
    Otherwise the result's compact JSON text is made to fit in [budget] code points: items
    of the array member [list_key] are dropped from the end, then the [text] or [snippet]
    of a single remaining item is cut on a UTF-8 boundary, and the result gains
    [truncated] ([true] when anything was cut). A [count] member, when present, is set to
    the number of items kept. *)
