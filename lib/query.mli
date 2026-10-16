(** Safe FTS5 query construction (spec C-9, FR-012, FR-013).

    Tokens are the maximal runs of ASCII letters, digits, [_] and non-ASCII bytes; every
    token is double-quoted, so no FTS5 operator, column filter or prefix syntax can be
    expressed by user input. Quoted tokens that the tokenizer reduces to nothing match
    nothing and raise no error (S0 spike). *)

val tokens : string -> string list
(** [tokens q] splits [q] as above, in order (duplicates kept). *)

val and_query : string list -> string
(** [and_query ts] is ["\"t1\" \"t2\" …"] (implicit AND). *)

val or_query : string list -> string
(** [or_query ts] is ["\"t1\" OR \"t2\" …"]. *)
