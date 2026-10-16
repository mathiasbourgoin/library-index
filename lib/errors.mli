(** Classification of unexpected exceptions into safe user-facing messages (AM-8).

    The message never contains exception text, SQL text, paths of secrets or stderr. *)

val classify : exn -> string
(** [classify e] is one of: the {!Db.Db_error} payload (already classified),
    ["I/O error (<UNIX ERROR NAME>)"], ["I/O error"], ["out of memory"] or
    ["internal error"]. *)

val guard : (unit -> ('a, string) result) -> ('a, string) result
(** [guard f] is [f ()], with any exception turned into [Error (classify e)]. *)
