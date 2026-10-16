(** Bounded parallelism for detail fetches (spec AM-12). Work items run on at most
    [workers] system threads; results are handed back on the calling thread, so the caller
    can keep every DB write on one thread. *)

val run : workers:int -> 'a list -> ('a -> 'b) -> (('b, exn) result -> unit) -> unit
(** [run ~workers items f on_result] applies [f] to every item on worker threads (at most
    [max 1 workers] of them, never more than there are items) and calls [on_result] on the
    calling thread for each result, in completion order, as results arrive. An exception
    raised by [f] is passed to [on_result] as [Error]; the remaining items still run.
    Returns after every item was handed to [on_result] and every worker thread was joined.
*)
