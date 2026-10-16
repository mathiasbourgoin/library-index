(** Read-only subprocess execution for refresh (spec AM-15, FR-072, C-34).

    Commands run through [Unix.create_process] with stdin on [/dev/null] and both output
    pipes drained with [select], under a per-command timeout after which the child is
    killed and reaped. The process environment is inherited unchanged and never logged.
    stderr is read only to classify a failure and is then dropped: nothing here prints,
    stores or returns stderr, the environment or the command line. *)

type failure = [ `Auth | `Not_found | `Rate_limit | `Network | `Other ]
(** The closed set of failure classes (US-8.3). A timeout is [`Network]; a command that
    cannot be started is [`Other]. *)

type runner = timeout:float -> string array -> (string, failure) result
(** A command runner: [run ~timeout argv] is the command's stdout on exit status 0, else
    its classified failure. Refresh code takes a runner as a parameter so that tests
    inject a fake one with canned output (no network, no subprocess). *)

val default_timeout : float
(** [30.] seconds per command (C-31). *)

val describe : failure -> string
(** [describe f] is ["auth"], ["not-found"], ["rate-limit"], ["network"] or ["other"]. *)

val classify_failure : string -> failure
(** [classify_failure stderr] maps gh/glab/git error text to a class: rate-limit
    (["rate limit"], HTTP 429, abuse detection), then auth (HTTP 401, bad credentials,
    "not logged in", [auth login] hints), then not-found (["HTTP 404"],
    ["404 Not Found"]), then network (resolution, connection, TLS handshake, unexpected
    EOF, timeouts, HTTP 5xx), else [`Other]. Phrases match case-insensitively on word
    boundaries only: a bare "not found" (e.g. "command not found") or "tls" inside a word
    is not classified. *)

val max_output : int
(** Cap on captured stdout (64 MiB); a larger output fails as [`Other]. *)

val max_stderr : int
(** Cap on the stderr kept for classification (64 KiB); the rest is read and dropped. *)

val run : runner
(** The real runner. [argv.(0)] is resolved through [PATH]. The timeout bounds the whole
    command, including the wait for its exit after both pipes closed: a child still
    running at the deadline is killed and the call fails as [`Network]. *)
