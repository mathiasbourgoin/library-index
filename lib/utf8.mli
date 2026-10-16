(** Small UTF-8 helpers (length in code points, boundary-safe truncation). *)

val length : string -> int
(** [length s] is the number of code points of [s], counting every byte that is not a
    continuation byte ([0x80..0xBF]). *)

val truncate_bytes : string -> int -> string
(** [truncate_bytes s n] is the longest prefix of [s] of at most [n] bytes that does not
    end inside a multi-byte sequence. *)

val take : string -> int -> string
(** [take s n] is the first [n] code points of [s] (all of [s] when it has fewer), like
    Python's [s[:n]] on a [str]. *)

val sanitize : string -> string
(** [sanitize s] is [s] made valid UTF-8 (spec AM-20): every maximal invalid byte sequence
    is replaced by U+FFFD. A valid [s] is returned unchanged. Every fetched or user text
    goes through it before storage ({!Redact.text}, {!Db.text}), and so does everything
    echoed back. *)

val sanitize_json : Yojson.Safe.t -> Yojson.Safe.t
(** [sanitize_json v] applies {!sanitize} to every string and member name of [v], so that
    the CLI and MCP outputs are always valid UTF-8 JSON. *)
