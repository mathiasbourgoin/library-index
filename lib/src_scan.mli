(** Comment-aware line scanning shared by the C/C++, Quint and Rocq extractors (spec
    FR-120, extras batch). Pure; never raises.

    {!scan} splits a source text into its code (one string per line, comments removed and
    the contents of string literals blanked, so that braces or keywords inside them are
    never seen) and its comments, each with the lines it spans. A doc comment is then
    attached to a declaration by {!doc_above}. *)

type syntax =
  | C_like  (** [//] line comments, [/* */] block comments, ["…"] and ['…'] literals. *)
  | Slash_like
      (** As [C_like] without character literals (Quint, where ['] primes a variable). *)
  | Ml_like  (** Nested ML block comments (["(*"] … ["*)"]), ["…"] literals (Rocq). *)

type comment = {
  text : string;  (** The raw comment, markers included. *)
  first : int;  (** 0-based line where it starts. *)
  last : int;  (** 0-based line where it ends. *)
  own_line : bool;  (** No code precedes it on its first line. *)
  line_comment : bool;  (** A [//] comment. *)
}

type t = { code : string array;  (** Code of each line. *) comments : comment list }

val scan : syntax -> string -> t
(** [scan syntax text] scans [text] ([\r] dropped). An unterminated comment or literal
    runs to the end of the text. *)

val doc_above : ?accept:(string -> bool) -> t -> int -> string
(** [doc_above ?accept t i] is the cleaned text of the comment attached to line [i]: the
    own-line comment ending on line [i - 1] (or, for [//] comments, the run of consecutive
    own-line [//] comments ending there), when [accept] holds for its raw text (default:
    every comment); [""] otherwise. Cleaning drops the markers (["/**"], ["/*"], ["*/"],
    ["///"], ["//!"], ["//"], ["(**"], ["(*"], ["*)"]) and a leading ["*"] on each line,
    and trims. *)

val is_doc_c : string -> bool
(** [is_doc_c raw] is [true] for a [/** */] or [///] / [//!] comment (not [/**/]). *)

val is_doc_ml : string -> bool
(** [is_doc_ml raw] is [true] for an ML doc comment (starting ["(**"], not the empty
    ["(**)"]). *)

val squash : string -> string
(** [squash s] collapses every whitespace run of [s] to one space and trims. *)
