(** Pure document chunking for the documents store (spec FR-083).

    - Markdown is split at ATX headings of levels 1 to 3 that are outside fenced code. A
      chunk is titled with its heading path (["Guide > Installing"]). Text before the
      first heading is titled with the document title.
    - An OCaml interface ([.mli]) gives one chunk per top-level item ([val], [type],
      [module], [module type], [exception], [class], [class type], [external], [include]).
      Each chunk carries the item's doc comment, whether it comes before or directly after
      the item, and is titled with the item kind and name (["val parse_file"]). The
      module-level doc comment gives a leading chunk.
    - Any other interface file is chunked as a whole.

    Every chunk is at most {!max_chars} code points. A longer section is split on blank
    lines (paragraphs), and a paragraph longer than the cap is cut on a UTF-8 boundary.
    The second and later parts get the titles ["<title> (2)"], ["<title> (3)"] and so on.
    Blank chunks are dropped. No function here raises. *)

type t = { title : string; text : string }
(** One chunk. *)

val max_chars : int
(** [2000] code points. *)

val split_long : title:string -> string -> t list
(** [split_long ~title text] is [text] (trimmed) as one chunk, or as several parts when it
    exceeds {!max_chars}; [[]] when [text] is blank. *)

val markdown_title : string -> string option
(** [markdown_title text] is the text of the first level-1 ATX heading outside fenced
    code, if any. *)

val markdown : default_title:string -> string -> t list
(** [markdown ~default_title text] splits markdown at h1–h3 headings (headings inside
    [```] or [~~~] fences are ignored). A section whose only content is its heading line
    is dropped. *)

val mli : module_name:string -> string -> t list
(** [mli ~module_name text] chunks an OCaml interface. Items are recognised at column 0
    and outside comments, strings and [sig]/[struct]/[object]/[begin] … [end] blocks.
    [and] continues the current item, and [open] lines are dropped. The module doc chunk
    is titled ["<module_name> (module)"]. A doc comment separated from the previous item
    by a blank line belongs to the next item; one written directly after an item belongs
    to that item. *)

val plain : title:string -> string -> t list
(** [plain ~title text] is {!split_long} of the whole text. *)

type mli_item = {
  title : string;
      (** The item kind and name, as {!mli} titles it (["val parse_file"]). *)
  signature : string;  (** The item's text. *)
  docs : string list;  (** Its doc comments (before, then after), verbatim. *)
}
(** One top-level item of an interface, for the API extractors ({!Extract}). *)

val mli_items : string -> string list * mli_item list
(** [mli_items text] is the module doc comments and the items of an interface, with the
    same attachment rules as {!mli}; [open] lines are dropped. *)
