(** OCaml API extractor (spec FR-120): the items of every [.mli] (with their [(** *)]
    docs, attached as {!Chunk.mli} attaches them), and, for a module without an [.mli],
    the column-0 [let] / [type] / [module] / [exception] / [external] / [class] items of
    its [.ml] with the doc comment written directly above. Qualified paths are
    [Module.name] ([Module] from the file name). Detected by a root [dune-project] or
    [*.opam], or the primary language. *)

val extractor : Api_item.extractor
(** The OCaml extractor. *)

val mli_items : path:string -> string -> Api_item.item list
(** [mli_items ~path text] is the items of an interface; a module doc gives a leading
    [module] item. *)

val ml_items : path:string -> string -> Api_item.item list
(** [ml_items ~path text] is the top-level items of an implementation. *)
