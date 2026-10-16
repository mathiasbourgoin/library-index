(** Deterministic JSON file rendering for the committed exports.

    Layout mirrors Python's [json.dumps(indent=1, ensure_ascii=False)]: one member per
    line, one extra space of indentation per level, raw UTF-8, [{}]/[[]] for empty
    containers, and a trailing newline. *)

val to_string : Yojson.Safe.t -> string
(** [to_string v] renders [v] in the layout above. *)
