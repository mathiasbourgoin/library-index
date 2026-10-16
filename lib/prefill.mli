(** Deterministic pre-fill of [what] and [interest] (spec FR-110, FR-111). No model is
    ever called: the values come from stored text by fixed rules.

    - [what]: the first non-empty of the repository description, its package synopsis,
      then the README's first paragraph ({!Classify.first_para}).
    - [interest]: a fixed template from ownership, primary language and domain
      (["Own OCaml code (Libraries)"]), empty when neither language nor domain is known.

    A field is written only when it is empty or its provenance is [auto]; the value
    written is marked [auto] ({!Field_meta}). Campaign and whitehat rows and unfetched
    stubs are never pre-filled. *)

val fields : string list
(** [["what"; "interest"]]. *)

val what_of :
  description:string option ->
  synopsis:string option ->
  readme:string option ->
  string option
(** [what_of ~description ~synopsis ~readme] is the [what] pre-fill. *)

val interest_of :
  ownership:string -> language:string option -> domain:string option -> string option
(** [interest_of ~ownership ~language ~domain] is the [interest] template: ["Own"],
    ["Contributed"] or ["Third-party"] (for [own], [participant], [external]), the
    language, ["code"], then the domain in parentheses. *)

val apply : Db.t -> now:string -> ?key:string -> unit -> int
(** [apply db ~now ?key ()] pre-fills every eligible row (or only [key]) and returns the
    number of fields written. Must run inside a transaction. *)
