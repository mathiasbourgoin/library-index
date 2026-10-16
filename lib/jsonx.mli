(** The one place for JSON field access and stored-JSON decoding. Every module reading a
    member of a JSON object, or a JSON-encoded TEXT column, goes through these helpers.
    None of them raises. *)

val member : string -> Yojson.Safe.t -> Yojson.Safe.t option
(** [member k j] is member [k] of the object [j]; [None] when absent or [j] is not an
    object. *)

val get : string -> Yojson.Safe.t -> Yojson.Safe.t
(** [get k j] is {!member} with [`Null] for absent. *)

val str : string -> Yojson.Safe.t -> string option
(** [str k j] is member [k] when it is a string. *)

val bool : string -> Yojson.Safe.t -> bool
(** [bool k j] is member [k] when it is a boolean, else [false]. *)

val int : string -> Yojson.Safe.t -> int
(** [int k j] is member [k] when it is an integer, else [0]. *)

val int_opt : string -> Yojson.Safe.t -> int option
(** [int_opt k j] is member [k] when it is an integer (or an integer literal). *)

val number : Yojson.Safe.t -> float option
(** [number v] is an integer or float value as a float. *)

val float_opt : string -> Yojson.Safe.t -> float option
(** [float_opt k j] is member [k] when it is a number ({!number}). *)

val truthy : Yojson.Safe.t option -> bool
(** [truthy v] is Python truthiness on a member: [true], a non-zero integer, a non-empty
    string or list. *)

val set : string -> Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t
(** [set k v j] replaces (or adds, at the end) member [k] of the object [j]; a non-object
    is returned unchanged. *)

val of_string_opt : string option -> Yojson.Safe.t
(** [of_string_opt s] is [`String s] or [`Null]. *)

val strings : Yojson.Safe.t list -> string list
(** [strings l] keeps the string elements of [l]. *)

val parse : string -> Yojson.Safe.t option
(** [parse s] parses one JSON document; [None] when malformed. *)

val stored_strings : string option -> string list
(** [stored_strings col] decodes a TEXT column holding a JSON array of strings (e.g.
    [consumers], [local_paths], [gh_member_orgs]); [[]] for NULL or malformed JSON. *)

val stored_assoc : string option -> (string * Yojson.Safe.t) list
(** [stored_assoc col] decodes a TEXT column holding a JSON object (e.g. [extra]); [[]]
    for NULL, malformed JSON or a non-object. *)
