(** Uniform argument validation for tool/CLI inputs (AM-8): the CLI and the MCP server
    both build a JSON object and go through these readers, so messages are identical. *)

type obj = (string * Yojson.Safe.t) list
(** The members of an arguments object. *)

val members : Yojson.Safe.t -> (obj, string) result
(** [members v] is the members of an object ([null] is read as [{}]); otherwise
    ["arguments must be a JSON object"]. *)

val object_ : allowed:string list -> Yojson.Safe.t -> (obj, string) result
(** [object_ ~allowed v] accepts an object (or [null], read as [{}]) whose keys are all in
    [allowed]; otherwise ["unknown argument '<k>'; allowed: …"] or
    ["arguments must be a JSON object"]. *)

val req_string : obj -> string -> (string, string) result
(** [req_string o k] reads a required string: ["missing required argument '<k>'"] or
    ["argument '<k>' must be a string"]. *)

val opt_string : obj -> string -> (string option, string) result
(** [opt_string o k] reads an optional string; [null] is absent. *)

val opt_int : obj -> string -> (int option, string) result
(** [opt_int o k] reads an optional integer (an integral float is accepted); [null] is
    absent. *)

val opt_bool : obj -> string -> (bool option, string) result
(** [opt_bool o k] reads an optional boolean: ["argument '<k>' must be a boolean"]; [null]
    is absent. *)

val opt_object : obj -> string -> (obj option, string) result
(** [opt_object o k] reads an optional object; [null] is absent. *)

val clamp : lo:int -> hi:int -> int -> int
(** [clamp ~lo ~hi n] bounds [n] to [lo..hi]. *)
