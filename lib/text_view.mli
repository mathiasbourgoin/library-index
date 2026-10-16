(** Short text renderings of tool results, for MCP clients that ignore [structuredContent]
    (spec FR-092). Each is at most {!cap} code points, cut on a UTF-8 boundary with a
    closing note, unless [full] (the [detail = full] request), which renders the complete
    JSON. *)

type tool =
  | Search
  | Get
  | List
  | Resolve
  | Query_docs
  | Find_capability
  | Copies
  | Explain
  | Whats_new
  | Other

val cap : int
(** [2000] code points. *)

val render : ?full:bool -> tool -> Yojson.Safe.t -> string
(** [render ?full tool json] is the rendering of a successful result of [tool]. *)
