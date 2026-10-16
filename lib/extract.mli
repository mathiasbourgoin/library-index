(** The registry of API extractors and their use by the documents store and the scanned
    API items (spec FR-120, AC-36, CHECK-37). The languages are OCaml,
    TypeScript/JavaScript, Rust, Go and Python (core batch), C/C++, Quint and Rocq (extras
    batch). A repository of an unknown language yields no API file: its documents are its
    README and markdown only. *)

val core : Api_item.extractor list
(** Every extractor, in detection order: [ocaml], [typescript], [rust], [go], [python],
    [c], [quint], [rocq]. *)

val detected : Api_item.context -> Api_item.extractor list
(** [detected ctx] is the extractors whose [detect] accepts [ctx]. *)

val select : Api_item.context -> string list
(** [select ctx] is the union of the detected extractors' selections, sorted. *)

val for_path : string -> Api_item.extractor option
(** [for_path p] is the first extractor that handles [p]'s extension. *)

val items : path:string -> string -> Api_item.item list
(** [items ~path text] is the items of one file ([[]] for a generated file
    ({!Api_item.generated_text}) or an unhandled extension). Never raises. *)

val chunks : path:string -> string -> Chunk.t list
(** [chunks ~path text] is one chunk per item ({!Api_item.chunk}). *)
