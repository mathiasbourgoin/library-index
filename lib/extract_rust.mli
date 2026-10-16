(** Rust API extractor (spec FR-120): for every crate (a [src/lib.rs], at the root or in a
    workspace member), the [.rs] files of its [src/] except [main.rs] and [bin/]; items
    are the [pub] (not [pub(crate)]) [fn] / [struct] / [enum] / [trait] / [type] / [const]
    / [static] / [mod] / [union] declarations with their [///] docs (attributes in between
    are skipped), a [pub fn] inside an [impl Type] block being [Type::fn]; a file's
    leading [//!] docs give a [mod] item. Qualified paths start with [crate] (or the
    workspace member's name) and follow the module files. *)

val extractor : Api_item.extractor
(** The Rust extractor. *)

val module_path : string -> string
(** [module_path path] is the module of a source file: [src/lib.rs] → [crate],
    [src/a/mod.rs] → [crate::a], [src/a/b.rs] → [crate::a::b]. *)
