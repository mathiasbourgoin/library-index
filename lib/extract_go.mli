(** Go API extractor (spec FR-120): the [.go] files outside tests ([_test.go]), the
    skipped directories, [internal/] and [cmd/]; a [package main] file gives nothing.
    Items are the exported (capitalised) [func]s, methods of exported types, [type] /
    [var] / [const] declarations (grouped ones included), each with the [//] comment
    directly above, and the package doc ([// Package …] above the [package] clause,
    conventionally in [doc.go]). Qualified paths are the package directory (dot-separated;
    the package name at the root), then the name. *)

val extractor : Api_item.extractor
(** The Go extractor. *)
