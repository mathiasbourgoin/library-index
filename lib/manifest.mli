(** Package names declared by a repository's manifests, recorded as its [aliases] so that
    a library is found under its package name (spec Amendment 2 S14b, S12 deviation 5: the
    package [json-kit] of the repository [ocaml-json]). Pure. *)

val aliases : paths:string list -> manifests:(string * string) list -> string list
(** [aliases ~paths ~manifests] is, lowercased, deduplicated and sorted: the basename of
    every root [*.opam] of [paths], the [name] of [package.json] (a scoped [@s/n] gives
    both [@s/n] and [n]), the [[package]] name of [Cargo.toml], the [[project]] (or
    [[tool.poetry]]) name of [pyproject.toml], the last segment of [go.mod]'s module path
    (a [vN] suffix skipped) and the [(name …)] of [dune-project]. [manifests] are (root
    path, text) pairs. *)
