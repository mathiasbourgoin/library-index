(** Which files of a repository become documents, and how they are chunked (spec FR-081,
    FR-083, FR-084). Pure: works on a list of repository-relative paths (from
    [git ls-files] or a forge tree listing).

    Order of selection:
    + the README (a root [README], [README.md], [README.markdown], [README.org],
      [README.txt] or [README.rst], case-insensitive; [.md] preferred), kind [readme];
    + [docs/**/*.md], kind [doc];
    + other root [*.md], except LICENSE, CHANGELOG, CODE_OF_CONDUCT and CONTRIBUTING
      (case-insensitive), kind [doc].

    Only when there is no markdown besides the README, public interface files are taken
    instead, kind [api]: those the detected per-language extractors select
    ({!Extract.select}, spec FR-120: OCaml, TypeScript/JavaScript, Rust, Go, Python,
    C/C++, Quint, Rocq; {!Api_item.skipped} paths never). A repository of no detected
    language gets none. At most {!max_files} paths are selected. Paths are sorted within
    each group. *)

type kind = Readme | Doc | Api

val kind_name : kind -> string
(** ["readme"], ["doc"] or ["api"]. *)

val kinds : string list
(** [["readme"; "doc"; "api"]]. *)

val max_files : int
(** [200] files per repository. *)

val select :
  ?language:string ->
  ?manifests:(string * string) list ->
  string list ->
  (string * kind) list
(** [select ?language ?manifests paths] is the selected [(path, kind)] list, in the order
    above. Duplicates, empty paths and absolute or [..] paths are ignored. [language] is
    the repository's primary language (default: {!Classify.language_of_files} of [paths]);
    [manifests] the root manifest files read ({!Api_item.manifest_files}), which detection
    and the TypeScript entry points use. *)

val api_paths :
  ?language:string -> ?manifests:(string * string) list -> string list -> string list
(** [api_paths ?language ?manifests paths] is every file the detected extractors select
    ({!Extract.select}), whatever the documentation gating above: the files scanned for
    the [api_items] of source [scan]. At most {!max_files}, sorted. *)

type format = Markdown | Mli | Plain

val format : string -> format
(** [format path]: [.md]/[.markdown] and an extension-less README are [Markdown]; [.mli]
    is [Mli]; anything else is [Plain]. *)

val title : string -> string -> string
(** [title path text] is the document title: the first level-1 heading of a markdown file,
    the module name of an OCaml [.mli] / [.ml] (["Itf"] for [src/itf.mli]), else [path].
*)

val chunks : string -> string -> Chunk.t list
(** [chunks path text] chunks markdown at its headings; a source or interface file an
    extractor handles gives one chunk per API item ({!Extract.chunks}, titled
    [kind qualified_path]); anything else is one plain chunk titled with its path. *)
