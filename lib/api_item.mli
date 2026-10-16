(** The pluggable API-extractor interface and the helpers every language extractor shares
    (spec FR-120). Extraction is lightweight source scanning: no build, no toolchain, no
    network, never an exception. *)

type item = {
  kind : string;  (** Declaration keyword: [val], [type], [fn], [func], [class], … *)
  name : string;  (** The declared name. *)
  qualified_path : string;
      (** Module path and name, e.g. [Json.parse], [crate::io::read]. *)
  signature : string;  (** The declaration text (without its doc). *)
  doc : string;  (** The doc comment text, [""] when none. *)
  line : int;  (** 1-based line of the declaration. *)
}
(** One public API item. *)

type context = {
  paths : string list;  (** Every repository-relative path (tree listing / ls-files). *)
  manifests : (string * string) list;
      (** Root manifest files read for detection ({!manifest_files}): (path, text). *)
  language : string option;  (** The repository's primary language, if known. *)
}
(** What detection and selection see of a repository. *)

type extractor = {
  language : string;
      (** Lowercase name: [ocaml], [typescript], [rust], [go], [python], [c], [quint],
          [rocq]. *)
  detect : context -> bool;  (** Manifest files or primary language. *)
  select : context -> string list;  (** The interface files to read, in path order. *)
  handles : string -> bool;  (** Whether a selected path is this extractor's. *)
  extract : path:string -> string -> item list;  (** Items of one file. *)
}

val manifest_files : string list
(** Root files read for detection, package aliases and entry points: [dune-project],
    [package.json], [tsconfig.json], [Cargo.toml], [go.mod], [pyproject.toml], [setup.py],
    [setup.cfg]. *)

val skipped_dirs : string list
(** Path segments never scanned: [vendor], [node_modules], [_build], [_opam], [target],
    [dist], [build], [test], [tests], [__tests__], [testdata], [examples], [.git]. *)

val skipped : string -> bool
(** [skipped p] is [true] when [p] has a {!skipped_dirs} segment or names a generated file
    ([*.min.js], [*.pb.go], [*_generated.*], [*.generated.*], [*.gen.*], [zz_generated*]).
*)

val generated_text : string -> bool
(** [generated_text s] is [true] when the first five lines carry a generated-code marker
    ([DO NOT EDIT], [@generated], [Code generated]). *)

val has_root : context -> string -> bool
(** [has_root ctx name] is [true] when [name] is a root path of the repository. *)

val language_is : context -> string list -> bool
(** [language_is ctx names] is [true] when the primary language is one of [names]
    (ignoring case). *)

val lines : string -> string array
(** [lines s] splits [s] on ['\n'], dropping ['\r']. *)

val trim : string -> string
(** [String.trim]. *)

val strip_prefix : prefix:string -> string -> string option
(** [strip_prefix ~prefix s] is [s] without [prefix], when [s] starts with it. *)

val cap : string -> string
(** [cap s] is [s] cut to 2000 code points ({!Chunk.max_chars}). *)

val chunk : item -> Chunk.t
(** [chunk it] is the item as one chunk titled [kind ^ " " ^ qualified_path], with the
    signature, a blank line and the doc, capped by {!cap}. *)
