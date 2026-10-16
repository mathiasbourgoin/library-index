(** [libindex://] resource URIs (spec FR-092, FR-093, US-13).

    - A repository: [libindex://repo/{host}/{owner}/{name}]; a [local] repository is
      [libindex://repo/local/_/{name}].
    - A document: [libindex://doc/{host}/{owner}/{name}/{path}].

    Each variable is one percent-encoded segment: every byte outside [A-Za-z0-9-._~] is
    encoded, so a GitLab owner with subgroups ([group/sub]) and a document path keep their
    [/] as [%2F]. Whitehat rows have no URI. *)

val repo_template : string
(** [libindex://repo/{host}/{owner}/{name}]. *)

val doc_template : string
(** [libindex://doc/{host}/{owner}/{name}/{path}]. *)

val encode : string -> string
(** [encode s] percent-encodes every byte of [s] outside [A-Za-z0-9-._~]. *)

val decode : string -> string option
(** [decode s] reverses {!encode} (any [%XX] is decoded); [None] on a malformed escape. *)

val repo : host:string -> owner:string -> name:string -> string option
(** [repo ~host ~owner ~name] is the repository URI; [None] for a [whitehat] row or any
    host other than [github], [gitlab] and [local]. *)

val doc : host:string -> owner:string -> name:string -> string -> string option
(** [doc ~host ~owner ~name path] is the document URI ([None] as for {!repo}). *)

type target =
  | Repo of string  (** Index key. *)
  | Doc of string * string  (** Key, path. *)

val parse : string -> target option
(** [parse uri] is the index key (and document path) a URI designates: [github:o/n],
    [gitlab:o/n] (keys are lowercase) or [local:n]. [None] for any other scheme, host,
    shape, an empty segment, a malformed escape, or a [?]/[#] part. A document path may be
    one encoded segment or several raw segments. *)
