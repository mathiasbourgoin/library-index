(** SHA-256 (FIPS 180-4) in pure OCaml, used for [readme_sha256] (spec FR-005, C-36). The
    local switch has no digest library beyond [Digest] (MD5), and no package may be
    installed for this task. *)

val hex : string -> string
(** [hex s] is the SHA-256 of the bytes of [s] as 64 lowercase hexadecimal digits. *)
