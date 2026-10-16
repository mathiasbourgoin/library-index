(** Registration targets (spec "Registration"): a GitHub/GitLab URL (https, [git@] or
    [ssh://git@], with or without [.git]) or a local checkout directory. *)

type t = {
  target : string;
      (** As stored in [sources.toml]: the URL, or the absolute directory. A
          [scheme://user[:secret]@…] URL, or one with a query string or fragment, is
          stored as the canonical [https://github.com/<full_name>] /
          [https://gitlab.com/<full_name>] of its key, so no credential is ever kept. *)
  key : string;  (** [github:o/r], [gitlab:g/sub/p] or [local:<directory name>]. *)
  host : string;  (** [github], [gitlab] or [local]. *)
  full_name : string;  (** [o/r]; the directory name for a [local:] key. *)
  local_dir : string option;  (** The checkout, for a directory target. *)
  checkout : Local_scan.checkout option;  (** Its git facts, for a directory target. *)
}

val is_url : string -> bool
(** [is_url s] is [true] for an [https://], [http://], [git@] or [ssh://] string. *)

val parse :
  id:Identity.t -> call:Fetch.call -> home:string option -> string -> (t, string) result
(** [parse ~id ~call ~home s] resolves a target. A URL must name a GitHub or GitLab
    repository ({!Remote.norm_remote}). A directory ([~] expanded with [home], made
    absolute) must be a git checkout: its key is its [origin] (else first) forge remote,
    else [local:<lowercased directory name>]; its remotes and commit counts come from
    read-only git ({!Local_scan.checkout}). Anything else is an error naming the target.
*)

val lookup_key : string -> string option
(** [lookup_key s] is the key a target string denotes without any I/O: a forge URL's key,
    or [s] itself when it already is a key ([github:…], [gitlab:…], [local:…]). *)
