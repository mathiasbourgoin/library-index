(** GitHub/GitLab remote URLs (spec C-18; port of the prototype's [norm_remote]). *)

val norm_remote : string -> string option
(** [norm_remote url] maps a [git@], [ssh://git@] or [https://] GitHub/GitLab remote to
    its lowercased key ([github:o/r], [gitlab:g/sub/p]), dropping userinfo, a query string
    or fragment (they may carry an access token), a trailing [.git] and [/]; [None] for
    other URLs. *)
