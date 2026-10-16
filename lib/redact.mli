(** Secret redaction applied on every text write path (spec C-32, FR-070, AM-19, AM-20).

    The pattern list is closed: GitHub tokens ([gh[pousr]_…], [github_pat_…]), GitLab
    tokens ([glpat-…], [glptt-…], runner/deploy/CI-build tokens [glrt-…], [gldt-…],
    [glcbt-…]), Slack tokens ([xox[abpr]-…]), [sk-…] keys including the dashed [sk-ant-…],
    [sk-proj-…], [sk-svcacct-…], [sk-admin-…] forms, npm tokens, AWS access key ids
    ([AKIA…]), base58check secret keys, unencrypted ([edsk…], [spsk…], [p2sk…], [BLsk…])
    or encrypted ([edesk…], [spesk…], [p2esk…], [BLesk…]), three-segment JWTs
    ([eyJ….eyJ….…]), [Bearer <credential>] values (the word [Bearer] is kept), PEM
    private-key blocks and PGP [PRIVATE KEY BLOCK] armour, and URL userinfo. Matches are
    replaced by {!marker}; URL userinfo ([scheme://user:pass@host]) is dropped. False
    positives are accepted (EC-28). The text is first made valid UTF-8 ({!Utf8.sanitize}).
*)

val marker : string
(** The replacement text, ["[redacted]"]. *)

val text : string -> string
(** [text s] is [s] made valid UTF-8, with every closed-list secret pattern replaced.
    Idempotent. *)

val text_opt : string option -> string option
(** [text_opt] maps {!text} over an option. *)

val contains_secret : string -> bool
(** [contains_secret s] is [true] iff {!text} would change [s]. *)

val json : Yojson.Safe.t -> Yojson.Safe.t
(** [json v] applies {!text} to every string value (not object keys) of [v]. *)
