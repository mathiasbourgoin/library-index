(** [INDEX.md] rendering (spec FR-050, FR-051, AM-1, AM-3, AM-4; port of [render_md] of
    [refresh.py] at b70f23d).

    Sections 1–7 reproduce the prototype byte for byte; the header keeps "Sources:",
    Counts and Gaps as the prototype computes them, while the generated-by line and the
    "How to use this index" bullets describe the [library-index] CLI and MCP flow (AM-4).
    Campaign repos are kept out of the section-1 table and section 2 and are named only in
    the workspaces line; README text is never emitted. *)

val md_esc : string -> string
(** [md_esc s] escapes [|] as [\|] and replaces newlines with spaces. *)

val render : home:string -> Render_data.t -> string
(** [render ~home data] is the whole [INDEX.md]. [home] (the real [$HOME] at render time)
    is replaced by [~] in local paths. *)
