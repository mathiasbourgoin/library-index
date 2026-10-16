(** The self-contained HTML view written to the notes directory (spec FR-050, FR-051,
    AM-4; port of [render_html] of [refresh.py] at b70f23d).

    Same 7 sections, columns, CSS and JS as the prototype; only the "Generated … by" meta
    line names the new tool. Campaign repos are kept out of the section-1 and section-2
    tables; README text is never emitted. *)

val escape : string -> string
(** [escape s] is Python's [html.escape(s, quote=True)]: ampersand, less-than,
    greater-than, double quote and single quote become [&amp;], [&lt;], [&gt;], [&quot;]
    and [&#x27;]. *)

val render : home:string -> Render_data.t -> string
(** [render ~home data] is the whole page ([<title>Library index</title>]). *)
