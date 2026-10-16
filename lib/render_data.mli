(** The data the rendered views are built from, loaded from the DB, plus the formatting
    helpers shared by {!Render_md} and {!Render_html} (spec FR-050, FR-051, AM-3, C-25;
    ports [build_json], the counts and the gap lines of [refresh.py] at b70f23d).

    Only fetched rows that are not [missing_since] are rendered (stubs, whitehat rows and
    rows registered with [confidential = "exclude-from-page"] are excluded from the repo
    sections; [external] rows are not counted as Tier 1), in key order, so that every
    stable sort below mirrors Python's [sorted] over [build_json]'s key-ordered rows.
    README text is never loaded. *)

type repo = {
  key : string;
  host : string;
  owner : string;
  name : string;
  full_name : string;
  url : string option;
  private_ : bool;
  fork : bool;
  archived : bool;
  language : string option;
  description : string;  (** Stored (campaign-withheld) description, [""] when NULL. *)
  synopsis : string option;
  what : string option;  (** Curated. *)
  interest : string option;  (** Curated. *)
  domain : string option;  (** Effective domain: curated, else computed. *)
  last_commit : string option;
  pushed_at : string option;
  status : string;  (** Effective status at the render clock. *)
  tier : string;
  role : string option;
  evidence : string;
  local_paths : string list;
  parent : string option;
  fork_note : string option;  (** {!Classify.fork_note} of the row. *)
  campaign : bool;  (** The stored campaign flag (AM-17). *)
  ownership : string;
      (** [own] | [participant] | [external] (override, else inferred). *)
  registered : bool;  (** Explicitly registered ([[[repo]]] entry). *)
}
(** One rendered repository. *)

type local = {
  path : string;
  remote_key : string option;
  last_commit : string option;
  my_commits : int;
  total_commits : int;
}
(** One local checkout of section 4. *)

type tool = {
  campaign : string;
  tool_path : string;
  tool_what : string;
  tool_language : string;
  tool_last_commit : string;
  reuse : string;
}
(** One whitehat tool row (section 6), in file order. *)

type settings = {
  identity : Identity.t;
      (** Sources line, collaborator section, counts labels, domains. *)
  title : string;  (** [render.title]. *)
  how_to : string list;  (** The "How to use" bullets, one line each. *)
  inherited_examples : string;  (** [render.inherited_examples], [""] for none. *)
  contributes_column : string;  (** Section 5's last column header. *)
  whitehat_where : string;  (** Section 6's "(…)" location note, [""] for none. *)
  fixed_gaps : string list;  (** Gap lines rendered after the stored ones. *)
  db_display : string;  (** The database path, [~]-abbreviated. *)
  index_display : string;  (** The INDEX.md path, [~]-abbreviated. *)
}
(** What the views take from [sources.toml] and the data directory (FR-101): no text
    naming a person, organisation or path is built into the code. *)

val default_how_to : notes_display:string option -> string list
(** The generic "How to use" bullets: search first, prefer upstream reuse, curate through
    the tool, refresh then render, and, with a notes directory, where the page is. *)

val default_fixed_gaps : string list
(** The generic fixed gap line (GitLab languages and sparing contributor statistics). *)

val settings :
  Config.t ->
  identity:Identity.t ->
  db_path:string ->
  index_md:string ->
  notes_dir:string option ->
  home:string ->
  settings
(** [settings c ~identity ~db_path ~index_md ~notes_dir ~home] takes [c.render] with its
    defaults ({!default_how_to}, {!default_fixed_gaps}) and abbreviates paths with
    {!tilde}. *)

type t = {
  settings : settings;
  generated : string;  (** Local date of the render clock, [YYYY-MM-DD]. *)
  repos : repo list;  (** Key order. *)
  section4 : local list;  (** Local-only checkouts, then unindexed ones, in scan order. *)
  tools : tool list;
  gaps : string list;
      (** Stored gaps, the unreachable-remote line, then [settings.fixed_gaps]. *)
  counts : (string * string) list;  (** The prototype's Counts, in order. *)
  member_orgs : string list;  (** [meta.gh_member_orgs]. *)
}
(** Everything a view needs. *)

val load : Db.t -> settings:settings -> now:Ptime.t -> t
(** [load db ~settings ~now] reads the view data. Never-refreshed DBs give empty lists and
    zero counts (EC-23). *)

val local_date : Ptime.t -> string
(** [local_date t] is [t] as a [YYYY-MM-DD] date in the local time zone (C-26). *)

(** {2 Shared formatting} *)

val d10 : string option -> string
(** [d10 s] is the first 10 code points of [s] ([""] for [None]). *)

val first_set : string option list -> string option
(** [first_set l] is the first [Some] non-empty string (Python's [a or b]). *)

val last_date : repo -> string option
(** [last_date r] is [last_commit] or [pushed_at]. *)

val date_number : string option -> int
(** [date_number s] is the integer made of the digits of [d10 s], 0 when none. *)

val role_rank : string option -> int
(** [role_rank] orders roles for section 1 (owner 0 … occasional 5, others 9). *)

val is_campaign : repo -> bool
(** The row's stored campaign flag (AM-17, {!Classify.campaign_flag}): owner or any local
    checkout remote is a campaign owner. *)

val what : repo -> string
(** {!Classify.what_fallback} of the row. *)

val tilde : home:string -> string -> string
(** [tilde ~home p] replaces every occurrence of [home] in [p] with [~] (Python
    [str.replace]); [p] unchanged when [home] is empty. *)

val replace_all : sub:string -> by:string -> string -> string
(** [replace_all ~sub ~by s] replaces every non-overlapping occurrence, left to right. *)

val stable_sort_by : ('a -> 'k) -> ('k -> 'k -> int) -> 'a list -> 'a list
(** [stable_sort_by key cmp l] is Python's [sorted(l, key=key)] with [cmp] on keys. *)

val external_registered : t -> repo list
(** [external_registered t] is section 8: the registered repositories whose ownership is
    [external] and that are not campaign rows, in key order. *)

val in_section7 : repo -> bool
(** [in_section7 r] is section 7's filter: tier ["2"], except the registered external
    repositories (section 8). *)
