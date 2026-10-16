(** Import of [curated.json] / [whitehat_tools.json] into the DB, and their export (spec
    FR-001, FR-002, FR-004, C-2..C-5, C-10, AM-5, EC-1, EC-2).

    Curated entries become rows keyed by their (lowercased) key: a stub row
    ([fetched = 0], identity derived from the key) is inserted when none exists, and the
    curated columns are replaced from the file. Recognised fields are [domain], [what],
    [interest], [status_override], [maintained_side], [notes], [rules] (strings),
    [consumers] (array of strings) and [usage] (array of [{consumer, note?, at}]); any
    other field, or a recognised field with an unexpected JSON type, is kept verbatim in
    the [extra] column and re-exported (no loss). JSON [null] fields are treated as
    absent. Whitehat tools become rows of host [whitehat] keyed {!Key.whitehat}, with
    [owner = campaign], [full_name = path], [interest = reuse], [tier = "whitehat"] and
    [ord] = file position. [_doc] / [_policy] are kept in [meta] and re-emitted verbatim.
    Every text value goes through {!Redact}. *)

type usage = { consumer : string; note : string option; at : string }
(** One usage record. *)

type entry = {
  key : string;
  stub : Key.stub;
  strings : (string * string) list;
  consumers : string list option;
  usage : usage list;
  extra : (string * Yojson.Safe.t) list;
}
(** One parsed curated entry; [strings] maps column names to values. *)

type curated = { doc : Yojson.Safe.t option; entries : entry list }
(** Parsed [curated.json]. *)

type tool = {
  campaign : string;
  path : string;
  fields : (string * string option) list;
  tool_extra : (string * Yojson.Safe.t) list;
}
(** One whitehat tool: [fields] holds [what], [language], [last_commit], [reuse]. *)

type whitehat = { policy : Yojson.Safe.t option; tools : tool list }
(** Parsed [whitehat_tools.json]. *)

type summary = { curated : int; whitehat : int; usage_added : int; usage_removed : int }
(** Counts of imported entries and tools, and of the usage rows inserted and deleted to
    make each imported entry's usage equal to the file's. *)

val string_fields : string list
(** Curated string columns, in export order: domain, what, interest, status_override,
    maintained_side, notes, rules. *)

val read_json : string -> (Yojson.Safe.t, string) result
(** [read_json path] parses a JSON file; errors name the file, never echo its content. *)

val parse_curated : Yojson.Safe.t -> (curated, string) result
(** [parse_curated v] validates [curated.json]. Unknown host prefixes, non-object entries,
    malformed usage records or unsupported [_]-keys are errors naming the key (EC-1). *)

val parse_whitehat : Yojson.Safe.t -> (whitehat, string) result
(** [parse_whitehat v] validates [whitehat_tools.json]: [_policy] plus [tools], each with
    string [campaign] and [path]; duplicate keys are errors. *)

val import : id:Identity.t -> ?prefer_file:bool -> Db.t -> curated -> whitehat -> summary
(** [import ~id db c w] upserts both inputs (a stub's campaign flag comes from its owner
    and [id]). It must run inside a transaction ({!Db.with_txn}). Every entry's curated
    fields are replaced by the file's, except with [prefer_file] (default [false]), where
    a field the entry does not carry keeps the database's value. For every entry of [c],
    the row's usage becomes exactly the entry's [usage] list, compared as a multiset of
    redacted [(consumer, note, at)]: matching rows are kept, the other rows deleted and
    the missing records inserted (an entry without [usage] ends with none). Rows whose key
    is not in [c] keep their usage. *)

val import_files :
  id:Identity.t ->
  Db.t ->
  curated_path:string ->
  whitehat_path:string ->
  ?prefer_file:bool ->
  unit ->
  (summary, string) result
(** [import_files ... ()] reads and parses both files, then imports them in one
    transaction. Unless [prefer_file] (default [false]), it refuses, writing nothing,
    while [meta.export_dirty] is set (AM-10, FR-129): the DB may then hold curated data
    the files lack, which the upsert and the usage replacement would overwrite or delete.
    The refusal names [--prefer-file]; it is checked inside the transaction. *)

val curated_json : Db.t -> Yojson.Safe.t
(** [curated_json db] is the [curated.json] value: [_doc] first, then every non-whitehat
    row with curated data, usage or extra fields, sorted by key. Null fields are omitted.
*)

val whitehat_json : Db.t -> Yojson.Safe.t
(** [whitehat_json db] is the [whitehat_tools.json] value: [_policy] then [tools] in [ord]
    order with fields [campaign, path, what, language, last_commit, reuse] (+ extra). *)
