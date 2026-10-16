(* Shared CLI plumbing: output conventions (one JSON document on stdout; errors on stderr
   as "library-index: <message>" with exit code 1), the DB/clock options and the filter
   and optional-parameter flags shared by the query commands. *)

open Cmdliner
open Library_index

let fail msg =
  prerr_endline ("library-index: " ^ Utf8.sanitize msg);
  1

(* Output is always valid UTF-8 JSON (AM-20). *)
let print_json j =
  print_string (Yojson.Safe.pretty_to_string (Utf8.sanitize_json j));
  print_newline ();
  0

let report = function Ok j -> print_json j | Error e -> fail e

(* ------------------------------------------------------------ common options *)

let db_arg =
  let doc =
    "SQLite database path. Defaults to $(b,LIBRARY_INDEX_DB), else library-index.db in \
     the data directory."
  in
  Arg.(value & opt (some string) None & info [ "db" ] ~docv:"PATH" ~doc)

let data_dir_arg =
  let doc =
    "Data directory (sources.toml, the database, the curated exports, INDEX.md). \
     Defaults to $(b,LIBRARY_INDEX_DATA), else the directory of an explicit database \
     ($(b,--db) or $(b,LIBRARY_INDEX_DB)), else $(b,\\$XDG_DATA_HOME/library-index) \
     ($(b,~/.local/share/library-index))."
  in
  Arg.(value & opt (some string) None & info [ "data-dir" ] ~docv:"DIR" ~doc)

let export_dir_arg =
  let doc =
    "Directory receiving curated.json and whitehat_tools.json after curated writes. \
     Defaults to the data directory."
  in
  Arg.(value & opt (some string) None & info [ "export-dir" ] ~docv:"DIR" ~doc)

let now_arg =
  let doc =
    "Clock used for status computation (RFC 3339). Defaults to the current time."
  in
  Arg.(value & opt (some string) None & info [ "now" ] ~docv:"TIME" ~doc)

let clock = function
  | None -> Ok Ctx.system_clock
  | Some s -> (
      match Status.parse_time s with
      | Some t -> Ok (fun () -> t)
      | None -> Error (Printf.sprintf "invalid --now '%s' (expected RFC 3339)" s))

(* The data directory, its database path and its parsed sources.toml. *)
let locate ~db ~data_dir =
  let ( let* ) = Result.bind in
  let* data_dir = Ctx.resolve_data_dir ~data_dir ~db in
  let* config, identity = Ctx.load_config data_dir in
  Ok (data_dir, Ctx.resolve_db_path ~db ~data_dir, config, identity)

let with_ctx db data_dir export_dir now f =
  match (locate ~db ~data_dir, clock now) with
  | Error e, _ | _, Error e -> fail e
  | Ok (data_dir, db_path, config, identity), Ok now -> (
      match Db.open_existing db_path with
      | Error e -> fail e
      | Ok db ->
          let export_dir = Option.value export_dir ~default:data_dir in
          let ctx = { Ctx.db; db_path; data_dir; export_dir; config; identity; now } in
          Fun.protect ~finally:(fun () -> Db.close db) (fun () -> f ctx))

type ctx_args = {
  db : string option;
  data_dir : string option;
  export_dir : string option;
  now : string option;
}

let ctx_term =
  Term.(
    const (fun db data_dir export_dir now -> { db; data_dir; export_dir; now })
    $ db_arg $ data_dir_arg $ export_dir_arg $ now_arg)

let run_ctx (a : ctx_args) f = with_ctx a.db a.data_dir a.export_dir a.now f

let filter_opt name doc =
  Arg.(
    value
    & opt (some string) None
    & info [ name ] ~docv:(String.uppercase_ascii name) ~doc)

(* Filter flags become a JSON object validated by the shared layer (same messages as MCP). *)
let filters_term =
  let pair name =
    Term.(
      const (Option.map (fun v -> (name, `String v)))
      $ filter_opt name ("Filter on " ^ name ^ "."))
  in
  let bool_pair name flag =
    Term.(
      const (Option.map (fun v -> (name, `String v)))
      $ filter_opt flag ("Filter on " ^ name ^ " (true|false)."))
  in
  Term.(
    const (fun a b c d e f g h i j ->
        List.filter_map Fun.id [ a; b; c; d; e; f; g; h; i; j ])
    $ pair "host" $ pair "owner" $ pair "language" $ pair "tier" $ pair "status"
    $ pair "domain" $ pair "private"
    $ bool_pair "maintained_only" "maintained-only"
    $ bool_pair "has_docs" "has-docs"
    $ pair "ownership")

let limit_arg doc = Arg.(value & opt (some int) None & info [ "limit" ] ~docv:"N" ~doc)

(* The optional richer parameters (US-11), shared by search, resolve and docs. *)
let rich_term ~kind =
  let detail =
    Arg.(
      value
      & opt (some string) None
      & info [ "detail" ] ~docv:"LEVEL" ~doc:"brief | standard (default) | full.")
  in
  let budget =
    Arg.(
      value
      & opt (some int) None
      & info [ "budget-chars" ] ~docv:"N"
          ~doc:"Cut the JSON result to N characters (100..1000000); sets truncated.")
  in
  let kind_arg =
    Arg.(
      value
      & opt (some string) None
      & info [ "kind" ] ~docv:"KIND" ~doc:"Document kind: readme | doc | api.")
  in
  let members d b k =
    List.filter_map Fun.id
      [
        Option.map (fun v -> ("detail", `String v)) d;
        Option.map (fun v -> ("budget_chars", `Int v)) b;
        Option.map (fun v -> ("kind", `String v)) k;
      ]
  in
  if kind then Term.(const members $ detail $ budget $ kind_arg)
  else Term.(const (fun d b -> members d b None) $ detail $ budget)

let opt_int k = function Some n -> [ (k, `Int n) ] | None -> []
