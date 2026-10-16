(* library-index CLI. Query commands print one JSON document (pretty-printed) on stdout;
   errors go to stderr as "library-index: <message>" with exit code 1. In `mcp` mode
   stdout carries JSON-RPC only. *)

open Cmdliner
open Library_index
open Cli
open Query_cmd

let summary_json (s : Curated_io.summary) =
  `Assoc
    [
      ("curated", `Int s.curated);
      ("whitehat", `Int s.whitehat);
      ("usage_added", `Int s.usage_added);
      ("usage_removed", `Int s.usage_removed);
    ]

(* ------------------------------------------------------------ init / import / export *)

let from_arg =
  let doc =
    "Directory holding curated.json and whitehat_tools.json to import. Defaults to the \
     data directory."
  in
  Arg.(value & opt (some string) None & info [ "from" ] ~docv:"DIR" ~doc)

let init_cmd =
  let force =
    Arg.(value & flag & info [ "force" ] ~doc:"Delete and recreate an existing DB.")
  in
  let discard =
    Arg.(
      value & flag
      & info [ "discard-unexported" ]
          ~doc:
            "With --force: recreate the DB even though it holds curated values that were \
             never exported (they are lost). Without it, --force refuses such a DB.")
  in
  let run db data_dir from force discard_unexported =
    match locate ~db ~data_dir with
    | Error e -> fail e
    | Ok (data_dir, db_path, _, id) ->
        let from_dir = Option.value from ~default:data_dir in
        report
          (Result.map summary_json
             (Init.init ~id ~db_path ~from_dir ~force ~discard_unexported ()))
  in
  Cmd.v
    (Cmd.info "init"
       ~doc:"Create the database and import the curated JSON files (no network).")
    Term.(const run $ db_arg $ data_dir_arg $ from_arg $ force $ discard)

let import_cmd =
  let prefer =
    Arg.(
      value & flag
      & info [ "prefer-file" ]
          ~doc:
            "Import even though the DB holds curated values that were never exported: \
             the files' values win. Without it, import refuses such a DB.")
  in
  let run c from prefer_file =
    run_ctx c (fun ctx ->
        let from_dir = Option.value from ~default:ctx.export_dir in
        report
          (Result.map summary_json
             (Init.import ~id:ctx.identity ctx.db ~from_dir ~prefer_file ())))
  in
  Cmd.v
    (Cmd.info "import"
       ~doc:
         "Upsert curated fields and whitehat rows from the JSON files, and make the \
          usage history of every library listed in curated.json equal to the file's \
          (records edited or removed there are replaced or deleted). Other rows and \
          fetched data are kept. Refused while the last export failed (export_dirty) \
          unless --prefer-file.")
    Term.(const run $ ctx_term $ from_arg $ prefer)

let export_cmd =
  let to_arg =
    Arg.(
      value
      & opt (some string) None
      & info [ "to" ] ~docv:"DIR" ~doc:"Output directory (default: the export directory).")
  in
  let run c to_ =
    run_ctx c (fun ctx ->
        let dir = Option.value to_ ~default:ctx.export_dir in
        report
          (Result.map
             (fun () -> `Assoc [ ("exported_to", `String dir) ])
             (Export.export ctx ~dir)))
  in
  Cmd.v
    (Cmd.info "export"
       ~doc:"Write curated.json and whitehat_tools.json from the database.")
    Term.(const run $ ctx_term $ to_arg)

(* ------------------------------------------------------------ curation *)

let split_set s =
  match String.index_opt s '=' with
  | None -> Error (Printf.sprintf "invalid --set '%s' (expected FIELD=VALUE)" s)
  | Some i ->
      let field = String.sub s 0 i
      and value = String.sub s (i + 1) (String.length s - i - 1) in
      if field = "consumers" then
        let items =
          List.filter
            (fun x -> x <> "")
            (List.map String.trim (String.split_on_char ',' value))
        in
        Ok (field, `List (List.map (fun x -> `String x) items))
      else Ok (field, `String value)

let annotate_fields json sets clears =
  let ( let* ) = Result.bind in
  let* base =
    match json with
    | None -> Ok []
    | Some s -> (
        match Jsonx.parse s with
        | Some (`Assoc m) -> Ok m
        | _ -> Error "invalid --json (expected a JSON object)")
  in
  let* sets =
    List.fold_right
      (fun s acc -> Result.bind acc (fun l -> Result.map (fun x -> x :: l) (split_set s)))
      sets (Ok [])
  in
  Ok (base @ sets @ List.map (fun f -> (f, `Null)) clears)

let annotate_cmd =
  let sets =
    Arg.(
      value & opt_all string []
      & info [ "set" ] ~docv:"FIELD=VALUE"
          ~doc:
            "Set a curated field (repeatable). For consumers, VALUE is a comma-separated \
             list.")
  in
  let clears =
    Arg.(
      value & opt_all string []
      & info [ "clear" ] ~docv:"FIELD" ~doc:"Clear a curated field (repeatable).")
  in
  let json =
    Arg.(
      value
      & opt (some string) None
      & info [ "json" ] ~docv:"OBJECT" ~doc:"Fields as a JSON object (null clears).")
  in
  let run c repo json sets clears =
    match annotate_fields json sets clears with
    | Error e -> fail e
    | Ok fields ->
        run_ctx c (fun ctx ->
            report
              (Ops.annotate ctx
                 (`Assoc [ ("repo", `String repo); ("fields", `Assoc fields) ])))
  in
  Cmd.v
    (Cmd.info "annotate"
       ~doc:
         "Set or clear curated fields (what, interest, domain, consumers, \
          status_override, maintained_side, notes), then rewrite curated.json in the \
          export directory.")
    Term.(const run $ ctx_term $ repo_pos $ json $ sets $ clears)

let record_usage_cmd =
  let consumer = Arg.(required & pos 1 (some string) None & info [] ~docv:"CONSUMER") in
  let note =
    Arg.(
      value
      & opt (some string) None
      & info [ "note" ] ~docv:"TEXT" ~doc:"What it is used for.")
  in
  let run c repo consumer note =
    let args =
      `Assoc
        ([ ("repo", `String repo); ("consumer", `String consumer) ]
        @ match note with Some n -> [ ("note", `String n) ] | None -> [])
    in
    run_ctx c (fun ctx -> report (Ops.record_usage ctx args))
  in
  Cmd.v
    (Cmd.info "record-usage"
       ~doc:
         "Record that CONSUMER uses REPO (usage row + consumers), then rewrite \
          curated.json.")
    Term.(const run $ ctx_term $ repo_pos $ consumer $ note)

(* ------------------------------------------------------------ import-raw / render *)

let import_raw_cmd =
  let raw =
    Arg.(
      value
      & opt (some string) None
      & info [ "raw" ] ~docv:"FILE"
          ~doc:
            "Prototype cache to import (default: cache/raw.json in the data directory).")
  in
  let run c raw =
    run_ctx c (fun ctx ->
        let path =
          Option.value raw ~default:(Filename.concat ctx.data_dir "cache/raw.json")
        in
        report
          (Result.map
             (fun (s : Import_raw.summary) ->
               `Assoc
                 [
                   ("repos", `Int s.repos);
                   ("local_checkouts", `Int s.local_checkouts);
                   ("gaps", `Int s.gaps);
                 ])
             (Import_raw.import_file ~id:ctx.identity ctx.db ~path)))
  in
  Cmd.v
    (Cmd.info "import-raw"
       ~doc:
         "Import the prototype's cache/raw.json (fetched metadata, local checkouts, \
          gaps, global counts) into an initialised database, offline.")
    Term.(const run $ ctx_term $ raw)

let render_cmd =
  let out =
    Arg.(
      value
      & opt (some string) None
      & info [ "out" ] ~docv:"FILE"
          ~doc:
            "INDEX.md path to write (default: render.index_md of sources.toml, else \
             INDEX.md in the data directory).")
  in
  let notes_dir =
    Arg.(
      value
      & opt (some string) None
      & info [ "notes-dir" ] ~docv:"DIR"
          ~doc:
            "Directory for the dated HTML page (default: render.notes_dir of \
             sources.toml; no page when neither is set).")
  in
  let run c out notes_dir =
    run_ctx c (fun ctx ->
        (* Without a HOME there is no "~" to substitute in local paths. *)
        let home = Result.value (Ctx.home ()) ~default:"" in
        let expand p = if home = "" then p else Config.expand_home ~home p in
        let r = ctx.config.render in
        let out =
          match (out, r.index_md) with
          | Some o, _ -> o
          | None, Some p -> expand p
          | None, None -> Filename.concat ctx.data_dir "INDEX.md"
        in
        let notes_dir =
          match notes_dir with Some d -> Some d | None -> Option.map expand r.notes_dir
        in
        let settings =
          Render_data.settings ctx.config ~identity:ctx.identity ~db_path:ctx.db_path
            ~index_md:out ~notes_dir ~home
        in
        match Render.run ctx.db ~settings ~out ~notes_dir ~now:(ctx.now ()) ~home with
        | Error e -> fail e
        | Ok o ->
            List.iter (fun w -> prerr_endline ("library-index: warning: " ^ w)) o.warnings;
            let code =
              print_json
                (`Assoc
                   [
                     ("index_md", `String o.index_md);
                     ( "notes_html",
                       match o.notes_html with Some p -> `String p | None -> `Null );
                     ("deleted", `List (List.map (fun n -> `String n) o.deleted));
                   ])
            in
            if o.warnings = [] then code else 2)
  in
  Cmd.v
    (Cmd.info "render"
       ~doc:
         "Render INDEX.md and <notes-dir>/<date>-library-index.html from the database \
          (exit 2 when the notes page could not be written). Use --now to fix the clock.")
    Term.(const run $ ctx_term $ out $ notes_dir)

(* ------------------------------------------------------------ refresh *)

let refresh_repo_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "repo" ] ~docv:"REPO"
        ~doc:"Refresh only this entry (key, owner/name or bare name).")

let refresh_full_arg =
  Arg.(
    value & flag
    & info [ "full" ]
        ~doc:"Refetch every README and detail regardless of pushed_at and age.")

let dev_root_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "dev-root" ] ~docv:"DIR"
        ~doc:
          "Directory scanned for local git checkouts (default: the first local-root \
           source of sources.toml; none when absent).")

(* The arguments go through the shared Ops layer, as for the MCP tool. *)
let no_analysis_arg =
  Arg.(
    value & flag
    & info [ "no-analysis" ]
        ~doc:
          "Skip the optional arch-index analysis a full refresh otherwise runs on the \
           Tier-1 own repositories whose local checkout changed.")

let refresh_cmd =
  let run c repo full dev_root no_analysis =
    let repo = match repo with Some r -> [ ("repo", `String r) ] | None -> [] in
    let args = `Assoc (repo @ [ ("full", `Bool full) ]) in
    let env = Repo_refresh.default_env () in
    let env = if no_analysis then { env with analyze = None } else env in
    run_ctx c (fun ctx ->
        match Ops.refresh ctx env ?dev_root ~allow_full:true args with
        | Error e -> fail e
        | Ok o ->
            List.iter prerr_endline o.lines;
            let code = print_json o.json in
            if o.complete then code else 2)
  in
  Cmd.v
    (Cmd.info "refresh"
       ~doc:
         "Update the database through read-only $(b,gh api)/$(b,glab api) GET calls and \
          local git metadata. Exit 0 when complete, 2 when partial (classified gaps \
          recorded, previous data kept), 1 on usage errors.")
    Term.(
      const run $ ctx_term $ refresh_repo_arg $ refresh_full_arg $ dev_root_arg
      $ no_analysis_arg)

(* ------------------------------------------------------------ analyze *)

let analyze_cmd =
  let repo = Arg.(required & pos 0 (some string) None & info [] ~docv:"REPO") in
  let allow_build =
    Arg.(
      value & flag
      & info [ "allow-build" ]
          ~doc:
            "Let an OCaml project be built (its own $(b,dune build)) so that \
             arch-index's CMT enricher can run. Without it nothing is built.")
  in
  let run c repo allow_build =
    let args = `Assoc [ ("repo", `String repo); ("allow_build", `Bool allow_build) ] in
    run_ctx c (fun ctx ->
        report (Ops.analyze_repo ctx (Repo_refresh.default_env ()) args))
  in
  Cmd.v
    (Cmd.info "analyze"
       ~doc:
         "Run the optional arch-index analysis of REPO's local checkout (same as the MCP \
          tool analyze-repo) and print {key, status, cause, head_sha, at, tool_version, \
          items, calls} as JSON. A skipped or failed analysis exits 0 with its cause.")
    Term.(const run $ ctx_term $ repo $ allow_build)

(* ------------------------------------------------------------ mcp *)

let mcp_cmd =
  let run c =
    run_ctx c (fun ctx ->
        match Mcp_server.server ctx (Repo_refresh.default_env ()) with
        | Error e -> fail e
        | Ok server ->
            Mcp_server.serve server;
            0)
  in
  Cmd.v
    (Cmd.info "mcp" ~doc:"Serve the MCP tools over stdio (JSON-RPC on stdout only).")
    Term.(const run $ ctx_term)

let main =
  let doc = "SQLite/FTS5 index of repositories and libraries, with an MCP server" in
  Cmd.group
    (Cmd.info "library-index" ~version:Mcp_server.version ~doc)
    [
      init_cmd;
      import_cmd;
      export_cmd;
      get_cmd;
      search_cmd;
      list_cmd;
      resolve_cmd;
      docs_cmd;
      find_cmd;
      copies_cmd;
      explain_cmd;
      whats_new_cmd;
      annotate_cmd;
      record_usage_cmd;
      import_raw_cmd;
      render_cmd;
      refresh_cmd;
      analyze_cmd;
      Register_cmd.register_cmd;
      Register_cmd.register_source_cmd;
      Register_cmd.unregister_cmd;
      Register_cmd.unregister_source_cmd;
      Register_cmd.sources_cmd;
      mcp_cmd;
    ]

let () = exit (Cmd.eval' main)
