(* Query commands: get, search, list, resolve, docs. Each builds a JSON argument object
   and goes through the shared Ops layer, as the MCP tools do (AM-8). *)

open Cmdliner
open Library_index
open Cli

(* ------------------------------------------------------------ queries *)

let repo_pos = Arg.(required & pos 0 (some string) None & info [] ~docv:"REPO")

let get_cmd =
  let run c repo =
    run_ctx c (fun ctx -> report (Ops.get ctx (`Assoc [ ("repo", `String repo) ])))
  in
  Cmd.v
    (Cmd.info "get"
       ~doc:"Print one repository entry as JSON (key, owner/name or bare name).")
    Term.(const run $ ctx_term $ repo_pos)

let search_cmd =
  let query = Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY") in
  let run c query limit filters rich =
    let args =
      `Assoc
        ([ ("query", `String query); ("filters", `Assoc filters) ]
        @ opt_int "limit" limit @ rich)
    in
    run_ctx c (fun ctx -> report (Ops.search ctx args))
  in
  Cmd.v
    (Cmd.info "search"
       ~doc:
         "Ranked full-text search; prints {query, mode, count, hits} as JSON. Filters: \
          --host, --owner, --language, --tier, --status, --domain, --private, \
          --maintained-only, --has-docs (true|false).")
    Term.(
      const run $ ctx_term $ query
      $ limit_arg "Maximum hits, 1..50 (default 10)."
      $ filters_term $ rich_term ~kind:true)

let resolve_cmd =
  let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME") in
  let query = Arg.(value & pos 1 (some string) None & info [] ~docv:"QUERY") in
  let run c name query filters rich =
    let args =
      `Assoc
        ([ ("libraryName", `String name); ("filters", `Assoc filters) ]
        @ (match query with Some q -> [ ("query", `String q) ] | None -> [])
        @ rich)
    in
    run_ctx c (fun ctx -> report (Ops.resolve_library_id ctx args))
  in
  Cmd.v
    (Cmd.info "resolve"
       ~doc:
         "Candidate library ids for NAME (same as the MCP tool resolve-library-id); \
          prints {candidates, hint?} as JSON. Same filter flags as $(b,search).")
    Term.(const run $ ctx_term $ name $ query $ filters_term $ rich_term ~kind:false)

let docs_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let query = Arg.(required & pos 1 (some string) None & info [] ~docv:"QUERY") in
  let run c id query limit rich =
    let args =
      `Assoc
        ([ ("libraryId", `String id); ("query", `String query) ]
        @ opt_int "limit" limit @ rich)
    in
    run_ctx c (fun ctx -> report (Ops.query_docs ctx args))
  in
  Cmd.v
    (Cmd.info "docs"
       ~doc:
         "Ranked excerpts of library ID's documents for QUERY (same as the MCP tool \
          query-docs); prints {library, rules?, excerpts, hint?} as JSON.")
    Term.(
      const run $ ctx_term $ id $ query
      $ limit_arg "Maximum excerpts, 1..20 (default 5)."
      $ rich_term ~kind:true)

let list_cmd =
  let offset =
    Arg.(value & opt (some int) None & info [ "offset" ] ~docv:"N" ~doc:"Rows to skip.")
  in
  let run c limit offset filters =
    let num k = function Some n -> [ (k, `Int n) ] | None -> [] in
    run_ctx c (fun ctx ->
        report (Ops.list ctx (`Assoc (filters @ num "limit" limit @ num "offset" offset))))
  in
  Cmd.v
    (Cmd.info "list"
       ~doc:
         "List entries sorted by key; prints {total, limit, offset, count, repos} as \
          JSON. Same filter flags as $(b,search).")
    Term.(
      const run $ ctx_term
      $ limit_arg "Maximum rows, 1..1000 (default 100)."
      $ offset $ filters_term)

(* ------------------------------------------------------------ extras (US-12) *)

let find_cmd =
  let query = Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY") in
  let run c query limit filters rich =
    let args =
      `Assoc
        ([ ("query", `String query); ("filters", `Assoc filters) ]
        @ opt_int "limit" limit @ rich)
    in
    run_ctx c (fun ctx -> report (Ops.find_capability ctx args))
  in
  Cmd.v
    (Cmd.info "find"
       ~doc:
         "Which library already provides QUERY, and with which item (same as the MCP \
          tool find-capability); prints {query, mode, count, results} as JSON. Same \
          filter flags as $(b,search).")
    Term.(
      const run $ ctx_term $ query
      $ limit_arg "Maximum results, 1..50 (default 10)."
      $ filters_term $ rich_term ~kind:false)

let ownership_opt =
  Arg.(
    value
    & opt (some string) None
    & info [ "ownership" ] ~docv:"KIND" ~doc:"Only own | participant | external entries.")

let opt_str k = function Some v -> [ (k, `String v) ] | None -> []

let copies_cmd =
  let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME") in
  let run c name ownership =
    run_ctx c (fun ctx ->
        report
          (Ops.copies ctx
             (`Assoc ([ ("name", `String name) ] @ opt_str "ownership" ownership))))
  in
  Cmd.v
    (Cmd.info "copies"
       ~doc:
         "Every copy of library NAME and the maintained one (same as the MCP tool \
          copies); prints {name, count, groups} as JSON.")
    Term.(const run $ ctx_term $ name $ ownership_opt)

let explain_cmd =
  let run c repo =
    run_ctx c (fun ctx -> report (Ops.explain ctx (`Assoc [ ("repo", `String repo) ])))
  in
  Cmd.v
    (Cmd.info "explain"
       ~doc:
         "Everything needed for a reuse decision about REPO (same as the MCP tool \
          explain), as JSON.")
    Term.(const run $ ctx_term $ repo_pos)

let whats_new_cmd =
  let since = Arg.(required & pos 0 (some string) None & info [] ~docv:"SINCE") in
  let run c since limit ownership =
    run_ctx c (fun ctx ->
        report
          (Ops.whats_new ctx
             (`Assoc
                ([ ("since", `String since) ]
                @ opt_int "limit" limit @ opt_str "ownership" ownership))))
  in
  Cmd.v
    (Cmd.info "whats-new"
       ~doc:
         "Repositories changed on or after SINCE (RFC 3339 or YYYY-MM-DD), newest first \
          (same as the MCP tool whats-new), as JSON.")
    Term.(
      const run $ ctx_term $ since
      $ limit_arg "Maximum repositories, 1..100 (default 20)."
      $ ownership_opt)
