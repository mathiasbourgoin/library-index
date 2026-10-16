(* Registration commands: register, register-source, unregister, unregister-source and
   sources list. Arguments become the same JSON object the MCP tools take and go through
   the shared Ops layer (AM-8). *)

open Cmdliner
open Library_index
open Cli

let str_opt name ~docv doc =
  Arg.(value & opt (some string) None & info [ name ] ~docv ~doc)

let some_str k = Option.map (fun v -> (k, `String v))
let some_bool k = Option.map (fun b -> (k, `Bool b))

(* A tri-state flag: [--on] sets it, [--off] clears it, neither keeps the stored value. *)
let tri ~on ~off ~doc_on ~doc_off =
  Arg.(
    value
    & vflag None
        [ (Some true, info [ on ] ~doc:doc_on); (Some false, info [ off ] ~doc:doc_off) ])

(* Exit 0 when the fetch completed, 2 when it was partial (the entry is registered). *)
let outcome = function
  | Error e -> fail e
  | Ok (o : Register.outcome) ->
      List.iter (fun g -> prerr_endline ("library-index: gap: " ^ Redact.text g)) o.gaps;
      let code = print_json o.json in
      if o.complete then code else 2

(* The [register] flags. *)
let register_target =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"URL|PATH" ~doc:"GitHub/GitLab URL or local checkout.")

let ownership =
  str_opt "ownership" ~docv:"KIND" "own | participant | external (default: inferred)."

let confidential =
  str_opt "confidential" ~docv:"LEVEL"
    "generic | exclude-from-page | none (default none)."

let campaign =
  tri ~on:"campaign" ~off:"no-campaign"
    ~doc_on:"A campaign repository (details withheld, no documents)."
    ~doc_off:
      "Clears the entry's campaign flag (a row another campaign rule matches stays \
       withheld)."

let domain = str_opt "domain" ~docv:"DOMAIN" "Curated domain (one of the index domains)."
let notes = str_opt "notes" ~docv:"TEXT" "Curated notes."

let rules =
  str_opt "rules" ~docv:"TEXT" "Curated usage conventions (shown first by docs)."

let deep =
  tri ~on:"deep" ~off:"no-deep"
    ~doc_on:
      "Deep analysis: after the fetch, run the arch-index analysis of the local checkout \
       (never builds the project; its outcome is reported and never fails the \
       registration). Stored: later registrations of this repository run it too."
    ~doc_off:"No deep analysis (clears a stored deep flag)."

let register_cmd =
  let run c target ownership confidential campaign domain notes rules deep =
    let args =
      `Assoc
        (("target", `String target)
        :: List.filter_map Fun.id
             [
               some_str "ownership" ownership;
               some_str "confidential" confidential;
               some_bool "campaign" campaign;
               some_str "domain" domain;
               some_str "notes" notes;
               some_str "rules" rules;
               some_bool "deep" deep;
             ])
    in
    run_ctx c (fun ctx ->
        outcome (Ops.register_repo ctx (Repo_refresh.default_env ()) args))
  in
  Cmd.v
    (Cmd.info "register"
       ~doc:
         "Register (or update) one repository in sources.toml and fetch it at once \
          (metadata, README, documents; read-only GET calls or the local checkout, never \
          a clone). Exit 0 when the fetch completed, 2 when it was partial, 1 on errors.")
    Term.(
      const run $ ctx_term $ register_target $ ownership $ confidential $ campaign
      $ domain $ notes $ rules $ deep)

let kind_arg =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"KIND" ~doc:"Source kind, e.g. github-org.")

let name_arg =
  Arg.(
    value
    & pos 1 (some string) None
    & info [] ~docv:"NAME" ~doc:"User, organisation, group or directory.")

let register_source_cmd =
  let host = str_opt "host" ~docv:"HOST" "Host name." in
  let run c kind name host own probe subgroups confidential campaign_prefix =
    let args =
      `Assoc
        (("kind", `String kind)
        :: List.filter_map Fun.id
             [
               some_str "name" name;
               some_str "host" host;
               some_bool "own" own;
               some_bool "probe" probe;
               some_bool "include_subgroups" subgroups;
               some_str "confidential" confidential;
               some_bool "campaign_prefix" campaign_prefix;
             ])
    in
    run_ctx c (fun ctx ->
        outcome (Ops.register_source ctx (Repo_refresh.default_env ()) args))
  in
  Cmd.v
    (Cmd.info "register-source"
       ~doc:
         "Register (or update) a source in sources.toml, list it and fetch its \
          repositories incrementally. On an existing source, a flag left out keeps its \
          stored value. Exit 0 when complete, 2 when partial, 1 on errors.")
    Term.(
      const run $ ctx_term $ kind_arg $ name_arg $ host
      $ tri ~on:"own" ~off:"no-own" ~doc_on:"One of the owner's own organisations."
          ~doc_off:"Not one of the owner's organisations."
      $ tri ~on:"probe" ~off:"no-probe" ~doc_on:"May be absent (a 404 is a gap)."
          ~doc_off:"Must exist."
      $ tri ~on:"subgroups" ~off:"no-subgroups" ~doc_on:"GitLab groups: list subgroups."
          ~doc_off:"GitLab groups: do not list subgroups."
      $ str_opt "confidential" ~docv:"LEVEL" "generic | exclude-from-page | none."
      $ tri ~on:"campaign-prefix" ~off:"no-campaign-prefix"
          ~doc_on:"The name is a campaign owner prefix."
          ~doc_off:"The name is not a campaign owner prefix (clears the stored flag).")

let unregister_cmd =
  let target =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"URL|KEY|PATH" ~doc:"The registered target.")
  in
  let run c target =
    run_ctx c (fun ctx ->
        report (Ops.unregister_repo ctx (`Assoc [ ("target", `String target) ])))
  in
  Cmd.v
    (Cmd.info "unregister"
       ~doc:
         "Remove a repository's registration. A repository the registration created, \
          that no source lists and nobody curated (no curated field, no usage) is \
          deleted from the index with its documents (no tombstone); any other keeps its \
          entry and curated data. curated.json is rewritten.")
    Term.(const run $ ctx_term $ target)

let unregister_source_cmd =
  let run c kind name =
    let args = `Assoc (("kind", `String kind) :: Option.to_list (some_str "name" name)) in
    run_ctx c (fun ctx -> report (Ops.unregister_source ctx args))
  in
  Cmd.v
    (Cmd.info "unregister-source"
       ~doc:
         "Remove a source. Repositories listed only by it (not registered, not curated) \
          are deleted with their documents; the others keep their entry.")
    Term.(const run $ ctx_term $ kind_arg $ name_arg)

let sources_cmd =
  let list_cmd =
    Cmd.v
      (Cmd.info "list" ~doc:"Print the configured sources and registered repositories.")
      Term.(
        const (fun c -> run_ctx c (fun ctx -> report (Ops.list_sources ctx (`Assoc []))))
        $ ctx_term)
  in
  Cmd.group (Cmd.info "sources" ~doc:"Configured sources (sources.toml).") [ list_cmd ]
