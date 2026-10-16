let ( let* ) = Result.bind

let get (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "repo" ] args in
      let* repo = Args.req_string o "repo" in
      Repo_view.get ctx repo)

let filters_arg (ctx : Ctx.t) o =
  let* raw = Args.opt_object o "filters" in
  Filters.of_assoc ~domains:ctx.identity.domains (Option.value raw ~default:[])

let search (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o =
        Args.object_
          ~allowed:[ "query"; "limit"; "filters"; "detail"; "budget_chars"; "kind" ]
          args
      in
      let* query = Args.req_string o "query" in
      let* limit = Args.opt_int o "limit" in
      let* filters = filters_arg ctx o in
      let* detail = Opts.detail o in
      let* budget = Opts.budget o in
      let* kind = Opts.kind o in
      Ok (Search.search ~detail ?kind ?budget ctx ~query ~limit ~filters))

let resolve_library_id (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o =
        Args.object_
          ~allowed:[ "libraryName"; "query"; "filters"; "detail"; "budget_chars" ]
          args
      in
      let* library_name = Args.req_string o "libraryName" in
      let* query = Args.opt_string o "query" in
      let* filters = filters_arg ctx o in
      let* detail = Opts.detail o in
      let* budget = Opts.budget o in
      let* r = Library_id.resolve ctx ~library_name ~query ~filters ~detail in
      Ok (Opts.apply_budget budget ~list_key:"candidates" r))

let query_docs (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o =
        Args.object_
          ~allowed:
            [ "libraryId"; "query"; "limit"; "kind"; "filters"; "detail"; "budget_chars" ]
          args
      in
      let* library = Args.req_string o "libraryId" in
      let* query = Args.req_string o "query" in
      let* limit = Args.opt_int o "limit" in
      let* kind = Opts.kind o in
      let* filters = filters_arg ctx o in
      let* detail = Opts.detail o in
      let* budget = Opts.budget o in
      let* r = Docs_query.query ctx ~library ~query ~limit ~kind ~detail ~filters in
      Ok (Opts.apply_budget budget ~list_key:"excerpts" r))

let find_capability (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o =
        Args.object_
          ~allowed:[ "query"; "limit"; "filters"; "detail"; "budget_chars" ]
          args
      in
      let* query = Args.req_string o "query" in
      let* limit = Args.opt_int o "limit" in
      let* filters = filters_arg ctx o in
      let* detail = Opts.detail o in
      let* budget = Opts.budget o in
      Ok
        (Opts.apply_budget budget ~list_key:"results"
           (Capability.find ctx ~query ~limit ~filters ~detail)))

(* The optional [ownership] argument of copies / whats-new (FR-105: every query tool
   takes the ownership filter), validated like the [filters] member. *)
let ownership_arg o =
  let* v = Args.opt_string o "ownership" in
  match v with
  | None -> Ok None
  | Some s -> (
      match Vocab.canonical Vocab.ownerships s with
      | Some c -> Ok (Some c)
      | None -> Error (Vocab.invalid_value ~what:"ownership" s Vocab.ownerships))

let copies (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "name"; "ownership" ] args in
      let* name = Args.req_string o "name" in
      let* ownership = ownership_arg o in
      Ok (Copies_view.copies ctx ?ownership name))

let explain (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "repo" ] args in
      let* repo = Args.req_string o "repo" in
      Explain.explain ctx repo)

let whats_new (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "since"; "limit"; "ownership" ] args in
      let* since = Args.req_string o "since" in
      let* since = Whats_new.parse_since since in
      let* limit = Args.opt_int o "limit" in
      let* ownership = ownership_arg o in
      Ok (Whats_new.whats_new ctx ?ownership ~since ~limit ()))

let list (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.members args in
      let* limit = Args.opt_int o "limit" in
      let* offset = Args.opt_int o "offset" in
      let rest = List.filter (fun (k, _) -> k <> "limit" && k <> "offset") o in
      let* filters =
        Filters.of_assoc ~domains:ctx.identity.domains ~extra_keys:[ "limit"; "offset" ]
          rest
      in
      Ok (Listing.list ctx ~filters ~limit ~offset))

let annotate (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "repo"; "fields" ] args in
      let* repo = Args.req_string o "repo" in
      let* fields =
        match List.assoc_opt "fields" o with
        | None | Some `Null -> Error "missing required argument 'fields'"
        | Some (`Assoc m) -> Ok m
        | Some _ -> Error "argument 'fields' must be an object"
      in
      Curate.annotate ctx ~repo ~fields)

let record_usage (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "repo"; "consumer"; "note" ] args in
      let* repo = Args.req_string o "repo" in
      let* consumer = Args.req_string o "consumer" in
      let* note = Args.opt_string o "note" in
      Curate.record_usage ctx ~repo ~consumer ~note)

type refresh_outcome = {
  json : Yojson.Safe.t;
  gaps : string list;
  lines : string list;
  complete : bool;
}

let mcp_refresh_timeout = 45.

(* The first configured local root, "~" expanded; none configured: no local scan. *)
let default_dev_root (ctx : Ctx.t) =
  match ctx.identity.local_roots with
  | [] -> Ok None
  | r :: _ when String.length r > 0 && r.[0] = '~' ->
      Result.map (fun home -> Some (Config.expand_home ~home r)) (Ctx.home ())
  | r :: _ -> Ok (Some r)

let refresh_all ctx env ~dev_root ~full =
  let* dev_root =
    match dev_root with Some d -> Ok (Some d) | None -> default_dev_root ctx
  in
  let* s = Refresh.run ctx env ~dev_root ~full in
  Ok
    {
      json = Refresh.summary_json s;
      gaps = s.failures @ s.warnings;
      lines = Refresh.summary_lines s;
      complete = s.complete;
    }

let refresh (ctx : Ctx.t) env ?timeout ?dev_root ?progress ~allow_full args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "repo"; "full" ] args in
      (* sources.toml may have been edited by a registration since the server started. *)
      let* ctx = Ctx.reload ctx in
      let* repo = Args.opt_string o "repo" in
      let* full = Args.opt_bool o "full" in
      let full = Option.value full ~default:false in
      match repo with
      | Some ident ->
          let deadline = Option.map (fun t -> Unix.gettimeofday () +. t) timeout in
          let* r = Repo_refresh.refresh_one ctx env ?deadline ?progress ~full ident in
          Ok { json = r.json; gaps = r.gaps; lines = []; complete = r.complete }
      | None when allow_full -> refresh_all ctx env ~dev_root ~full
      | None -> Error "missing required argument 'repo'")

(* ------------------------------------------------------------ registration *)

let register_timeout = 120.

let register_repo (ctx : Ctx.t) env ?timeout ?progress args =
  let deadline = Option.map (fun t -> Unix.gettimeofday () +. t) timeout in
  Register.register_repo ctx env ?progress ?deadline args

let register_source (ctx : Ctx.t) env ?timeout ?progress args =
  let deadline = Option.map (fun t -> Unix.gettimeofday () +. t) timeout in
  Source_reg.register_source ctx env ?progress ?deadline args

let unregister_repo = Unregister.unregister_repo
let unregister_source = Unregister.unregister_source
let list_sources = Register.list_sources

(* ------------------------------------------------------------ analysis *)

let analyze_repo (ctx : Ctx.t) (env : Repo_refresh.env) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "repo"; "allow_build" ] args in
      let* repo = Args.req_string o "repo" in
      let* allow_build = Args.opt_bool o "allow_build" in
      let allow_build = Option.value allow_build ~default:false in
      Refresh.with_lock ctx.export_dir (fun () ->
          Analysis.analyze ctx ~run:env.analyze ~allow_build repo))
