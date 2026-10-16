let ( let* ) = Result.bind

type runner = Proc.runner

let timeout_env = "EPURE_ARCH_INDEX_TIMEOUT_S=300"
let arch_timeout = 420.
let build_timeout = 600.
let empty_call_graph = "empty call graph (LSP prepareCallHierarchy)"

let on_path name =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      List.find_map
        (fun dir ->
          if dir = "" then None
          else
            let p = Filename.concat dir name in
            match Unix.access p [ Unix.X_OK ] with
            | () when not (Sys.is_directory p) -> Some p
            | () -> None
            | exception Unix.Unix_error _ -> None
            | exception Sys_error _ -> None)
        (String.split_on_char ':' path)

let lsp_table =
  [
    ("ocaml", "ocamllsp");
    ("typescript", "typescript-language-server");
    ("rust", "rust-analyzer");
    ("go", "gopls");
    ("python", "pylsp");
  ]

let lsp_binary language = List.assoc_opt language lsp_table

let arch_language ctx =
  List.find_map
    (fun (e : Api_item.extractor) ->
      if List.mem_assoc e.language lsp_table then Some e.language else None)
    (Extract.detected ctx)

(* ------------------------------------------------------------ checkout *)

let read_small dir path =
  let p = Filename.concat dir path in
  match Unix.lstat p with
  | { Unix.st_kind = Unix.S_REG; st_size; _ } when st_size <= 1_048_576 -> (
      try
        let ic = open_in_bin p in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> Some (really_input_string ic st_size))
      with Sys_error _ | End_of_file -> None)
  | _ -> None
  | exception Unix.Unix_error _ -> None

let head ~run dir =
  match run ~timeout:Proc.default_timeout [| "git"; "-C"; dir; "rev-parse"; "HEAD" |] with
  | Ok out when String.trim out <> "" -> Some (String.trim out)
  | _ -> None

let context ~run ~language dir =
  let paths =
    match run ~timeout:Proc.default_timeout (Commands.git_ls_files_z dir) with
    | Ok out -> List.filter (fun p -> p <> "") (String.split_on_char '\000' out)
    | Error _ -> []
  in
  let manifests =
    List.filter_map
      (fun p ->
        if List.mem p paths then Option.map (fun t -> (p, t)) (read_small dir p) else None)
      Api_item.manifest_files
  in
  { Api_item.paths; manifests; language }

(* Whether [_build/default] holds a .cmt file (bounded walk). *)
let built dir =
  let root = Filename.concat dir "_build/default" in
  let budget = ref 50_000 in
  let rec walk d depth =
    depth <= 10 && !budget > 0
    &&
    match Sys.readdir d with
    | entries ->
        Array.exists
          (fun e ->
            decr budget;
            let p = Filename.concat d e in
            Filename.check_suffix e ".cmt"
            ||
            match Unix.lstat p with
            | { Unix.st_kind = Unix.S_DIR; _ } -> walk p (depth + 1)
            | _ -> false
            | exception Unix.Unix_error _ -> false)
          entries
    | exception Sys_error _ -> false
  in
  walk root 0

(* ------------------------------------------------------------ one row *)

type outcome = {
  status : string;
  cause : string option;
  head_sha : string option;
  tool_version : string option;
  language : string option;
  items : int;
  calls : int;
}

let outcome ?head_sha ?tool_version ?language ?(items = 0) ?(calls = 0) status cause =
  { status; cause; head_sha; tool_version; language; items; calls }

let record (ctx : Ctx.t) ~repo_id o =
  let now = Ctx.rfc3339 (ctx.now ()) in
  Db.run ctx.db
    {|INSERT INTO analysis(repo_id, status, cause, head_sha, at, tool_version)
VALUES (?, ?, ?, ?, ?, ?)
ON CONFLICT(repo_id) DO UPDATE SET status = excluded.status, cause = excluded.cause,
  head_sha = excluded.head_sha, at = excluded.at, tool_version = excluded.tool_version|}
    [
      Db.int repo_id;
      Db.text o.status;
      Db.rtext_opt o.cause;
      (match o.head_sha with Some h -> Db.text h | None -> Sqlite3.Data.NULL);
      Db.text now;
      Db.rtext_opt o.tool_version;
    ];
  now

let remove_db path =
  List.iter
    (fun suffix -> try Sys.remove (path ^ suffix) with Sys_error _ -> ())
    [ ""; "-wal"; "-shm"; "-journal" ]

let temp_db (ctx : Ctx.t) key =
  let dir = Filename.concat ctx.data_dir "analysis" in
  (try Unix.mkdir dir 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let safe =
    String.map
      (fun c ->
        match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> c | _ -> '_')
      key
  in
  Filename.concat dir
    (Printf.sprintf "%s-%d-%.0f.db" safe (Unix.getpid ()) (Unix.gettimeofday () *. 1000.))

type row = {
  id : int;
  key : string;
  host : string;
  campaign : bool;
  local_dir : string option;
  language : string option;
}

let load_row (ctx : Ctx.t) id =
  Db.query_one ctx.db
    "SELECT id, key, host, campaign, local_paths, language FROM repos WHERE id = ?"
    [ Db.int id ]
    (fun r ->
      let s i = Db.to_string_opt r.(i) in
      {
        id;
        key = Option.value (s 1) ~default:"";
        host = Option.value (s 2) ~default:"";
        campaign = Db.to_bool r.(3);
        local_dir =
          Doc_refresh.local_dir
            (Option.value (Option.bind (s 4) Jsonx.parse) ~default:`Null);
        language = s 5;
      })

let run_arch ~run ~bin ~wrapper ~language ~dir ~enrich out =
  let argv =
    if wrapper then [| "env"; timeout_env; bin; dir; out; language |]
    else
      Array.of_list
        ([
           "env";
           timeout_env;
           bin;
           "--language";
           language;
           "--project";
           dir;
           "--output";
           out;
         ]
        @ if enrich then [] else [ "--no-enrich" ])
  in
  run ~timeout:arch_timeout argv

(* Each precondition is a step: [Error o] stops the analysis with the outcome [o]. *)
let gate ~run (r : row) =
  if Eligibility.is_confidential ~campaign:r.campaign ~host:r.host then
    Error (outcome "skipped" (Some "campaign repository: not analysed"))
  else
    match (r.local_dir, run) with
    | None, _ ->
        Error
          (outcome "skipped"
             (Some "no local checkout (arch-index needs one; never cloned)"))
    | Some _, None ->
        Error (outcome "skipped" (Some "analysis disabled in this environment"))
    | Some dir, Some run -> Ok (dir, run)

let find_arch ?head_sha () =
  match on_path "arch_index_cli" with
  | Some b -> Ok (b, false)
  | None -> (
      match on_path "arch-index" with
      | Some b -> Ok (b, true)
      | None ->
          Error
            (outcome ?head_sha "skipped"
               (Some "arch-index not found on PATH (arch_index_cli or arch-index)")))

let pick_language ~run ?head_sha (r : row) dir =
  match arch_language (context ~run ~language:r.language dir) with
  | Some language -> Ok language
  | None ->
      Error
        (outcome ?head_sha "skipped"
           (Some "no language arch-index supports (ocaml, typescript, rust, go, python)"))

let check_lsp ?head_sha language =
  let lsp = Option.value (lsp_binary language) ~default:"" in
  if on_path lsp = None then
    Error
      (outcome ?head_sha ~language "skipped"
         (Some (Printf.sprintf "LSP binary %s not found on PATH" lsp)))
  else Ok ()

(* OCaml: the CMT enricher needs a build; the project is built only when allowed. *)
let build_cause ~run ~allow_build ~language dir =
  if language <> "ocaml" || built dir then None
  else if not allow_build then
    Some "not built: CMT enrichment skipped (allow_build=false)"
  else
    match run ~timeout:build_timeout [| "dune"; "build"; "--root"; dir |] with
    | Ok _ when built dir -> None
    | Ok _ -> Some "built, but no .cmt file: CMT enrichment skipped"
    | Error f ->
        Some
          (Printf.sprintf "build failed (%s): CMT enrichment skipped" (Proc.describe f))

let imported_outcome ?head_sha ~language ~build_cause (i : Comment_db.imported) =
  if i.total = 0 then
    (* Nothing indexed: the previous arch-index items stay. *)
    let detail = match build_cause with None -> "" | Some b -> "; " ^ b in
    outcome ?head_sha ~language ~tool_version:i.tool "failed"
      (Some ("arch-index indexed no function (LSP produced nothing" ^ detail ^ ")"))
  else
    let causes =
      (if i.edges = 0 then [ empty_call_graph ] else []) @ Option.to_list build_cause
    in
    outcome ?head_sha ~language ~tool_version:i.tool ~items:i.items ~calls:i.calls
      (if causes = [] then "ok" else "partial")
      (if causes = [] then None else Some (String.concat "; " causes))

let run_and_import (ctx : Ctx.t) (r : row) ~run ~bin ~wrapper ~language ~dir ?head_sha
    ~build_cause () =
  let out = temp_db ctx r.key in
  Fun.protect
    ~finally:(fun () -> remove_db out)
    (fun () ->
      let enrich = build_cause = None in
      match run_arch ~run ~bin ~wrapper ~language ~dir ~enrich out with
      | Error f ->
          outcome ?head_sha ~language "failed"
            (Some (Printf.sprintf "arch-index failed (%s)" (Proc.describe f)))
      | Ok _ when not (Sys.file_exists out) ->
          outcome ?head_sha ~language "failed" (Some "arch-index wrote no database")
      | Ok _ -> (
          match
            Errors.guard (fun () ->
                Comment_db.import ctx ~repo_id:r.id ~head:head_sha ~language out)
          with
          | Error e -> outcome ?head_sha ~language "failed" (Some e)
          | Ok i -> imported_outcome ?head_sha ~language ~build_cause i))

(* The campaign decision, applied now ({!Campaign.apply_now}, in its own transaction): a
   checkout remote under a campaign prefix (or a forced checkout) makes the row a campaign
   row before anything is analysed (AM-17). *)
let runner_call (run : runner) (c : Commands.t) = run ~timeout:Proc.default_timeout c.argv

let analyze_row (ctx : Ctx.t) ~run ~allow_build ?local (r : row) =
  let steps =
    let* () =
      match run with
      | Some run when not r.campaign -> (
          match Campaign.apply_now ctx ~call:(runner_call run) ?local r.key with
          | Error e -> Error (outcome "skipped" (Some e))
          | Ok true ->
              Error (outcome "skipped" (Some "campaign repository: not analysed"))
          | Ok false -> Ok ())
      | _ -> Ok ()
    in
    let* dir, run = gate ~run r in
    let head_sha = head ~run dir in
    let* bin, wrapper = find_arch ?head_sha () in
    let* language = pick_language ~run ?head_sha r dir in
    let* () = check_lsp ?head_sha language in
    let build_cause = build_cause ~run ~allow_build ~language dir in
    Ok (run_and_import ctx r ~run ~bin ~wrapper ~language ~dir ?head_sha ~build_cause ())
  in
  match steps with Ok o | Error o -> o

let to_json ~key ~at o =
  let s = function Some v -> `String v | None -> `Null in
  `Assoc
    [
      ("key", `String key);
      ("status", `String o.status);
      ("cause", s o.cause);
      ("head_sha", s o.head_sha);
      ("at", `String at);
      ("tool_version", s o.tool_version);
      ("language", s o.language);
      ("items", `Int o.items);
      ("calls", `Int o.calls);
    ]

let analyze_id (ctx : Ctx.t) ~run ~allow_build ?local id =
  match load_row ctx id with
  | None -> Error "unknown repo"
  | Some r ->
      let o =
        match
          Errors.guard (fun () -> Ok (analyze_row ctx ~run ~allow_build ?local r))
        with
        | Ok o -> o
        | Error e -> outcome "failed" (Some e)
      in
      let at = record ctx ~repo_id:r.id o in
      Ok (r.key, at, o)

let analyze (ctx : Ctx.t) ~run ~allow_build ident =
  Errors.guard (fun () ->
      let* { Resolve.id; _ } = Resolve.resolve ctx.db ident in
      let* key, at, o = analyze_id ctx ~run ~allow_build id in
      Ok (to_json ~key ~at o))

(* ------------------------------------------------------------ refresh hook *)

type summary = {
  considered : int;
  analysed : int;
  ok : int;
  partial : int;
  skipped : int;
  failed : int;
}

let summary_json s =
  `Assoc
    [
      ("considered", `Int s.considered);
      ("analysed", `Int s.analysed);
      ("ok", `Int s.ok);
      ("partial", `Int s.partial);
      ("skipped", `Int s.skipped);
      ("failed", `Int s.failed);
    ]

let zero = { considered = 0; analysed = 0; ok = 0; partial = 0; skipped = 0; failed = 0 }

let refresh_hook (ctx : Ctx.t) ~run =
  let candidates =
    match
      Errors.guard (fun () ->
          Ok
            (Db.query ctx.db
               ({|SELECT r.id, r.local_paths, a.head_sha FROM repos r
LEFT JOIN analysis a ON a.repo_id = r.id
WHERE r.tier = '1' AND r.ownership = 'own' AND r.missing_since IS NULL AND |}
               ^ Eligibility.not_confidential ~alias:"r" ()
               ^ " ORDER BY r.key")
               []
               (fun r ->
                 ( Option.value (Db.to_int_opt r.(0)) ~default:0,
                   Doc_refresh.local_dir
                     (Option.value
                        (Option.bind (Db.to_string_opt r.(1)) Jsonx.parse)
                        ~default:`Null),
                   Db.to_string_opt r.(2) ))))
    with
    | Ok l -> l
    | Error _ -> []
  in
  (* The checkouts are scanned once for the whole hook, and only when a row is analysed. *)
  let local =
    lazy
      (Local_keys.configured ctx ~call:(fun (c : Commands.t) ->
           run ~timeout:Proc.default_timeout c.argv))
  in
  List.fold_left
    (fun s (id, dir, stored) ->
      match dir with
      | None -> s
      | Some dir -> (
          let s = { s with considered = s.considered + 1 } in
          match head ~run dir with
          | Some h when Some h <> stored -> (
              let s = { s with analysed = s.analysed + 1 } in
              match analyze_id ctx ~run:(Some run) ~allow_build:false ~local id with
              | Ok (_, _, o) -> (
                  match o.status with
                  | "ok" -> { s with ok = s.ok + 1 }
                  | "partial" -> { s with partial = s.partial + 1 }
                  | "skipped" -> { s with skipped = s.skipped + 1 }
                  | _ -> { s with failed = s.failed + 1 })
              | Error _ -> { s with failed = s.failed + 1 }
              | exception _ -> { s with failed = s.failed + 1 })
          | _ -> s))
    zero candidates
