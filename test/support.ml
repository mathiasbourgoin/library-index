(* Shared helpers for the alcotest suites.

   [run] adds an argv shim so the spec's Runnable Checks work as written:
   `test.exe test <suite> <case>` (alcotest reads the third word as a numeric range) is
   rewritten to `test.exe test ^<case>$`, where each case is an alcotest group. An
   unknown case name exits 1 instead of silently running nothing. *)

open Library_index

let parity_now = Option.get (Status.parse_time "2026-10-03T19:58:30Z")
let fixed_clock () = parity_now

let run suite groups =
  let argv = Sys.argv in
  let names = List.map fst groups in
  let argv =
    if
      Array.length argv = 4
      && argv.(1) = "test"
      && argv.(2) = suite
      && int_of_string_opt (String.sub argv.(3) 0 1) = None
    then (
      if not (List.mem argv.(3) names) then (
        Printf.eprintf "unknown test case '%s' in suite '%s' (known: %s)\n" argv.(3) suite
          (String.concat ", " names);
        exit 1);
      [| argv.(0); "test"; "^" ^ argv.(3) ^ "$" |])
    else argv
  in
  Alcotest.run ~argv suite groups

(* ------------------------------------------------------------ files *)

let rec rm_rf path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun f -> rm_rf (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error _ -> ()

let temp_dir () =
  let d = Filename.temp_dir "library-index-test" "" in
  at_exit (fun () -> rm_rf d);
  d

let exe_dir = Filename.dirname Sys.executable_name

let first_existing what candidates =
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> failwith ("cannot locate " ^ what)

let fixture path =
  first_existing ("test/fixtures/" ^ path)
    [
      Filename.concat exe_dir ("fixtures/" ^ path);
      "test/fixtures/" ^ path;
      "fixtures/" ^ path;
    ]

(** Synthetic curated.json / whitehat_tools.json (fictional names only). *)
let fixtures_dir = fixture "sample"

(** The synthetic [sources.toml] every suite runs under, and its identity. *)
let config =
  match Config.load (fixture "sources.test.toml") with
  | Ok c -> c
  | Error e -> failwith ("test configuration: " ^ e)

let id = Identity.of_config config

let copy_file src dst =
  let ic = open_in_bin src in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let oc = open_out_bin dst in
  output_string oc s;
  close_out oc

let read_file p =
  let ic = open_in_bin p in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* ------------------------------------------------------------ DBs *)

let ok_or_fail = function Ok v -> v | Error e -> Alcotest.failf "unexpected error: %s" e

let open_ctx ?(export_dir = "") ?(config = config) db_path =
  let db = ok_or_fail (Db.open_existing db_path) in
  let data_dir = Filename.dirname db_path in
  let export_dir = if export_dir = "" then data_dir else export_dir in
  {
    Ctx.db;
    db_path;
    data_dir;
    export_dir;
    config;
    identity = Identity.of_config config;
    now = fixed_clock;
  }

(** A fresh DB initialised from the synthetic sample fixtures, in its own temp dir (which
    is also the export dir). *)
let fresh_ctx ?(from = fixtures_dir) () =
  let dir = temp_dir () in
  let db_path = Filename.concat dir "t.db" in
  ignore (ok_or_fail (Init.init ~id ~db_path ~from_dir:from ~force:false ()));
  open_ctx db_path

(** An empty initialised DB (no curated rows) for hand-built fixtures. *)
let empty_ctx () =
  let dir = temp_dir () in
  let src = Filename.concat dir "src" in
  Unix.mkdir src 0o755;
  let write name s =
    let oc = open_out_bin (Filename.concat src name) in
    output_string oc s;
    close_out oc
  in
  write "curated.json" "{}";
  write "whitehat_tools.json" {|{"tools": []}|};
  let db_path = Filename.concat dir "t.db" in
  ignore (ok_or_fail (Init.init ~id ~db_path ~from_dir:src ~force:false ()));
  open_ctx db_path

let member k = function
  | `Assoc m -> Option.value ~default:`Null (List.assoc_opt k m)
  | _ -> `Null

let to_str = function `String s -> s | j -> Yojson.Safe.to_string j

let fts_integrity (ctx : Ctx.t) =
  Db.exec ctx.db "INSERT INTO repos_fts(repos_fts) VALUES('integrity-check')"

(** Render settings of the synthetic configuration for [ctx]. *)
let render_settings (ctx : Ctx.t) ~out ~notes_dir ~home =
  Render_data.settings ctx.config ~identity:ctx.identity ~db_path:ctx.db_path
    ~index_md:out ~notes_dir ~home

(** {!Render.run} with the context's configured texts and a notes directory. *)
let render (ctx : Ctx.t) ~out ~notes_dir ~now ~home =
  let notes_dir = Some notes_dir in
  Render.run ctx.db
    ~settings:(render_settings ctx ~out ~notes_dir ~home)
    ~out ~notes_dir ~now ~home

(* ------------------------------------------------------------ source scans *)

(** The [.ml]/[.mli] files of the code tree's [lib/] and [bin/]. *)
let source_files () =
  let ok d =
    Sys.file_exists (Filename.concat d "lib/library_id.ml")
    && Sys.file_exists (Filename.concat d "bin/main.ml")
  in
  match List.find_opt ok [ ".."; "."; Filename.concat exe_dir "../../.." ] with
  | None -> failwith "cannot locate lib/ and bin/"
  | Some root ->
      List.concat_map
        (fun d ->
          let d = Filename.concat root d in
          Sys.readdir d |> Array.to_list |> List.sort compare
          |> List.filter (fun f ->
              Filename.check_suffix f ".ml" || Filename.check_suffix f ".mli")
          |> List.map (Filename.concat d))
        [ "lib"; "bin" ]

(** Model-API hosts, endpoints and SDK names (spec FR-110: no LLM call, ever). *)
let model_api_markers =
  [
    "openai";
    "anthropic";
    "generativelanguage";
    "aiplatform.googleapis";
    "ollama";
    "mistral.ai";
    "api.cohere";
    "huggingface";
    "bedrock-runtime";
    "langchain";
    "litellm";
    "api.groq.com";
    "api.together";
    "api.deepseek";
    "replicate.com";
    "llama.cpp";
    "/chat/completions";
    "/v1/messages";
    "gpt-4";
    "claude-";
  ]

(** [file:line marker] for every model-API marker found (case-insensitive) in [files]. *)
let model_api_hits files =
  List.concat_map
    (fun f ->
      List.concat
        (List.mapi
           (fun i line ->
             let l = String.lowercase_ascii line in
             List.filter_map
               (fun m ->
                 if Re.execp (Re.compile (Re.str m)) l then
                   Some (Printf.sprintf "%s:%d %s" (Filename.basename f) (i + 1) m)
                 else None)
               model_api_markers)
           (String.split_on_char '\n' (read_file f))))
    files
