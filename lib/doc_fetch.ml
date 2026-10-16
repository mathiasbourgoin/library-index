type target = {
  repo_id : int;
  key : string;
  host : string;
  full_name : string;
  gl_id : int option;
  default_branch : string option;
  local_dir : string option;
  pushed_at : string option;
  docs_pushed_at : string option;
  stored_blobs : (string * string) list;
  stored_scan_blobs : (string * string) list;
  language : string option;
}

type mode = From_local | From_api | Not_due | No_source

let mode_name = function
  | From_local -> "local"
  | From_api -> "api"
  | Not_due -> "not-due"
  | No_source -> "no-source"

type scan_file = { path : string; text : string; blob_sha : string option }

type result = {
  target : target;
  mode : mode;
  docs : Doc_store.doc list;
  keep : string list option;
  complete : bool;
  capped : bool;
  failures : (Commands.purpose * Proc.failure) list;
  aliases : string list option;
  scan : (scan_file list * string list) option;
}

let max_repo_bytes = 4 * 1024 * 1024
let max_api_fetches = 60

let api_due ~full t =
  full
  ||
  match (t.docs_pushed_at, t.pushed_at) with
  | None, _ -> true
  | Some d, Some p -> d <> p
  | Some _, None -> false

let empty t mode =
  {
    target = t;
    mode;
    docs = [];
    keep = None;
    complete = true;
    capped = false;
    failures = [];
    aliases = None;
    scan = None;
  }

(* Keep documents in selection order while their total stays within the byte cap. *)
let within_bytes docs =
  let rec go total acc = function
    | [] -> (List.rev acc, false)
    | (d : Doc_store.doc) :: rest ->
        let total = total + min (String.length d.text) Doc_store.file_cap in
        if total > max_repo_bytes then (List.rev acc, true) else go total (d :: acc) rest
  in
  go 0 [] docs

(* ------------------------------------------------------------ local checkout *)

(* Regular files only (no symbolic link is followed), at most a little over the cap. *)
let read_regular dir path =
  let p = Filename.concat dir path in
  match Unix.lstat p with
  | { Unix.st_kind = Unix.S_REG; st_size; _ } -> (
      try
        let ic = open_in_bin p in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            Some (really_input_string ic (min st_size (Doc_store.file_cap + 64))))
      with Sys_error _ | End_of_file -> None)
  | _ -> None
  | exception Unix.Unix_error _ -> None

let split_nul s = List.filter (fun p -> p <> "") (String.split_on_char '\000' s)
let root_manifests paths = List.filter (fun p -> List.mem p paths) Api_item.manifest_files

(* Manifests feed language detection, the TypeScript entry points and the aliases. *)
let selection t paths manifests =
  ( Doc_select.select ?language:t.language ~manifests paths,
    Manifest.aliases ~paths ~manifests,
    Doc_select.api_paths ?language:t.language ~manifests paths )

(* Scanned files in path order while their total stays within the byte cap. *)
let scan_within_bytes files =
  let rec go total acc = function
    | [] -> (List.rev acc, false)
    | (f : scan_file) :: rest ->
        let total = total + min (String.length f.text) Doc_store.file_cap in
        if total > max_repo_bytes then (List.rev acc, true) else go total (f :: acc) rest
  in
  go 0 [] files

let from_local ~call t dir =
  match call { Commands.purpose = Local_git; argv = Commands.git_ls_files_z dir } with
  | Error `Not_found -> empty t From_local
  | Error f ->
      { (empty t From_local) with complete = false; failures = [ (Local_git, f) ] }
  | Ok out ->
      let paths = split_nul out in
      let manifests =
        List.filter_map
          (fun p -> Option.map (fun text -> (p, text)) (read_regular dir p))
          (root_manifests paths)
      in
      let selected, aliases, api_paths = selection t paths manifests in
      let docs =
        List.filter_map
          (fun (path, kind) ->
            Option.map
              (fun text ->
                { Doc_store.path; kind; source = Local; text; blob_sha = None })
              (read_regular dir path))
          selected
      in
      let docs, capped = within_bytes docs in
      let scanned =
        List.filter_map
          (fun path ->
            let text =
              match List.find_opt (fun (d : Doc_store.doc) -> d.path = path) docs with
              | Some d -> Some d.text
              | None -> read_regular dir path
            in
            Option.map (fun text -> { path; text; blob_sha = None }) text)
          api_paths
      in
      let scanned, scan_capped = scan_within_bytes scanned in
      let capped = capped || scan_capped in
      {
        (empty t From_local) with
        docs;
        keep = Some (List.map (fun (d : Doc_store.doc) -> d.path) docs);
        capped;
        complete = not capped;
        aliases = Some aliases;
        scan = Some (scanned, List.map (fun (f : scan_file) -> f.path) scanned);
      }

(* ------------------------------------------------------------ forge API *)

let blob_entries ~id_field items =
  List.filter_map
    (fun j ->
      match (Jsonx.str "type" j, Jsonx.str "path" j, Jsonx.str id_field j) with
      | Some "blob", Some p, Some sha -> Some (p, sha)
      | _ -> None)
    items

let tree ~call t =
  let run argv parse =
    match call { Commands.purpose = Doc_tree; argv } with
    | Error f -> Error f
    | Ok out -> ( match parse out with Some l -> Ok l | None -> Error `Other)
  in
  if t.host = "github" then
    run (Commands.gh_doc_tree ~full_name:t.full_name ~branch:t.default_branch) (fun out ->
        match Jsonx.parse out with
        | Some j -> (
            match Jsonx.get "tree" j with
            | `List l -> Some (blob_entries ~id_field:"sha" l)
            | _ -> None)
        | None -> None)
  else
    run (Commands.gl_doc_tree ~id:t.gl_id ~full_name:t.full_name) (fun out ->
        Option.map (blob_entries ~id_field:"id") (Sources.parse_paginated out))

let file_argv t path =
  if t.host = "github" then Commands.gh_doc_file ~full_name:t.full_name path
  else
    Commands.gl_doc_file ~id:t.gl_id ~full_name:t.full_name ~branch:t.default_branch path

let rec split_at n = function
  | x :: rest when n > 0 ->
      let a, b = split_at (n - 1) rest in
      (x :: a, b)
  | l -> ([], l)

let fetch_all ~call ~workers t jobs =
  let results = Hashtbl.create 16 in
  Pool.run ~workers jobs
    (fun (path, _, _) ->
      (path, call { Commands.purpose = Doc_file; argv = file_argv t path }))
    (function Ok (path, r) -> Hashtbl.replace results path r | Error _ -> ());
  List.map
    (fun ((path, _, _) as job) ->
      (job, Option.value (Hashtbl.find_opt results path) ~default:(Error `Other)))
    jobs

let file_failures results =
  List.filter_map
    (fun (_, r) ->
      match r with
      | Error `Not_found | Ok _ -> None
      | Error f -> Some (Commands.Doc_file, f))
    results

(* The root manifests (for aliases and selection) and their failures. *)
let fetch_manifests ~call ~workers t paths =
  let fetched =
    fetch_all ~call ~workers t
      (List.map (fun p -> (p, Doc_select.Doc, None)) (root_manifests paths))
  in
  let manifests =
    List.filter_map
      (fun ((p, _, _), r) -> match r with Ok text -> Some (p, text) | Error _ -> None)
      fetched
  in
  (manifests, file_failures fetched)

(* The selected documents whose blob changed (all with [full]), then the scanned API
   files that no document fetch covers. *)
let due_jobs t ~full ~entries ~selected ~api_paths =
  let unchanged stored path blob =
    (not full) && blob <> None && List.assoc_opt path stored = blob
  in
  let due =
    List.filter_map
      (fun (path, kind) ->
        let blob = List.assoc_opt path entries in
        if unchanged t.stored_blobs path blob then None else Some (path, kind, blob))
      selected
  in
  let scan_due =
    List.filter_map
      (fun path ->
        let blob = List.assoc_opt path entries in
        if
          List.exists (fun (p, _, _) -> p = path) due
          || unchanged t.stored_scan_blobs path blob
        then None
        else Some (path, Doc_select.Api, blob))
      api_paths
  in
  due @ scan_due

let scanned_of ~api_paths fetched =
  List.filter_map
    (fun ((path, _, blob_sha), r) ->
      match r with
      | Ok text when List.mem path api_paths -> Some { path; text; blob_sha }
      | _ -> None)
    fetched

let docs_of ~selected fetched =
  List.filter_map
    (fun ((path, kind, blob_sha), r) ->
      match r with
      | Ok text when List.mem_assoc path selected ->
          Some { Doc_store.path; kind; source = Api; text; blob_sha }
      | _ -> None)
    fetched

let from_entries ~call ~full ~workers t entries =
  let paths = List.map fst entries in
  let manifests, manifest_failures = fetch_manifests ~call ~workers t paths in
  let selected, aliases, api_paths = selection t paths manifests in
  let jobs, over =
    split_at max_api_fetches (due_jobs t ~full ~entries ~selected ~api_paths)
  in
  let fetched = fetch_all ~call ~workers t jobs in
  let scanned, scan_capped = scan_within_bytes (scanned_of ~api_paths fetched) in
  let docs, byte_capped = within_bytes (docs_of ~selected fetched) in
  let failures = manifest_failures @ file_failures fetched in
  let capped = over <> [] || byte_capped || scan_capped in
  {
    (empty t From_api) with
    docs;
    keep = Some (List.map fst selected);
    scan = Some (scanned, api_paths);
    capped;
    failures;
    complete = failures = [] && not capped;
    (* A manifest that could not be read must not drop its aliases. *)
    aliases = (if manifest_failures = [] then Some aliases else None);
  }

let from_api ~call ~full ~workers t =
  match tree ~call t with
  | Error `Not_found -> empty t From_api (* empty or unreadable tree: no documents *)
  | Error f -> { (empty t From_api) with complete = false; failures = [ (Doc_tree, f) ] }
  | Ok entries -> from_entries ~call ~full ~workers t entries

let run ~call ~full ~workers t =
  try
    match t.local_dir with
    | Some dir -> from_local ~call t dir
    | None when t.host <> "github" && t.host <> "gitlab" -> empty t No_source
    | None when not (api_due ~full t) -> empty t Not_due
    | None -> from_api ~call ~full ~workers t
  with
  | Unix.Unix_error _ | Sys_error _ | Failure _ | Not_found | Invalid_argument _
  | Yojson.Json_error _
  ->
    (* An I/O or decoding failure of one repository is a recorded gap; anything else
       (a programming error, Out_of_memory, Stack_overflow) propagates. *)
    { (empty t From_api) with complete = false; failures = [ (Doc_tree, `Other) ] }
