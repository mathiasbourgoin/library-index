(* CHECK-31 (spec FR-100, AC-31): no sensitive name in the code tree.

   Reads the data directory ($LIBRARY_INDEX_DATA, else $XDG_DATA_HOME/library-index or
   ~/.local/share/library-index). A set LIBRARY_INDEX_DATA that does not hold
   denylist.txt or sources.toml fails; without it, a missing default data directory (a
   public clone) prints "skipped: no data dir" and passes. Otherwise the names are the
   lines of denylist.txt (blank lines and # comments ignored) plus every user,
   organisation, group, owner and repository name of sources.toml ({!Config.names});
   every regular file of the source tree (located from dune's DUNE_SOURCEROOT whatever
   --build-dir is; excluding _build, _opam, .git, any dune build directory and the root
   LICENSE, whose copyright notice names its holder on purpose) is scanned, ignoring
   case, at word boundaries (no ASCII letter or digit on either side). Each hit is
   reported as file:line and the name; the line itself is never printed. *)

open Library_index

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("guard: " ^ s);
      exit 1)
    fmt

let read_lines path =
  In_channel.with_open_bin path In_channel.input_all |> String.split_on_char '\n'

let denylist dir =
  let path = Filename.concat dir "denylist.txt" in
  if not (Sys.file_exists path) then []
  else
    List.filter_map
      (fun l ->
        let l = String.trim l in
        if l = "" || l.[0] = '#' then None else Some l)
      (read_lines path)

let names dir =
  let config =
    if Sys.file_exists (Filename.concat dir Config.file_name) then
      match Config.load_dir dir with Ok c -> Config.names c | Error e -> fail "%s" e
    else []
  in
  List.sort_uniq compare (List.map String.lowercase_ascii (denylist dir @ config))

let realpath p = try Unix.realpath p with Unix.Unix_error _ -> p
let exists_in d f = Sys.file_exists (Filename.concat d f)

(* The code tree: a directory holding dune-project, lib/ and test/test_guard.ml. *)
let is_tool_root d =
  exists_in d "dune-project" && exists_in d "lib" && exists_in d "test/test_guard.ml"

(* A dune build copy of the tree (any --build-dir) holds the build's .merlin-conf. *)
let is_build_copy d = exists_in d "test/.merlin-conf"

let rec up_to_root ~stop d =
  if is_tool_root d && not (is_build_copy d) then Some d
  else
    let p = Filename.dirname d in
    if p = d || d = stop then None else up_to_root ~stop p

let strip_prefix ~prefix s =
  let n = String.length prefix in
  if s = prefix then Some ""
  else if String.length s > n && String.sub s 0 n = prefix && s.[n] = '/' then
    Some (String.sub s (n + 1) (String.length s - n - 1))
  else None

(* Under `dune runtest`, the action runs in <build>/default/<rel> ($INSIDE_DUNE is
   <build>/default) and $DUNE_SOURCEROOT is the source workspace: the test's source
   directory is $DUNE_SOURCEROOT/<rel>, whatever --build-dir is. Under `dune exec` (the
   working directory is the caller's) or by hand, the code tree is the nearest ancestor of
   the working directory that is not a build copy. *)
(* The code tree, and the directory a relative $LIBRARY_INDEX_DATA is resolved against:
   the code tree under `dune runtest` (the gate runs it from there as ../data), the
   caller's working directory otherwise. *)
let tool_root () =
  let cwd = realpath (Sys.getcwd ()) in
  let found, base =
    match (Sys.getenv_opt "DUNE_SOURCEROOT", Sys.getenv_opt "INSIDE_DUNE") with
    | Some src, Some build when src <> "" && build <> "" -> (
        let src = realpath src in
        match strip_prefix ~prefix:(realpath build) cwd with
        | Some rel -> (up_to_root ~stop:src (Filename.concat src rel), `Root)
        | None -> (up_to_root ~stop:"/" cwd, `Cwd))
    | _ -> (up_to_root ~stop:"/" cwd, `Cwd)
  in
  match found with
  | Some root -> (root, match base with `Root -> root | `Cwd -> cwd)
  | None -> fail "cannot locate the code tree (dune-project, lib/, test/test_guard.ml)"

(* A dune build directory (its root or a context or a copied directory in it), whatever
   its name: never scanned. *)
let is_dune_build_dir d =
  exists_in d ".digest-db" || exists_in d "default/.dune" || exists_in d ".merlin-conf"

(* The build directory of this run, never scanned even when it lies inside the tree. *)
let build_dir () =
  match Sys.getenv_opt "INSIDE_DUNE" with
  | Some b when b <> "" -> Some (realpath (Filename.dirname b))
  | _ -> None

let skipped = [ "_build"; "_opam"; ".git" ]

(* Root files that name a person by design: the copyright notice. *)
let skipped_root_files = [ "./LICENSE" ]

let rec files ~excluded dir rel =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.concat_map (fun name ->
      let path = Filename.concat dir name and rel = Filename.concat rel name in
      match (Unix.lstat path).st_kind with
      | Unix.S_DIR
        when (not (List.mem name skipped))
             && (not (List.mem (realpath path) excluded))
             && not (is_dune_build_dir path) ->
          files ~excluded path rel
      | Unix.S_REG -> [ (path, rel) ]
      | _ -> [])

let is_word c = match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

(* [occurs ~name line]: [name] (lowercase) occurs in the lowercased [line] with no ASCII
   letter or digit right before or right after it. *)
let occurs ~name line =
  let n = String.length line and k = String.length name in
  let rec at i =
    i + k <= n
    && (String.sub line i k = name
        && (i = 0 || not (is_word line.[i - 1]))
        && (i + k = n || not (is_word line.[i + k]))
       || at (i + 1))
  in
  k > 0 && at 0

let scan names (path, rel) =
  List.concat
    (List.mapi
       (fun i line ->
         let line = String.lowercase_ascii line in
         List.filter_map
           (fun name ->
             if occurs ~name line then Some (Printf.sprintf "%s:%d: %s" rel (i + 1) name)
             else None)
           names)
       (read_lines path))

let has_names dir = exists_in dir "denylist.txt" || exists_in dir Config.file_name

(* The data directory to check against. A set $LIBRARY_INDEX_DATA must name a directory
   holding denylist.txt or sources.toml (a relative one is resolved against [base], see
   {!tool_root}): anything else fails. Unset, the default data directory is used; the
   guard is skipped only when that directory does not exist. *)
let data_dir ~base =
  match Sys.getenv_opt "LIBRARY_INDEX_DATA" with
  | Some d when String.trim d <> "" ->
      let d = if Filename.is_relative d then Filename.concat base d else d in
      if has_names d then `Check d
      else
        fail "LIBRARY_INDEX_DATA=%s holds neither denylist.txt nor %s" d Config.file_name
  | _ -> (
      match Ctx.xdg_data_dir () with
      | Ok d when Sys.file_exists d ->
          if has_names d then `Check d
          else
            fail "data directory %s holds neither denylist.txt nor %s" d Config.file_name
      | _ -> `Skip)

let () =
  let root, base = tool_root () in
  match data_dir ~base with
  | `Skip -> print_endline "skipped: no data dir"
  | `Check dir ->
      let names = names dir in
      if names = [] then fail "no names in %s" dir;
      let excluded = Option.to_list (build_dir ()) in
      let tree =
        List.filter
          (fun (_, rel) -> not (List.mem rel skipped_root_files))
          (files ~excluded root ".")
      in
      let hits = List.concat_map (scan names) tree in
      List.iter print_endline hits;
      if hits <> [] then
        fail "%d sensitive name occurrence(s) in %s (names from %s)" (List.length hits)
          root dir
      else
        Printf.printf "guard: ok (%d files, %d names from %s)\n" (List.length tree)
          (List.length names) dir
