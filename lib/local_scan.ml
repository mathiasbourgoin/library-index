(* Local checkouts under the dev root ([refresh.py] fetch_local, local_language, enrich). *)

type checkout = {
  path : string;
  worktree : bool;
  remote_url : string option;
  remote_key : string option;
  all_remote_keys : string list;
  last_commit : string option;
  my_commits : int;
  total_commits : int;
  remotes_unreadable : bool;
}

let git_out ~call argv =
  match call { Commands.purpose = Local_git; argv } with
  | Ok s -> String.trim s
  | Error _ -> ""

(* [git remote -v]: name → URL, first-seen name order, last URL wins (Python dict). *)
let remotes out =
  List.fold_left
    (fun acc line ->
      match
        String.split_on_char ' ' (String.concat " " (String.split_on_char '\t' line))
        |> List.filter (( <> ) "")
      with
      | name :: url :: _ ->
          if List.mem_assoc name acc then
            List.map (fun (n, u) -> if n = name then (n, url) else (n, u)) acc
          else acc @ [ (name, url) ]
      | _ -> acc)
    []
    (String.split_on_char '\n' out)

let read_file p =
  try Some (In_channel.with_open_bin p In_channel.input_all) with Sys_error _ -> None

(* The git directory of a checkout: [.git], or the [gitdir:] a worktree's [.git] file
   names (its [commondir] holds the shared config). *)
let git_config dir =
  let dot = Filename.concat dir ".git" in
  let gitdir =
    if Sys.file_exists dot && Sys.is_directory dot then Some dot
    else
      match read_file dot with
      | Some s when String.starts_with ~prefix:"gitdir:" (String.trim s) ->
          let g =
            String.trim (String.sub (String.trim s) 7 (String.length (String.trim s) - 7))
          in
          let g = if Filename.is_relative g then Filename.concat dir g else g in
          let common =
            match read_file (Filename.concat g "commondir") with
            | Some c ->
                let c = String.trim c in
                if Filename.is_relative c then Filename.concat g c else c
            | None -> g
          in
          Some common
      | _ -> None
  in
  Option.bind gitdir (fun g -> read_file (Filename.concat g "config"))

let remote_re = Re.compile (Re.Perl.re {|^\s*\[\s*remote\s+"([^"]*)"\s*\]|})
let url_re = Re.compile (Re.Perl.re {|^\s*url\s*=\s*(.*?)\s*$|})

(* The remotes a config file declares, in [git remote -v] order of appearance. *)
let config_remotes text =
  List.fold_left
    (fun (cur, acc) line ->
      match Re.exec_opt remote_re line with
      | Some g -> (Some (Re.Group.get g 1), acc)
      | None when String.length (String.trim line) > 0 && (String.trim line).[0] = '[' ->
          (None, acc)
      | None -> (
          match (cur, Re.exec_opt url_re line) with
          | Some n, Some g -> (cur, acc @ [ (n, Re.Group.get g 1) ])
          | _ -> (cur, acc)))
    (None, [])
    (String.split_on_char '\n' text)
  |> snd

(* [git remote -v], else the checkout's config file read directly (git refuses some
   checkouts, e.g. another user's: safe.directory); [None] when neither is readable. *)
let read_remotes ~call dir =
  match call { Commands.purpose = Local_git; argv = Commands.git_remotes dir } with
  | Ok out -> Some (remotes (String.trim out))
  | Error _ -> Option.map config_remotes (git_config dir)

let scan_one ~(id : Identity.t) ~call dir =
  let remotes_out = read_remotes ~call dir in
  let rs = Option.value remotes_out ~default:[] in
  let url =
    match List.assoc_opt "origin" rs with
    | Some u -> Some u
    | None -> Option.map snd (List.nth_opt rs 0)
  in
  let count argv = Option.value (int_of_string_opt (git_out ~call argv)) ~default:0 in
  let last = git_out ~call (Commands.git_last_commit dir) in
  {
    path = dir;
    worktree =
      (try not (Sys.is_directory (Filename.concat dir ".git")) with Sys_error _ -> false);
    remote_url = url;
    remote_key = Option.bind url Classify.norm_remote;
    all_remote_keys =
      List.sort_uniq compare (List.filter_map (fun (_, u) -> Classify.norm_remote u) rs);
    last_commit = (if last = "" then None else Some last);
    my_commits =
      (match id.author_regex with
      | Some author -> count (Commands.git_count_mine ~author dir)
      | None -> 0);
    total_commits = count (Commands.git_count dir);
    remotes_unreadable = remotes_out = None;
  }

let scan ~id ~call dev_root =
  match Sys.readdir dev_root with
  | exception Sys_error _ -> []
  | names ->
      Array.sort compare names;
      List.filter_map
        (fun n ->
          let dir = Filename.concat dev_root n in
          if Sys.file_exists (Filename.concat dir ".git") then
            Some (scan_one ~id ~call dir)
          else None)
        (Array.to_list names)

let checkout_json c =
  `Assoc
    [
      ("path", `String c.path);
      ("worktree", `Bool c.worktree);
      ("remote_url", Jsonx.of_string_opt c.remote_url);
      ("remote_key", Jsonx.of_string_opt c.remote_key);
      ("all_remote_keys", `List (List.map (fun k -> `String k) c.all_remote_keys));
      ("last_commit", Jsonx.of_string_opt c.last_commit);
      ("my_commits", `Int c.my_commits);
      ("total_commits", `Int c.total_commits);
    ]

let to_json = checkout_json

let local_language ~call dir =
  Classify.language_of_files
    (List.filter (( <> ) "")
       (String.split_on_char '\n' (git_out ~call (Commands.git_ls_files dir))))

let enrich_row ~call ~by_remote ~by_path row =
  let key = Option.value (Norm.key row) ~default:"" in
  let is_local = Jsonx.str "host" row = Some "local" in
  let ls =
    if is_local then
      Option.to_list
        (Hashtbl.find_opt by_path
           (String.lowercase_ascii (Option.value (Jsonx.str "name" row) ~default:"")))
    else Option.value (Hashtbl.find_opt by_remote key) ~default:[]
  in
  let row =
    match (is_local, Jsonx.get "language" row, ls) with
    | true, `Null, c :: _ ->
        Jsonx.set "language" (Jsonx.of_string_opt (local_language ~call c.path)) row
    | _ -> row
  in
  let paths l = List.map (fun c -> `String c.path) l in
  let main = List.filter (fun c -> not c.worktree) ls in
  let local_paths =
    if main <> [] then paths main else paths (List.filteri (fun i _ -> i = 0) ls)
  in
  let prim = List.filter (fun c -> c.remote_key = Some key) main in
  let maxi f = `Int (List.fold_left (fun m c -> max m (f c)) 0 prim) in
  let remote_keys =
    List.concat_map (fun c -> Option.to_list c.remote_key @ c.all_remote_keys) ls
  in
  (* The checkouts' remotes travel with the row; {!Campaign.decide} reads them (INV-D). *)
  row
  |> Jsonx.set "checkout_remote_keys"
       (`List (List.map (fun k -> `String k) (List.sort_uniq compare remote_keys)))
  |> Jsonx.set "local_paths" (`List local_paths)
  |> Jsonx.set "local_worktrees"
       (`Int (List.length (List.filter (fun c -> c.worktree) ls)))
  |> Jsonx.set "local_my_commits"
       (if prim = [] then `Null else maxi (fun c -> c.my_commits))
  |> Jsonx.set "local_total_commits"
       (if prim = [] then `Null else maxi (fun c -> c.total_commits))

let enrich ~call local rows =
  let by_remote = Hashtbl.create 64 and by_path = Hashtbl.create 64 in
  List.iter
    (fun c ->
      List.iter
        (fun k ->
          Hashtbl.replace by_remote k
            (Option.value (Hashtbl.find_opt by_remote k) ~default:[] @ [ c ]))
        c.all_remote_keys;
      Hashtbl.replace by_path (String.lowercase_ascii (Filename.basename c.path)) c)
    local;
  List.map (enrich_row ~call ~by_remote ~by_path) rows

let checkout = scan_one
