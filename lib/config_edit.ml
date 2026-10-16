let ( let* ) = Result.bind

type table = Repo | Source

let header_name = function Repo -> "repo" | Source -> "source"

(* ------------------------------------------------------------ lines *)

let is_space c = c = ' ' || c = '\t' || c = '\r'

let trim_left s =
  let n = String.length s in
  let rec go i = if i < n && is_space s.[i] then go (i + 1) else i in
  let i = go 0 in
  String.sub s i (n - i)

(* A table header line: [name] or [[name]], optionally followed by a comment. A line of a
   multi-line array never matches: it would hold a comma, a quote or a brace. *)
let header_re =
  Re.compile (Re.Perl.re {|^[ \t]*\[\[?[ \t]*[A-Za-z0-9_.-]+[ \t]*\]\]?[ \t]*(#.*)?\r?$|})

let is_header l = Re.execp header_re l

let array_header_re name =
  Re.compile
    (Re.Perl.re (Printf.sprintf {|^[ \t]*\[\[[ \t]*%s[ \t]*\]\][ \t]*(#.*)?\r?$|} name))

let is_content l =
  let t = trim_left l in
  t <> "" && t.[0] <> '#'

(* Each block of [table]: (first line, last line) indices, inclusive. A block runs from
   its header to the last content line before the next header: trailing blank lines and
   comments stay outside, with whatever follows. *)
let blocks lines table =
  let re = array_header_re (header_name table) in
  let n = Array.length lines in
  let rec next_header i =
    if i >= n || is_header lines.(i) then i else next_header (i + 1)
  in
  let rec last_content i stop acc =
    if i >= stop then acc
    else last_content (i + 1) stop (if is_content lines.(i) then i else acc)
  in
  let rec go i acc =
    if i >= n then List.rev acc
    else if Re.execp re lines.(i) then
      let stop = next_header (i + 1) in
      go stop ((i, last_content (i + 1) stop i) :: acc)
    else go (i + 1) acc
  in
  go 0 []

let split text = Array.of_list (String.split_on_char '\n' text)
let sub lines a b = Array.to_list (Array.sub lines a (b - a + 1))

(* ------------------------------------------------------------ rendering *)

(* Every value written to sources.toml goes through {!Redact.text}: no credential is
   ever persisted, whatever the caller passed. *)
let quote s =
  let s = Redact.text s in
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string b (Printf.sprintf "\\u%04X" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let line k v = Printf.sprintf "%s = %s" k v
let bool b = if b then "true" else "false"

let render_repo (r : Config.repo) =
  String.concat "\n"
    ([ "[[repo]]"; line "target" (quote r.target) ]
    @ Option.to_list (Option.map (fun k -> line "key" (quote k)) r.key)
    @ Option.to_list
        (Option.map
           (fun o -> line "ownership" (quote (Config.ownership_name o)))
           r.ownership)
    @ (if r.own then [ line "own" "true" ] else [])
    @ [ line "confidential" (quote (Config.confidential_name r.confidential)) ]
    @ (if r.campaign then [ line "campaign" "true" ] else [])
    @ (if r.campaign_prefix then [ line "campaign_prefix" "true" ] else [])
    @ if r.deep then [ line "deep" "true" ] else [])

let render_source (s : Config.source) =
  String.concat "\n"
    ([ "[[source]]"; line "kind" (quote (Config.kind_name s.kind)) ]
    @ (if s.name = "" then [] else [ line "name" (quote s.name) ])
    @ Option.to_list (Option.map (fun h -> line "host" (quote h)) s.host)
    @ [ line "own" (bool s.own); line "probe" (bool s.probe) ]
    @ (if s.include_subgroups then [] else [ line "include_subgroups" "false" ])
    @ [ line "confidential" (quote (Config.confidential_name s.confidential)) ]
    @ if s.campaign_prefix then [ line "campaign_prefix" "true" ] else [])

(* ------------------------------------------------------------ matching *)

let parse_block lines (a, b) = Config.of_string (String.concat "\n" (sub lines a b))

let find lines table pred =
  List.find_opt
    (fun blk ->
      match parse_block lines blk with
      | Ok c -> (
          match table with
          | Repo -> List.exists (fun r -> pred (`Repo r)) c.repos
          | Source -> List.exists (fun s -> pred (`Source s)) c.sources)
      | Error _ -> false)
    (blocks lines table)

(* ------------------------------------------------------------ edits *)

type outcome = Created | Updated

let validate text =
  Result.map_error
    (fun e -> "edited sources.toml is invalid: " ^ e)
    (Config.of_string text)

(* [key = value] lines: the key and the value text (a trailing comment included). *)
let kv_re = Re.compile (Re.Perl.re {|^[ \t]*([A-Za-z0-9_-]+)[ \t]*=[ \t]*(.*?)[ \t\r]*$|})

let kv l =
  match Re.exec_opt kv_re l with
  | Some g -> Some (Re.Group.get g 1, Re.Group.get g 2)
  | None -> None

(* A stored value equals the rendered one, possibly followed by a comment. *)
let same_value ~stored ~value =
  stored = value
  || String.starts_with ~prefix:value stored
     &&
     let rest =
       String.sub stored (String.length value) (String.length stored - String.length value)
     in
     let r = trim_left rest in
     r <> "" && r.[0] = '#' && String.length rest > String.length r

(* The edited block keeps its header line, comments, blank lines and every unchanged key
   line byte for byte; a changed key line is rewritten in place, a key the new entry
   drops is removed and a new key is appended after the block's last key line. [None]
   when the old block holds a line that is not [key = value] (a multi-line value): the
   caller then replaces the whole block. *)
let merge_block old_lines block =
  match split block |> Array.to_list with
  | [] -> None
  | _new_header :: new_kv_lines -> (
      let wanted = List.filter_map kv new_kv_lines in
      match old_lines with
      | [] -> None
      | header :: body ->
          let rec go used acc = function
            | [] -> Some (List.rev acc, used)
            | l :: rest when not (is_content l) -> go used (l :: acc) rest
            | l :: rest -> (
                match kv l with
                | None -> None
                | Some (k, stored) -> (
                    match List.assoc_opt k wanted with
                    | None -> go used acc rest
                    | Some value when same_value ~stored ~value ->
                        go (k :: used) (l :: acc) rest
                    | Some value -> go (k :: used) (line k value :: acc) rest))
          in
          Option.map
            (fun (kept, used) ->
              let added =
                List.filter_map
                  (fun (k, v) -> if List.mem k used then None else Some (line k v))
                  wanted
              in
              String.concat "\n" ((header :: kept) @ added))
            (go [] [] body))

(* The merged block must parse to the same entry as the rendered one, else the whole
   block is replaced. *)
let same_entry a b =
  match (Config.of_string a, Config.of_string b) with
  | Ok x, Ok y -> x.repos = y.repos && x.sources = y.sources
  | _ -> false

let upsert text table ~pred ~block =
  let lines = split text in
  let* text, outcome =
    match find lines table pred with
    | Some (a, b) ->
        let before = Array.to_list (Array.sub lines 0 a) in
        let after =
          Array.to_list (Array.sub lines (b + 1) (Array.length lines - b - 1))
        in
        let edited =
          match merge_block (sub lines a b) block with
          | Some m when same_entry m block -> m
          | _ -> block
        in
        Ok (String.concat "\n" (before @ [ edited ] @ after), Updated)
    | None ->
        let sep =
          if text = "" then ""
          else if String.ends_with ~suffix:"\n\n" text then ""
          else if String.ends_with ~suffix:"\n" text then "\n"
          else "\n\n"
        in
        Ok (text ^ sep ^ block ^ "\n", Created)
  in
  let* _ = validate text in
  Ok (text, outcome)

let blank l = String.trim l = ""

let remove text table ~pred =
  let lines = split text in
  match find lines table pred with
  | None -> Ok None
  | Some (a, b) ->
      let n = Array.length lines in
      (* Drop one separating blank line so that removal undoes an append. *)
      let a =
        if a > 0 && blank lines.(a - 1) && (b + 1 >= n || blank lines.(b + 1)) then a - 1
        else a
      in
      let before = Array.to_list (Array.sub lines 0 a) in
      let after = Array.to_list (Array.sub lines (b + 1) (n - b - 1)) in
      let text = String.concat "\n" (before @ after) in
      let* _ = validate text in
      Ok (Some text)

(* ------------------------------------------------------------ files *)

let read path =
  if not (Sys.file_exists path) then Ok ""
  else
    match In_channel.with_open_bin path In_channel.input_all with
    | s -> Ok s
    | exception Sys_error _ -> Error (Printf.sprintf "cannot read %s" path)

(* The file a write to [path] must replace: [path] itself, or, through any chain of
   symlinks (a dangling last link included), the file the chain names. A relative link is
   resolved against its own directory. *)
let rec link_target ?(hops = 0) path =
  match Unix.lstat path with
  | { st_kind = S_LNK; _ } when hops < 40 ->
      let dest = Unix.readlink path in
      let dest =
        if Filename.is_relative dest then Filename.concat (Filename.dirname path) dest
        else dest
      in
      link_target ~hops:(hops + 1) dest
  | { st_kind = S_LNK; _ } -> raise (Unix.Unix_error (Unix.ELOOP, "readlink", path))
  | _ -> path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> path

(* A symlinked sources.toml is edited through the link: the temporary file is created
   beside the link's target and renamed over it, with the target's permissions (0600 for
   a target that does not exist yet). The link itself is never replaced. *)
let write_atomic path text =
  match link_target path with
  | exception Unix.Unix_error _ -> Error (Printf.sprintf "cannot write %s" path)
  | target -> (
      let perm = try (Unix.stat target).st_perm with Unix.Unix_error _ -> 0o600 in
      match Fsio.atomic_write ~perm target text with
      | () -> Ok ()
      | exception (Sys_error _ | Unix.Unix_error _) ->
          Error (Printf.sprintf "cannot write %s" path))

let lock_file = ".sources.toml.lock"

let with_lock data_dir f =
  let* () = Ctx.ensure_dir data_dir in
  Lockfile.with_lock ~blocking:true ~busy:"sources.toml is locked"
    (Filename.concat data_dir lock_file)
    f
