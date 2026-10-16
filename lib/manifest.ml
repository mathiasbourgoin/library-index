let re s = Re.compile (Re.Perl.re s)
let lower = String.lowercase_ascii

(* The first [name = "x"] of a TOML table ([package], [project], …). *)
let toml_name ~tables text =
  let header = re {|^[ \t]*\[([A-Za-z0-9_.-]+)\][ \t]*(#.*)?$|} in
  let name = re {|^[ \t]*name[ \t]*=[ \t]*["']([^"']+)["']|} in
  let current = ref "" and found = ref None in
  List.iter
    (fun l ->
      if !found = None then
        match Re.exec_opt header l with
        | Some g -> current := Re.Group.get g 1
        | None -> (
            if List.mem !current tables then
              match Re.exec_opt name l with
              | Some g -> found := Some (Re.Group.get g 1)
              | None -> ()))
    (String.split_on_char '\n' text);
  !found

let package_json text =
  match Jsonx.parse text with
  | Some j -> (
      match Jsonx.str "name" j with
      | Some n when String.length n > 0 && n.[0] = '@' -> (
          match String.index_opt n '/' with
          | Some i -> [ n; String.sub n (i + 1) (String.length n - i - 1) ]
          | None -> [ n ])
      | Some n -> [ n ]
      | None -> [])
  | None -> []

let go_mod text =
  let module_re = re {|^module[ \t]+([^ \t\r]+)|} in
  match List.find_map (Re.exec_opt module_re) (String.split_on_char '\n' text) with
  | None -> []
  | Some g -> (
      let segs = List.rev (String.split_on_char '/' (Re.Group.get g 1)) in
      match segs with
      | v :: name :: _ when Re.execp (re {|^v[0-9]+$|}) v -> [ name ]
      | name :: _ -> [ name ]
      | [] -> [])

let dune_project text =
  match Re.exec_opt (re {|\(name[ \t\r\n]+([A-Za-z0-9_.-]+)[ \t\r\n]*\)|}) text with
  | Some g -> [ Re.Group.get g 1 ]
  | None -> []

let of_manifest (path, text) =
  match path with
  | "package.json" -> package_json text
  | "Cargo.toml" -> Option.to_list (toml_name ~tables:[ "package" ] text)
  | "pyproject.toml" ->
      Option.to_list (toml_name ~tables:[ "project"; "tool.poetry" ] text)
  | "go.mod" -> go_mod text
  | "dune-project" -> dune_project text
  | _ -> []

let opam_names paths =
  List.filter_map
    (fun p ->
      if (not (String.contains p '/')) && Filename.check_suffix p ".opam" then
        Some (Filename.chop_suffix p ".opam")
      else None)
    paths

let valid a =
  a <> ""
  && String.length a <= 214
  && String.for_all (fun c -> Char.code c > 32 && Char.code c < 127 && c <> '"') a

let aliases ~paths ~manifests =
  opam_names paths @ List.concat_map of_manifest manifests
  |> List.map (fun a -> lower (String.trim a))
  |> List.filter valid |> List.sort_uniq compare
