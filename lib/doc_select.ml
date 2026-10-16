type kind = Readme | Doc | Api

let kind_name = function Readme -> "readme" | Doc -> "doc" | Api -> "api"
let kinds = [ "readme"; "doc"; "api" ]
let max_files = 200
let lower = String.lowercase_ascii

(* README names, most preferred first. *)
let readme_names =
  [ "readme.md"; "readme.markdown"; "readme"; "readme.org"; "readme.txt"; "readme.rst" ]

let excluded_root_md = [ "license"; "changelog"; "code_of_conduct"; "contributing" ]
let is_root p = not (String.contains p '/')
let is_md p = Filename.check_suffix (lower p) ".md"
let valid p = p <> "" && p.[0] <> '/' && not (List.mem ".." (String.split_on_char '/' p))

let readme paths =
  let rank p =
    if is_root p then List.find_index (fun n -> n = lower p) readme_names else None
  in
  List.filter_map (fun p -> Option.map (fun r -> (r, p)) (rank p)) paths
  |> List.sort compare
  |> function
  | (_, p) :: _ -> Some p
  | [] -> None

let root_doc p =
  is_root p && is_md p
  && not (List.mem (Filename.remove_extension (lower p)) ("readme" :: excluded_root_md))

let rec take n = function x :: rest when n > 0 -> x :: take (n - 1) rest | _ -> []

let api_context ?language ?(manifests = []) paths =
  let language =
    match language with Some l -> Some l | None -> Classify.language_of_files paths
  in
  { Api_item.paths; manifests; language }

let api_paths ?language ?manifests paths =
  let paths = List.sort_uniq compare (List.filter valid paths) in
  take max_files (Extract.select (api_context ?language ?manifests paths))

let select ?language ?manifests paths =
  let paths = List.sort_uniq compare (List.filter valid paths) in
  let readme = readme paths in
  let docs =
    List.filter (fun p -> String.starts_with ~prefix:"docs/" p && is_md p) paths
    @ List.filter root_doc paths
  in
  let rest =
    if docs <> [] then List.map (fun p -> (p, Doc)) docs
    else
      List.map
        (fun p -> (p, Api))
        (Extract.select (api_context ?language ?manifests paths))
  in
  take max_files ((match readme with Some p -> [ (p, Readme) ] | None -> []) @ rest)

type format = Markdown | Mli | Plain

let format path =
  let l = lower path in
  if Filename.check_suffix l ".md" || Filename.check_suffix l ".markdown" then Markdown
  else if Filename.check_suffix l ".mli" then Mli
  else if is_root path && l = "readme" then Markdown
  else Plain

let module_name path =
  String.capitalize_ascii (Filename.remove_extension (Filename.basename path))

let title path text =
  match format path with
  | Markdown -> Option.value (Chunk.markdown_title text) ~default:path
  | Mli -> module_name path
  | Plain when Filename.check_suffix path ".ml" -> module_name path
  | Plain -> path

let chunks path text =
  match format path with
  | Markdown -> Chunk.markdown ~default_title:(title path text) text
  | (Mli | Plain) when Extract.for_path path <> None -> Extract.chunks ~path text
  | Mli | Plain -> Chunk.plain ~title:path text
