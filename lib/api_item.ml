type item = {
  kind : string;
  name : string;
  qualified_path : string;
  signature : string;
  doc : string;
  line : int;
}

type context = {
  paths : string list;
  manifests : (string * string) list;
  language : string option;
}

type extractor = {
  language : string;
  detect : context -> bool;
  select : context -> string list;
  handles : string -> bool;
  extract : path:string -> string -> item list;
}

let manifest_files =
  [
    "dune-project";
    "package.json";
    "tsconfig.json";
    "Cargo.toml";
    "go.mod";
    "pyproject.toml";
    "setup.py";
    "setup.cfg";
  ]

let skipped_dirs =
  [
    "vendor";
    "node_modules";
    "_build";
    "_opam";
    "target";
    "dist";
    "build";
    "test";
    "tests";
    "__tests__";
    "testdata";
    "examples";
    ".git";
  ]

let lower = String.lowercase_ascii

let contains ~sub s =
  let n = String.length s and k = String.length sub in
  let rec go i = i + k <= n && (String.sub s i k = sub || go (i + 1)) in
  k = 0 || go 0

let generated_name base =
  let b = lower base in
  String.ends_with ~suffix:".min.js" b
  || String.ends_with ~suffix:".pb.go" b
  || contains ~sub:"_generated." b || contains ~sub:".generated." b
  || contains ~sub:".gen." b
  || String.starts_with ~prefix:"zz_generated" b

let skipped p =
  let segs = String.split_on_char '/' p in
  let dirs = List.filteri (fun i _ -> i < List.length segs - 1) segs in
  List.exists (fun s -> List.mem (lower s) skipped_dirs) dirs
  || generated_name (Filename.basename p)

let lines s =
  Array.of_list
    (List.map
       (fun l ->
         if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1)
         else l)
       (String.split_on_char '\n' s))

let generated_text s =
  let ls = lines s in
  let head = Array.to_list (Array.sub ls 0 (min 5 (Array.length ls))) in
  List.exists
    (fun l ->
      contains ~sub:"DO NOT EDIT" l || contains ~sub:"@generated" l
      || contains ~sub:"Code generated" l)
    head

let has_root (ctx : context) name = List.mem name ctx.paths

let language_is (ctx : context) names =
  match ctx.language with
  | Some l -> List.mem (lower l) (List.map lower names)
  | None -> false

let trim = String.trim

let strip_prefix ~prefix s =
  if String.starts_with ~prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let cap s = Utf8.take s Chunk.max_chars

let chunk it =
  let text = if it.doc = "" then it.signature else it.signature ^ "\n\n" ^ it.doc in
  { Chunk.title = it.kind ^ " " ^ it.qualified_path; text = cap (trim text) }
