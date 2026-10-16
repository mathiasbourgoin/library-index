open Api_item

let is_rs p = Filename.check_suffix p ".rs"

(* Crate roots: the directories D with D/src/lib.rs ("" for the repository root). *)
let crate_roots paths =
  List.filter_map
    (fun p ->
      if p = "src/lib.rs" then Some ""
      else if String.ends_with ~suffix:"/src/lib.rs" p then
        Some (String.sub p 0 (String.length p - String.length "/src/lib.rs"))
      else None)
    paths

let under root p =
  let prefix = if root = "" then "src/" else root ^ "/src/" in
  Option.map (fun rest -> (root, rest)) (strip_prefix ~prefix p)

let in_crate rest = rest <> "main.rs" && not (String.starts_with ~prefix:"bin/" rest)

let select (ctx : context) =
  let roots = crate_roots ctx.paths in
  List.filter
    (fun p ->
      is_rs p
      && List.exists
           (fun root ->
             match under root p with
             | Some (_, rest) -> in_crate rest && not (skipped rest)
             | None -> false)
           roots)
    ctx.paths
  |> List.sort_uniq compare

let detect (ctx : context) = has_root ctx "Cargo.toml" || language_is ctx [ "Rust" ]

(* The crate directory and the path inside its src/: "a/src/x.rs" → ("a", "x.rs"). *)
let split_src path =
  match strip_prefix ~prefix:"src/" path with
  | Some rest -> ("", rest)
  | None -> (
      let n = String.length path in
      let rec find i =
        if i + 5 > n then None
        else if String.sub path i 5 = "/src/" then Some i
        else find (i + 1)
      in
      match find 0 with
      | Some i -> (String.sub path 0 i, String.sub path (i + 5) (n - i - 5))
      | None -> ("", Filename.basename path))

(* src/lib.rs → crate; src/a.rs, src/a/mod.rs → crate::a; src/a/b.rs → crate::a::b; a
   workspace member's paths start with its directory name instead of [crate]. *)
let module_path path =
  let root, rest = split_src path in
  let crate =
    if root = "" then "crate"
    else String.map (fun c -> if c = '-' then '_' else c) (Filename.basename root)
  in
  let segs = String.split_on_char '/' (Filename.remove_extension rest) in
  let segs =
    match List.rev segs with ("lib" | "mod") :: rev -> List.rev rev | _ -> segs
  in
  String.concat "::" (crate :: List.filter (( <> ) "") segs)

let item_re =
  Re.compile
    (Re.Perl.re
       {|^([ \t]*)pub[ \t]+(?:unsafe[ \t]+)?(?:async[ \t]+)?(?:const[ \t]+)?(?:extern[ \t]+"[^"]*"[ \t]+)?(fn|struct|enum|trait|type|const|static|mod|union)[ \t]+([A-Za-z_][A-Za-z0-9_]*)|})

let impl_re =
  Re.compile
    (Re.Perl.re
       {|^impl(?:<[^>]*>)?[ \t]+(?:[A-Za-z0-9_:<>, &']+[ \t]+for[ \t]+)?([A-Za-z_][A-Za-z0-9_]*)|})

let signature l =
  let l = trim l in
  let l =
    if String.ends_with ~suffix:"{" l then trim (String.sub l 0 (String.length l - 1))
    else l
  in
  Utf8.take l 300

let doc_lines prefix ls i =
  let rec up j acc =
    if j < 0 then acc
    else
      let t = trim ls.(j) in
      if String.starts_with ~prefix t then
        up (j - 1)
          (trim
             (String.sub t (String.length prefix)
                (String.length t - String.length prefix))
          :: acc)
      else if String.starts_with ~prefix:"#[" t then up (j - 1) acc
      else acc
  in
  String.concat "\n" (up (i - 1) [])

let extract ~path text =
  let m = module_path path in
  let ls = lines text in
  let module_doc =
    let rec go i acc =
      if i >= Array.length ls then List.rev acc
      else
        let t = trim ls.(i) in
        match strip_prefix ~prefix:"//!" t with
        | Some d -> go (i + 1) (trim d :: acc)
        | None ->
            if t = "" || String.starts_with ~prefix:"#![" t then go (i + 1) acc
            else List.rev acc
    in
    go 0 []
  in
  let head =
    if module_doc = [] then []
    else
      [
        {
          kind = "mod";
          name = m;
          qualified_path = m;
          signature = "mod " ^ m;
          doc = String.concat "\n" module_doc;
          line = 1;
        };
      ]
  in
  let impl = ref None and acc = ref [] in
  Array.iteri
    (fun i l ->
      if String.starts_with ~prefix:"}" l then impl := None
      else
        match Re.exec_opt impl_re l with
        | Some g -> impl := Some (Re.Group.get g 1)
        | None -> (
            match Re.exec_opt item_re l with
            | Some g ->
                let indented = Re.Group.get g 1 <> "" in
                let kind = Re.Group.get g 2 and name = Re.Group.get g 3 in
                let owner =
                  match (indented, !impl) with true, Some t -> m ^ "::" ^ t | _ -> m
                in
                acc :=
                  {
                    kind;
                    name;
                    qualified_path = owner ^ "::" ^ name;
                    signature = signature l;
                    doc = doc_lines "///" ls i;
                    line = i + 1;
                  }
                  :: !acc
            | None -> ()))
    ls;
  head @ List.rev !acc

let extractor = { language = "rust"; detect; select; handles = is_rs; extract }
