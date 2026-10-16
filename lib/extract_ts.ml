open Api_item

let exts =
  [ ".d.ts"; ".d.mts"; ".d.cts"; ".ts"; ".tsx"; ".mts"; ".cts"; ".js"; ".mjs"; ".cjs" ]

let handles p = List.exists (fun e -> Filename.check_suffix p e) exts

let is_dts p =
  List.exists (fun e -> Filename.check_suffix p e) [ ".d.ts"; ".d.mts"; ".d.cts" ]

let strip_ext p =
  match List.find_opt (fun e -> Filename.check_suffix p e) exts with
  | Some e -> String.sub p 0 (String.length p - String.length e)
  | None -> p

let module_of path =
  let m = strip_ext path in
  let m =
    List.fold_left
      (fun m prefix -> Option.value (strip_prefix ~prefix m) ~default:m)
      m [ "src/"; "lib/" ]
  in
  let m = if m = "index" then "" else m in
  let m =
    if String.ends_with ~suffix:"/index" m then String.sub m 0 (String.length m - 6)
    else m
  in
  String.map (fun c -> if c = '/' then '.' else c) m

(* ------------------------------------------------------------ entry points *)

let rec strings_of = function
  | `String s -> [ s ]
  | `Assoc m -> List.concat_map (fun (_, v) -> strings_of v) m
  | `List l -> List.concat_map strings_of l
  | _ -> []

let norm p = Option.value (strip_prefix ~prefix:"./" p) ~default:p

(* A declared entry, plus the source it is usually built from. *)
let candidates e =
  let e = norm e in
  let stem = strip_ext e in
  let srcs =
    List.concat_map
      (fun d ->
        match strip_prefix ~prefix:(d ^ "/") stem with
        | Some rest ->
            [ "src/" ^ rest ^ ".ts"; "src/" ^ rest ^ ".tsx"; "src/" ^ rest ^ ".js" ]
        | None -> [])
      [ "dist"; "lib"; "build"; "out" ]
  in
  e :: (stem ^ ".ts") :: (stem ^ ".d.ts") :: srcs

let entries (ctx : context) =
  match List.assoc_opt "package.json" ctx.manifests with
  | None -> []
  | Some text -> (
      match Jsonx.parse text with
      | Some (`Assoc m) ->
          List.concat_map
            (fun k -> match List.assoc_opt k m with Some v -> strings_of v | None -> [])
            [ "types"; "typings"; "exports"; "module"; "main" ]
          |> List.filter handles
      | _ -> [])

let select (ctx : context) =
  let present p = List.mem p ctx.paths in
  let dts = List.filter (fun p -> is_dts p && not (skipped p)) ctx.paths in
  let declared = List.filter present (List.concat_map candidates (entries ctx)) in
  let fallback =
    if declared <> [] || dts <> [] then []
    else
      List.filter present
        [
          "index.ts";
          "src/index.ts";
          "index.d.ts";
          "index.js";
          "src/index.js";
          "index.mjs";
        ]
  in
  List.sort_uniq compare (dts @ declared @ fallback)

let detect (ctx : context) =
  has_root ctx "package.json" || has_root ctx "tsconfig.json"
  || language_is ctx [ "TypeScript"; "JavaScript" ]

(* ------------------------------------------------------------ items *)

let decl_re =
  Re.compile
    (Re.Perl.re
       {|^[ \t]*(?:export[ \t]+(?:declare[ \t]+)?(?:default[ \t]+)?|declare[ \t]+)(?:async[ \t]+)?(?:abstract[ \t]+)?(function\*?|class|interface|type|const|let|var|enum|namespace|module)[ \t]+([A-Za-z_$][A-Za-z0-9_$]*)|})

let ambient_re =
  Re.compile
    (Re.Perl.re
       {|^(function|class|interface|type|const|let|var|enum|namespace)[ \t]+([A-Za-z_$][A-Za-z0-9_$]*)|})

let kind_of = function "function*" -> "function" | "let" | "var" -> "const" | k -> k

(* "/** a\n * b */" → "a\nb". *)
let jsdoc_text raw =
  lines raw |> Array.to_list
  |> List.map (fun l ->
      let l = trim l in
      let l = Option.value (strip_prefix ~prefix:"/**" l) ~default:l in
      let l =
        if String.ends_with ~suffix:"*/" l then String.sub l 0 (String.length l - 2)
        else l
      in
      let l = trim l in
      trim (Option.value (strip_prefix ~prefix:"*" l) ~default:l))
  |> List.filter (( <> ) "")
  |> String.concat "\n"

let signature l =
  let l = trim l in
  let l =
    if String.ends_with ~suffix:"{" l then trim (String.sub l 0 (String.length l - 1))
    else l
  in
  Utf8.take l 300

let extract ~path text =
  let m = module_of path in
  let ls = lines text in
  let dts = is_dts path in
  let doc = ref "" and doc_end = ref (-2) and buf = ref None and acc = ref [] in
  Array.iteri
    (fun i l ->
      match !buf with
      | Some b ->
          Buffer.add_char b '\n';
          Buffer.add_string b l;
          if String.ends_with ~suffix:"*/" (trim l) then (
            doc := jsdoc_text (Buffer.contents b);
            doc_end := i;
            buf := None)
      | None -> (
          let t = trim l in
          if String.starts_with ~prefix:"/**" t then (
            if String.ends_with ~suffix:"*/" t then (
              doc := jsdoc_text t;
              doc_end := i)
            else
              let b = Buffer.create 128 in
              Buffer.add_string b t;
              buf := Some b)
          else if String.starts_with ~prefix:"@" t then (
            if
              (* A decorator between the doc and the declaration keeps the doc. *)
              !doc_end = i - 1
            then doc_end := i)
          else
            let found =
              match Re.exec_opt decl_re l with
              | Some g -> Some g
              | None -> if dts then Re.exec_opt ambient_re l else None
            in
            match found with
            | Some g ->
                let kind = kind_of (Re.Group.get g 1) and name = Re.Group.get g 2 in
                acc :=
                  {
                    kind;
                    name;
                    qualified_path = (if m = "" then name else m ^ "." ^ name);
                    signature = signature l;
                    doc = (if !doc_end = i - 1 then !doc else "");
                    line = i + 1;
                  }
                  :: !acc
            | None -> ()))
    ls;
  List.rev !acc

let extractor = { language = "typescript"; detect; select; handles; extract }
