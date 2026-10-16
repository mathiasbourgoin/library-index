type tool =
  | Search
  | Get
  | List
  | Resolve
  | Query_docs
  | Find_capability
  | Copies
  | Explain
  | Whats_new
  | Other

let cap = 2000
let note = "\n… (cut; the complete result is in structuredContent)"
let s k j = Option.value (Jsonx.str k j) ~default:""
let items k j = match Jsonx.get k j with `List l -> l | _ -> []
let opt_line label k j = match Jsonx.str k j with Some v -> [ label ^ v ] | None -> []

let one_line text =
  String.map (fun c -> if c = '\n' || c = '\r' then ' ' else c) (Utf8.take text 300)

let search j =
  Printf.sprintf "search %S: %d hits (mode %s)" (s "query" j)
    (Option.value (Jsonx.int_opt "count" j) ~default:0)
    (s "mode" j)
  :: List.concat_map
       (fun h ->
         [ Printf.sprintf "- %s — %s" (s "key" h) (one_line (s "what" h)) ]
         @ opt_line "  " "uri" h
         @
         match Jsonx.member "excerpt" h with
         | Some e ->
             [
               Printf.sprintf "  excerpt: %s (%s) %s" (s "title" e) (s "path" e)
                 (s "uri" e);
             ]
         | None -> [])
       (items "hits" j)

let get j =
  [ Printf.sprintf "%s — %s" (s "key" j) (one_line (s "what" j)) ]
  @ [
      Printf.sprintf "status: %s; tier: %s; language: %s" (s "status" j) (s "tier" j)
        (s "language" j);
    ]
  @ opt_line "url: " "url" j @ opt_line "uri: " "uri" j @ opt_line "rules: " "rules" j
  @ [
      Printf.sprintf "documents: %d; README: %d bytes (full entry in structuredContent)"
        (List.length (items "documents" j))
        (String.length (s "readme" j));
    ]

let list j =
  Printf.sprintf "%d entries; %d shown from offset %d"
    (Option.value (Jsonx.int_opt "total" j) ~default:0)
    (Option.value (Jsonx.int_opt "count" j) ~default:0)
    (Option.value (Jsonx.int_opt "offset" j) ~default:0)
  :: List.map
       (fun r -> Printf.sprintf "- %s — %s" (s "key" r) (one_line (s "what" r)))
       (items "repos" j)

let resolve j =
  match items "candidates" j with
  | [] -> [ s "hint" j ]
  | cands ->
      List.concat_map
        (fun c ->
          [
            Printf.sprintf "- %s — %s%s" (s "id" c)
              (one_line (s "what" c))
              (match Jsonx.str "maintained_copy" c with
              | Some m -> " (maintained copy: " ^ m ^ ")"
              | None -> "");
          ]
          @ opt_line "  " "uri" c)
        cands

let query_docs j =
  let lib = Jsonx.get "library" j in
  [ Printf.sprintf "%s (%s)" (s "id" lib) (s "uri" lib) ]
  @ opt_line "rules: " "rules" j @ opt_line "" "hint" j
  @ List.concat_map
      (fun e ->
        [ Printf.sprintf "- %s (%s)" (s "title" e) (s "path" e) ]
        @ (match Jsonx.str "snippet" e with
          | Some v -> [ "  " ^ one_line v ]
          | None -> [])
        @ opt_line "  " "uri" e)
      (items "excerpts" j)

let find_capability j =
  Printf.sprintf "find-capability %S: %d results (mode %s)" (s "query" j)
    (Option.value (Jsonx.int_opt "count" j) ~default:0)
    (s "mode" j)
  :: List.concat_map
       (fun r ->
         let lib = Jsonx.get "library" r and item = Jsonx.get "item" r in
         [
           Printf.sprintf "- %s: %s %s (%s%s) [%s]" (s "id" lib) (s "kind" item)
             (s "qualified_path" item) (s "file" item)
             (match Jsonx.int_opt "line" item with
             | Some l -> ":" ^ string_of_int l
             | None -> "")
             (s "source" item);
         ]
         @ (match Jsonx.str "signature" item with
           | Some v -> [ "  " ^ one_line v ]
           | None -> [])
         @ (match Jsonx.str "doc_excerpt" item with
           | Some v when String.trim v <> "" -> [ "  " ^ one_line v ]
           | _ -> [])
         @ opt_line "  " "uri" r)
       (items "results" j)

let copies j =
  match items "groups" j with
  | [] -> [ s "hint" j ]
  | groups ->
      List.concat_map
        (fun g ->
          Printf.sprintf "maintained: %s" (s "maintained" g)
          :: List.map
               (fun m ->
                 Printf.sprintf "- %s%s%s; last commit %s; pushed %s%s" (s "key" m)
                   (if Jsonx.get "maintained" m = `Bool true then " (maintained)" else "")
                   (if Jsonx.get "archived" m = `Bool true then " (archived)" else "")
                   (s "last_commit" m) (s "pushed_at" m)
                   (match items "local_paths" m with
                   | [] -> ""
                   | ps ->
                       "; local: "
                       ^ String.concat ", "
                           (List.map (function `String p -> p | _ -> "") ps)))
               (items "members" g))
        groups

let explain j =
  let copies = Jsonx.get "copies" j and counts = Jsonx.get "api_item_count" j in
  let analysis = Jsonx.get "analysis" j in
  [ Printf.sprintf "%s — %s" (s "key" j) (one_line (s "what" j)) ]
  @ opt_line "interest: " "interest" j
  @ [
      Printf.sprintf "status: %s; tier: %s; ownership: %s; domain: %s" (s "status" j)
        (s "tier" j) (s "ownership" j) (s "domain" j);
    ]
  @ opt_line "rules: " "rules" j
  @ [
      Printf.sprintf "usage: %d records; copies: %d (maintained: %s)"
        (List.length (items "usage" j))
        (Option.value (Jsonx.int_opt "count" copies) ~default:0)
        (s "maintained" copies);
      Printf.sprintf "documents: %d; API items: %d; last commit: %s"
        (Option.value (Jsonx.int_opt "doc_count" j) ~default:0)
        (Option.value (Jsonx.int_opt "total" counts) ~default:0)
        (s "last_commit" j);
      (match analysis with
      | `Null -> "analysis: none"
      | a ->
          Printf.sprintf "analysis: %s%s" (s "status" a)
            (match Jsonx.str "cause" a with Some c -> " (" ^ c ^ ")" | None -> ""));
    ]
  @ opt_line "uri: " "uri" j

let whats_new j =
  Printf.sprintf "%d repositories changed since %s (%d shown)"
    (Option.value (Jsonx.int_opt "total" j) ~default:0)
    (s "since" j)
    (Option.value (Jsonx.int_opt "count" j) ~default:0)
  :: List.map
       (fun r ->
         let ch = Jsonx.get "changes" r in
         Printf.sprintf "- %s %s%s%s" (s "latest" r) (s "key" r)
           (if Jsonx.member "metadata" ch <> None then " [metadata]" else "")
           (match Jsonx.member "docs" ch with
           | Some d ->
               Printf.sprintf " [%d documents]"
                 (Option.value (Jsonx.int_opt "count" d) ~default:0)
           | None -> ""))
       (items "repos" j)

let limit text =
  if Utf8.length text <= cap then text else Utf8.take text (cap - Utf8.length note) ^ note

let render ?(full = false) tool json =
  if full then Yojson.Safe.to_string json
  else
    let lines =
      match tool with
      | Search -> search json
      | Get -> get json
      | List -> list json
      | Resolve -> resolve json
      | Query_docs -> query_docs json
      | Find_capability -> find_capability json
      | Copies -> copies json
      | Explain -> explain json
      | Whats_new -> whats_new json
      | Other -> [ Yojson.Safe.to_string json ]
    in
    limit (String.concat "\n" lines)
