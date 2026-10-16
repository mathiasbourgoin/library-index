open Api_item

let module_name path =
  String.capitalize_ascii (Filename.remove_extension (Filename.basename path))

(* "(** text *)" → "text". *)
let doc_text d =
  let d = trim d in
  let d = Option.value (strip_prefix ~prefix:"(**" d) ~default:d in
  let d =
    if String.ends_with ~suffix:"*)" d then String.sub d 0 (String.length d - 2) else d
  in
  String.concat "\n" (List.map trim (String.split_on_char '\n' (trim d)))

let line_of text sub =
  let n = String.length text and k = String.length sub in
  let rec find i =
    if i + k > n then None else if String.sub text i k = sub then Some i else find (i + 1)
  in
  match if k = 0 then None else find 0 with
  | None -> 0
  | Some i ->
      let c = ref 1 in
      String.iteri (fun j ch -> if j < i && ch = '\n' then incr c) text;
      !c

let split_title t =
  match String.index_opt t ' ' with
  | Some i -> (String.sub t 0 i, String.sub t (i + 1) (String.length t - i - 1))
  | None -> (t, "")

let mli_items ~path text =
  let m = module_name path in
  let module_doc, items = Chunk.mli_items text in
  let module_item =
    match module_doc with
    | [] -> []
    | docs ->
        [
          {
            kind = "module";
            name = m;
            qualified_path = m;
            signature = "module " ^ m;
            doc = String.concat "\n\n" (List.map doc_text docs);
            line = 1;
          };
        ]
  in
  module_item
  @ List.map
      (fun (it : Chunk.mli_item) ->
        let kind, name = split_title it.title in
        {
          kind;
          name;
          qualified_path = (if name = "" then m else m ^ "." ^ name);
          signature = it.signature;
          doc = String.concat "\n\n" (List.map doc_text it.docs);
          line = line_of text it.signature;
        })
      items

(* ------------------------------------------------------------ .ml without .mli *)

let ident = {|([a-z_][A-Za-z0-9_']*|\([^)]*\))|}

let rules =
  List.map
    (fun (kind, re, group) -> (kind, Re.compile (Re.Perl.re re), group))
    [
      ("let", "^let(?:[ \t]+rec)?[ \t]+" ^ ident, 1);
      ( "type",
        {|^type(?:[ \t]+nonrec)?[ \t]+(?:(?:'[a-z]+|\([^)]*\))[ \t]+)?([a-z_][A-Za-z0-9_']*)|},
        1 );
      ("module type", {|^module[ \t]+type[ \t]+([A-Z][A-Za-z0-9_']*)|}, 1);
      ("module", {|^module[ \t]+([A-Z][A-Za-z0-9_']*)|}, 1);
      ("exception", {|^exception[ \t]+([A-Z][A-Za-z0-9_']*)|}, 1);
      ("external", "^external[ \t]+" ^ ident, 1);
      ("class", {|^class(?:[ \t]+type)?[ \t]+([a-z_][A-Za-z0-9_']*)|}, 1);
    ]

let match_item l =
  List.find_map
    (fun (kind, re, g) ->
      match Re.exec_opt re l with
      | Some grp -> Some (kind, Re.Group.get grp g)
      | None -> None)
    rules

(* The signature of a definition: its first line, cut before the body ("=" ending the
   header) for [let]. *)
let header kind l =
  let l = trim l in
  if kind <> "let" then l
  else
    match String.rindex_opt l '=' with
    | Some i when i = String.length l - 1 -> trim (String.sub l 0 i)
    | _ -> l

let ml_items ~path text =
  let m = module_name path in
  let ls = lines text in
  let doc = ref None and doc_end = ref (-2) and acc = ref [] in
  let in_doc = ref None in
  Array.iteri
    (fun i l ->
      match !in_doc with
      | Some buf ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf l;
          if Api_item.trim l |> String.ends_with ~suffix:"*)" then (
            doc := Some (doc_text (Buffer.contents buf));
            doc_end := i;
            in_doc := None)
      | None -> (
          if
            String.starts_with ~prefix:"(**" l
            && not (String.starts_with ~prefix:"(***" l)
          then (
            let buf = Buffer.create 128 in
            Buffer.add_string buf l;
            if String.ends_with ~suffix:"*)" (trim l) then (
              doc := Some (doc_text l);
              doc_end := i)
            else in_doc := Some buf)
          else
            match match_item l with
            | Some (_, name) when name = "()" || String.starts_with ~prefix:"_" name -> ()
            | Some (kind, name) when not (String.starts_with ~prefix:"let open" l) ->
                let d = if !doc_end = i - 1 then Option.value !doc ~default:"" else "" in
                acc :=
                  {
                    kind;
                    name;
                    qualified_path = m ^ "." ^ name;
                    signature = header kind l;
                    doc = d;
                    line = i + 1;
                  }
                  :: !acc
            | _ -> ()))
    ls;
  List.rev !acc

(* ------------------------------------------------------------ the extractor *)

let is_mli p = Filename.check_suffix p ".mli"
let is_ml p = Filename.check_suffix p ".ml"

let select (ctx : context) =
  let candidates = List.filter (fun p -> not (skipped p)) ctx.paths in
  let mlis = List.filter is_mli candidates in
  let has_mli p = List.mem (Filename.remove_extension p ^ ".mli") mlis in
  List.sort_uniq compare
    (mlis @ List.filter (fun p -> is_ml p && not (has_mli p)) candidates)

let detect (ctx : context) =
  has_root ctx "dune-project"
  || List.exists
       (fun p -> (not (String.contains p '/')) && Filename.check_suffix p ".opam")
       ctx.paths
  || language_is ctx [ "OCaml" ]

let extractor =
  {
    language = "ocaml";
    detect;
    select;
    handles = (fun p -> is_mli p || is_ml p);
    extract =
      (fun ~path text ->
        if is_mli path then mli_items ~path text else ml_items ~path text);
  }
