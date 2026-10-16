open Api_item

let is_v p = Filename.check_suffix p ".v"

let select (ctx : context) =
  List.filter (fun p -> is_v p && not (skipped p)) ctx.paths |> List.sort_uniq compare

let contains ~sub s =
  let n = String.length s and k = String.length sub in
  let rec go i = i + k <= n && (String.sub s i k = sub || go (i + 1)) in
  go 0

let detect (ctx : context) =
  has_root ctx "_CoqProject" || has_root ctx "_RocqProject"
  || language_is ctx [ "Coq"; "Rocq"; "Rocq Prover" ]
  ||
  match List.assoc_opt "dune-project" ctx.manifests with
  | Some text -> contains ~sub:"(using coq" text || contains ~sub:"(using rocq" text
  | None -> false

let re s = Re.compile (Re.Perl.re s)

let keywords =
  [
    "Definition";
    "Fixpoint";
    "CoFixpoint";
    "Inductive";
    "CoInductive";
    "Variant";
    "Theorem";
    "Lemma";
    "Corollary";
    "Proposition";
    "Fact";
    "Remark";
    "Record";
    "Structure";
    "Class";
    "Instance";
    "Axiom";
    "Parameter";
  ]

let decl_re =
  re
    (Printf.sprintf
       {|^(?:#\[[^\]]*\][ \t]*)?((?:(?:Global|Local|Polymorphic|Monomorphic|Program|Cumulative|NonCumulative|Private)[ \t]+)*)(Module[ \t]+Type|Module(?:[ \t]+(?:Import|Export))?|%s)[ \t]+([A-Za-z_][A-Za-z0-9_']*)|}
       (String.concat "|" keywords))

let section_re = re {|^Section[ \t]+([A-Za-z_][A-Za-z0-9_']*)[ \t]*\.|}
let end_re = re {|^End[ \t]+([A-Za-z_][A-Za-z0-9_']*)[ \t]*\.|}
let period_re = re {|\.([ \t]|$)|}

type frame = Mod of string | Sec of string

let frame_name = function Mod n | Sec n -> n

let find_sub ~sub s =
  let n = String.length s and k = String.length sub in
  let rec go i =
    if i + k > n then None else if String.sub s i k = sub then Some i else go (i + 1)
  in
  go 0

(* The sentence starting at line [i]: up to ":=" or its final period, at most 20 lines. *)
let sentence code i =
  let n = Array.length code in
  let buf = Buffer.create 120 and body = ref false in
  let rec go j =
    if j < n && j < i + 20 then
      let l = code.(j) in
      match (find_sub ~sub:":=" l, Re.exec_opt period_re l) with
      | Some k, Some g when Re.Group.start g 0 > k ->
          body := true;
          Buffer.add_string buf (String.sub l 0 k)
      | Some k, None ->
          body := true;
          Buffer.add_string buf (String.sub l 0 k)
      | _, Some g -> Buffer.add_string buf (String.sub l 0 (Re.Group.start g 0 + 1))
      | None, None ->
          Buffer.add_string buf l;
          Buffer.add_char buf ' ';
          go (j + 1)
  in
  go i;
  (Buffer.contents buf, !body)

let extract ~path text =
  let sc = Src_scan.scan Src_scan.Ml_like text in
  let root = Filename.remove_extension (Filename.basename path) in
  let items = ref [] and stack = ref [] in
  let modules () =
    List.rev (List.filter_map (function Mod n -> Some n | Sec _ -> None) !stack)
  in
  Array.iteri
    (fun i code ->
      let t = String.trim code in
      match Re.exec_opt end_re t with
      | Some g ->
          let name = Re.Group.get g 1 in
          if List.exists (fun f -> frame_name f = name) !stack then
            let rec pop = function
              | f :: rest -> if frame_name f = name then rest else pop rest
              | [] -> []
            in
            stack := pop !stack
      | None -> (
          match Re.exec_opt section_re t with
          | Some g -> stack := Sec (Re.Group.get g 1) :: !stack
          | None -> (
              match Re.exec_opt decl_re t with
              | None -> ()
              | Some g ->
                  let quals = Re.Group.get g 1 in
                  let kw = Src_scan.squash (Re.Group.get g 2) in
                  let name = Re.Group.get g 3 in
                  let s, has_body = sentence sc.code i in
                  let is_module = String.starts_with ~prefix:"Module" kw in
                  if not (contains ~sub:"Local" quals) then
                    items :=
                      {
                        kind =
                          String.lowercase_ascii
                            (if String.starts_with ~prefix:"Module Type" kw then kw
                             else if is_module then "Module"
                             else kw);
                        name;
                        qualified_path =
                          String.concat "." ((root :: modules ()) @ [ name ]);
                        signature = Utf8.take (Src_scan.squash s) 300;
                        doc = Src_scan.doc_above ~accept:Src_scan.is_doc_ml sc i;
                        line = i + 1;
                      }
                      :: !items;
                  (* An interactive module (no ":=") is open until its End. *)
                  if is_module && not has_body then stack := Mod name :: !stack)))
    sc.code;
  List.rev !items

let extractor = { language = "rocq"; detect; select; handles = is_v; extract }
