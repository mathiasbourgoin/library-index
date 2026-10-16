open Api_item

let is_py p = Filename.check_suffix p ".py" || Filename.check_suffix p ".pyi"

let excluded_base b =
  b = "setup.py" || b = "conftest.py" || b = "noxfile.py"
  || String.starts_with ~prefix:"test_" b
  || String.ends_with ~suffix:"_test.py" b
  || (String.starts_with ~prefix:"_" b && not (String.starts_with ~prefix:"__init__." b))

let select (ctx : context) =
  let candidates =
    List.filter
      (fun p ->
        is_py p
        && (not (skipped p))
        && (not (excluded_base (Filename.basename p)))
        && not (List.mem "docs" (String.split_on_char '/' p)))
      ctx.paths
  in
  (* A stub replaces its module. *)
  List.filter
    (fun p -> not (Filename.check_suffix p ".py" && List.mem (p ^ "i") candidates))
    candidates
  |> List.sort_uniq compare

let detect (ctx : context) =
  has_root ctx "pyproject.toml" || has_root ctx "setup.py" || has_root ctx "setup.cfg"
  || language_is ctx [ "Python" ]

let module_of path =
  let p = Option.value (strip_prefix ~prefix:"src/" path) ~default:path in
  let p = Filename.remove_extension p in
  let p = if Filename.basename p = "__init__" then Filename.dirname p else p in
  if p = "." then "" else String.map (fun c -> if c = '/' then '.' else c) p

let re s = Re.compile (Re.Perl.re s)
let def_re = re {|^(async[ \t]+def|def|class)[ \t]+([A-Za-z_][A-Za-z0-9_]*)|}
let assign_re = re {|^([A-Za-z_][A-Za-z0-9_]*)[ \t]*(?::[^=]*)?=|}
let all_start_re = re {|^__all__[ \t]*(?::[^=]*)?(?:\+)?=[ \t]*[\[(]|}
let quoted_re = re {|["']([A-Za-z_][A-Za-z0-9_]*)["']|}

(* The names of [__all__ = [...]], possibly over several lines. *)
let all_names ls =
  let n = Array.length ls in
  let rec find i =
    if i >= n then None
    else if Re.execp all_start_re ls.(i) then (
      let b = Buffer.create 128 in
      let rec collect j =
        if j < n then (
          Buffer.add_string b ls.(j);
          Buffer.add_char b '\n';
          if not (String.contains ls.(j) ']' || String.contains ls.(j) ')') then
            collect (j + 1))
      in
      collect i;
      Some (List.map (fun g -> Re.Group.get g 1) (Re.all quoted_re (Buffer.contents b))))
    else find (i + 1)
  in
  find 0

(* A definition header may span lines: up to the line ending with ":". *)
let header ls i =
  let n = Array.length ls in
  let rec go j acc =
    let acc = acc @ [ trim ls.(j) ] in
    if j + 1 >= n || String.ends_with ~suffix:":" (trim ls.(j)) || j - i >= 6 then
      (String.concat " " acc, j)
    else go (j + 1) acc
  in
  go i []

let triple_quote t =
  List.find_map
    (fun q -> if String.starts_with ~prefix:q t then Some q else None)
    [ {|"""|}; "'''"; {|r"""|}; "r'''" ]

(* The docstring opening the body after line [j]. *)
let docstring ls j =
  let n = Array.length ls in
  let rec first k = if k < n && trim ls.(k) = "" then first (k + 1) else k in
  let k = first (j + 1) in
  if k >= n then ""
  else
    let t = trim ls.(k) in
    match triple_quote t with
    | None -> ""
    | Some q ->
        let close = if String.length q = 4 then String.sub q 1 3 else q in
        let body = String.sub t (String.length q) (String.length t - String.length q) in
        let rec gather k acc body =
          match Re.exec_opt (Re.compile (Re.str close)) body with
          | Some g ->
              let stop = Re.Group.start g 0 in
              List.rev (String.sub body 0 stop :: acc)
          | None ->
              if k + 1 >= n then List.rev (body :: acc)
              else gather (k + 1) (body :: acc) ls.(k + 1)
        in
        String.trim (String.concat "\n" (List.map trim (gather k [] body)))

let extract ~path text =
  let m = module_of path in
  let ls = lines text in
  let all = all_names ls in
  let public name =
    match all with
    | Some l -> List.mem name l
    | None -> not (String.starts_with ~prefix:"_" name)
  in
  let qualified name = if m = "" then name else m ^ "." ^ name in
  let acc = ref [] in
  Array.iteri
    (fun i l ->
      match Re.exec_opt def_re l with
      | Some g ->
          let kw = Re.Group.get g 1 and name = Re.Group.get g 2 in
          if public name then
            let sig_, j = header ls i in
            let kind = if kw = "class" then "class" else "def" in
            acc :=
              {
                kind;
                name;
                qualified_path = qualified name;
                signature = Utf8.take sig_ 300;
                doc = docstring ls j;
                line = i + 1;
              }
              :: !acc
      | None -> (
          match (all, Re.exec_opt assign_re l) with
          | Some names, Some g
            when List.mem (Re.Group.get g 1) names && Re.Group.get g 1 <> "__all__" ->
              let name = Re.Group.get g 1 in
              acc :=
                {
                  kind = "const";
                  name;
                  qualified_path = qualified name;
                  signature = Utf8.take (trim l) 300;
                  doc = "";
                  line = i + 1;
                }
                :: !acc
          | _ -> ()))
    ls;
  List.rev !acc

let extractor = { language = "python"; detect; select; handles = is_py; extract }
