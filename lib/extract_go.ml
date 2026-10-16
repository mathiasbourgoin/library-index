open Api_item

let is_go p = Filename.check_suffix p ".go"

let select (ctx : context) =
  List.filter
    (fun p ->
      is_go p
      && (not (Filename.check_suffix p "_test.go"))
      && (not (skipped p))
      &&
      let segs = String.split_on_char '/' p in
      not (List.exists (fun s -> s = "internal" || s = "cmd") segs))
    ctx.paths
  |> List.sort_uniq compare

let detect (ctx : context) = has_root ctx "go.mod" || language_is ctx [ "Go" ]
let re s = Re.compile (Re.Perl.re s)
let package_re = re {|^package[ \t]+([A-Za-z_][A-Za-z0-9_]*)|}

let func_re =
  re
    {|^func[ \t]*(?:\([ \t]*[A-Za-z_0-9]*[ \t]*\*?[ \t]*([A-Za-z_][A-Za-z0-9_]*)(?:\[[^\]]*\])?[ \t]*\)[ \t]*)?([A-Za-z_][A-Za-z0-9_]*)|}

let decl_re = re {|^(type|var|const)[ \t]+([A-Z][A-Za-z0-9_]*)|}
let group_re = re {|^(type|var|const)[ \t]*\([ \t]*$|}
let member_re = re {|^[ \t]+([A-Z][A-Za-z0-9_]*)|}
let exported n = n <> "" && n.[0] >= 'A' && n.[0] <= 'Z'

let comment_above ls i =
  let rec up j acc =
    if j < 0 then acc
    else
      match strip_prefix ~prefix:"//" (trim ls.(j)) with
      | Some c -> up (j - 1) (trim c :: acc)
      | None -> acc
  in
  String.concat "\n" (up (i - 1) [])

let signature l =
  let l = trim l in
  let l =
    if String.ends_with ~suffix:"{" l then trim (String.sub l 0 (String.length l - 1))
    else l
  in
  Utf8.take l 300

let extract ~path text =
  let ls = lines text in
  let pkg =
    Array.to_list ls
    |> List.find_map (fun l ->
        Option.map (fun g -> Re.Group.get g 1) (Re.exec_opt package_re l))
  in
  match pkg with
  | None | Some "main" -> []
  | Some pkg ->
      let dir = Filename.dirname path in
      let prefix =
        if dir = "." || dir = "" then pkg
        else String.map (fun c -> if c = '/' then '.' else c) dir
      in
      let item ?(owner = prefix) kind name i l =
        {
          kind;
          name;
          qualified_path = owner ^ "." ^ name;
          signature = signature l;
          doc = comment_above ls i;
          line = i + 1;
        }
      in
      let group = ref None and acc = ref [] in
      Array.iteri
        (fun i l ->
          let add it = acc := it :: !acc in
          match !group with
          | Some kind ->
              if String.starts_with ~prefix:")" l then group := None
              else
                Option.iter
                  (fun g -> add (item kind (Re.Group.get g 1) i l))
                  (Re.exec_opt member_re l)
          | None -> (
              if Re.execp package_re l then (
                let doc = comment_above ls i in
                if String.starts_with ~prefix:"Package " doc then
                  add
                    {
                      kind = "package";
                      name = pkg;
                      qualified_path = prefix;
                      signature = trim l;
                      doc;
                      line = i + 1;
                    })
              else
                match Re.exec_opt group_re l with
                | Some g -> group := Some (Re.Group.get g 1)
                | None -> (
                    match Re.exec_opt decl_re l with
                    | Some g -> add (item (Re.Group.get g 1) (Re.Group.get g 2) i l)
                    | None -> (
                        match Re.exec_opt func_re l with
                        | Some g -> (
                            let name = Re.Group.get g 2 in
                            let recv = Re.Group.get_opt g 1 in
                            if exported name then
                              match recv with
                              | Some r when exported r ->
                                  add (item ~owner:(prefix ^ "." ^ r) "func" name i l)
                              | Some _ -> ()
                              | None -> add (item "func" name i l))
                        | None -> ()))))
        ls;
      List.rev !acc

let extractor = { language = "go"; detect; select; handles = is_go; extract }
