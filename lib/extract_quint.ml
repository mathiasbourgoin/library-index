open Api_item

let is_qnt p = Filename.check_suffix p ".qnt"

let select (ctx : context) =
  List.filter (fun p -> is_qnt p && not (skipped p)) ctx.paths |> List.sort_uniq compare

let detect (ctx : context) = language_is ctx [ "Quint" ] || select ctx <> []
let re s = Re.compile (Re.Perl.re s)
let module_re = re {|^module[ \t]+([A-Za-z_][A-Za-z0-9_]*)|}

let decl_re =
  re
    {|^(?:(?:pure|nondet)[ \t]+)?(def|val|action|temporal|type|const|var|run)[ \t]+([A-Za-z_][A-Za-z0-9_]*)|}

let count c s = String.fold_left (fun n x -> if x = c then n + 1 else n) 0 s

let extract ~path:_ text =
  let sc = Src_scan.scan Src_scan.Slash_like text in
  let items = ref [] in
  (* The open modules, innermost first, with the brace depth of their body. *)
  let modules = ref [] and depth = ref 0 in
  Array.iteri
    (fun i code ->
      let t = String.trim code in
      let add kind name qualified_path =
        items :=
          {
            kind;
            name;
            qualified_path;
            signature = Utf8.take (Src_scan.squash t) 300;
            doc = Src_scan.doc_above sc i;
            line = i + 1;
          }
          :: !items
      in
      let path () = List.rev_map fst !modules in
      (match Re.exec_opt module_re t with
      | Some g ->
          let name = Re.Group.get g 1 in
          add "module" name (String.concat "." (path () @ [ name ]));
          modules := (name, !depth + 1) :: !modules
      | None -> (
          match (!modules, Re.exec_opt decl_re t) with
          | (_, d) :: _, Some g when d = !depth ->
              let name = Re.Group.get g 2 in
              add (Re.Group.get g 1) name (String.concat "." (path () @ [ name ]))
          | _ -> ()));
      depth := !depth + count '{' code - count '}' code;
      modules := List.filter (fun (_, d) -> d <= !depth) !modules)
    sc.code;
  List.rev !items

let extractor = { language = "quint"; detect; select; handles = is_qnt; extract }
