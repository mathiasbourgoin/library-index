let fields = [ "what"; "interest" ]

let non_empty = function
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let what_of ~description ~synopsis ~readme =
  List.find_map Fun.id
    [
      non_empty description;
      non_empty synopsis;
      Option.bind (non_empty readme) Classify.first_para;
    ]

let ownership_word = function
  | "own" -> "Own"
  | "participant" -> "Contributed"
  | _ -> "Third-party"

let interest_of ~ownership ~language ~domain =
  match (non_empty language, non_empty domain) with
  | None, None -> None
  | lang, dom ->
      Some
        (String.concat " "
           (List.filter_map Fun.id
              [
                Some (ownership_word ownership);
                lang;
                Some "code";
                Option.map (fun d -> "(" ^ d ^ ")") dom;
              ]))

let sql =
  {|SELECT id, what, interest, description, synopsis, readme, ownership, language,
  effective_domain
FROM repos WHERE fetched = 1 AND |}
  ^ Eligibility.not_confidential ()

let write db ~now ~repo_id ~field ~current candidate =
  let auto = Field_meta.get db ~repo_id ~field = Some Field_meta.Auto in
  let empty = non_empty current = None in
  if (empty || auto) && candidate <> current && not (empty && candidate = None) then (
    (* [field] is one of [fields]: a fixed column name. *)
    Db.run db
      (Printf.sprintf "UPDATE repos SET %s = ? WHERE id = ?" field)
      [ Db.rtext_opt candidate; Db.int repo_id ];
    Field_meta.set db ~repo_id ~field Auto ~now;
    1)
  else 0

let apply db ~now ?key () =
  let where, params =
    match key with
    | Some k -> (" AND key = ?", [ Db.text (Key.normalize k) ])
    | None -> ("", [])
  in
  let rows = Db.query db (sql ^ where) params (fun r -> r) in
  List.fold_left
    (fun n r ->
      let s i = Db.to_string_opt r.(i) in
      let repo_id = Option.value (Db.to_int_opt r.(0)) ~default:0 in
      let what = what_of ~description:(s 3) ~synopsis:(s 4) ~readme:(s 5) in
      let interest =
        interest_of
          ~ownership:(Option.value (s 6) ~default:"external")
          ~language:(s 7) ~domain:(s 8)
      in
      n
      + write db ~now ~repo_id ~field:"what" ~current:(s 1) what
      + write db ~now ~repo_id ~field:"interest" ~current:(s 2) interest)
    0 rows
