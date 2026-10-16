let keys = [ "gh_member_orgs"; "gh_search_commits"; "gh_search_prs"; "gl_mr_total" ]
let set db k v = Db.set_meta db k (Yojson.Safe.to_string (Redact.json v))

let counts_of_json = function
  | `Assoc m ->
      List.filter_map
        (fun (name, v) ->
          match v with `Int n -> Some (String.lowercase_ascii name, n) | _ -> None)
        m
  | _ -> []

let search_counts db k =
  match Option.bind (Db.get_meta db k) Jsonx.parse with
  | Some j -> counts_of_json j
  | None -> []

let member_orgs db = Jsonx.stored_strings (Db.get_meta db "gh_member_orgs")
