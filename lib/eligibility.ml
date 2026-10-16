let col alias c = if alias = "" then c else alias ^ "." ^ c

let not_confidential ?(alias = "") () =
  Printf.sprintf "%s = 0 AND %s <> 'whitehat'" (col alias "campaign") (col alias "host")

let docs_eligible ?(alias = "") () =
  Printf.sprintf "(coalesce(%s, '') = '1' OR %s = 1) AND %s" (col alias "tier")
    (col alias "registered") (not_confidential ~alias ())

let docs_fetchable ?(alias = "") () =
  Printf.sprintf "%s AND %s IN ('github', 'gitlab', 'local') AND %s = 1 AND %s IS NULL"
    (docs_eligible ~alias ()) (col alias "host") (col alias "fetched")
    (col alias "missing_since")

let ineligible_repo_ids =
  Printf.sprintf "repo_id IN (SELECT id FROM repos WHERE NOT (%s))" (docs_eligible ())

let is_confidential ~campaign ~host = campaign || host = "whitehat"
