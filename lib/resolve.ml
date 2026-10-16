type t = { id : int; key : string }

let row r =
  {
    id = Option.value ~default:0 (Db.to_int_opt r.(0));
    key = Option.value ~default:"" (Db.to_string_opt r.(1));
  }

let lookup db where v =
  Db.query db
    ("SELECT id, key FROM repos WHERE " ^ where ^ " ORDER BY key")
    [ Db.text v ]
    row

let resolve db ident =
  let ident = String.trim ident in
  if ident = "" then Error "repo must be a non-empty string"
  else
    let lower = String.lowercase_ascii ident in
    let steps =
      [
        ("key = ?", true);
        ("lower(full_name) = ?", String.contains ident '/');
        ("lower(name) = ?", true);
      ]
    in
    let rec go = function
      | [] -> Error (Printf.sprintf "unknown repo '%s'" ident)
      | (_, false) :: rest -> go rest
      | (where, true) :: rest -> (
          match lookup db where lower with
          | [] -> go rest
          | [ r ] -> Ok r
          | many ->
              Error
                (Printf.sprintf "ambiguous repo '%s': matches %s; use a full key" ident
                   (String.concat ", " (List.map (fun r -> r.key) many))))
    in
    go steps
