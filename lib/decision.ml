type t = { readme : bool; detail : bool }

let max_detail_age_days = 30

let decide ~stored_pushed_at ~listed_pushed_at ~readme_stored ~detail_age_days ~full =
  let changed =
    match stored_pushed_at with None -> true | Some s -> Some s <> listed_pushed_at
  in
  let readme = full || changed || not readme_stored in
  let stale =
    match detail_age_days with None -> true | Some d -> d >= max_detail_age_days
  in
  { readme; detail = readme || stale }

let age_days ~now ts =
  match Option.bind ts Status.parse_time with
  | None -> None
  | Some t ->
      let seconds = Ptime.Span.to_float_s (Ptime.diff now t) in
      Some (max 0 (int_of_float (Float.round (Float.trunc (seconds /. 86400.)))))
