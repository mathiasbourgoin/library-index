let parse_time s =
  let s = String.trim s in
  let s =
    if String.length s = 10 && s.[4] = '-' && s.[7] = '-' then s ^ "T00:00:00Z" else s
  in
  match Ptime.of_rfc3339 s with Ok (t, _, _) -> Some t | Error _ -> None

let age_days ~now t =
  int_of_float (Float.floor (Ptime.Span.to_float_s (Ptime.diff now t) /. 86400.))

let non_empty = function Some s when String.trim s <> "" -> Some s | _ -> None

let classify ~now d =
  match parse_time d with
  | None -> "unknown"
  | Some t ->
      let age = age_days ~now t in
      if age <= 90 then "active" else if age <= 730 then "dormant" else "stale"

let computed ~now ~archived ~last_commit ~pushed_at =
  if archived then "archived"
  else
    match non_empty last_commit with
    | Some d -> classify ~now d
    | None -> (
        match non_empty pushed_at with Some d -> classify ~now d | None -> "unknown")

let effective ~now ~override ~archived ~last_commit ~pushed_at =
  match non_empty override with
  | Some o -> o
  | None -> computed ~now ~archived ~last_commit ~pushed_at
