let ( let* ) = Result.bind

type detail = Brief | Standard | Full

let details = [ "brief"; "standard"; "full" ]

let detail o =
  let* v = Args.opt_string o "detail" in
  match v with
  | None -> Ok Standard
  | Some s -> (
      match Vocab.canonical details s with
      | Some "brief" -> Ok Brief
      | Some "full" -> Ok Full
      | Some _ -> Ok Standard
      | None -> Error (Vocab.invalid_value ~what:"detail" s details))

let min_budget = 100
let max_budget = 1_000_000

let budget o =
  let* v = Args.opt_int o "budget_chars" in
  match v with
  | Some n when n < min_budget || n > max_budget ->
      Error
        (Printf.sprintf "argument 'budget_chars' must be an integer between %d and %d"
           min_budget max_budget)
  | v -> Ok v

let kind o =
  let* v = Args.opt_string o "kind" in
  match v with
  | None -> Ok None
  | Some s -> (
      match Vocab.canonical Doc_select.kinds s with
      | Some k -> Ok (Some k)
      | None -> Error (Vocab.invalid_value ~what:"kind" s Doc_select.kinds))

let size j = Utf8.length (Yojson.Safe.to_string j)
let set_list list_key items j = Jsonx.set list_key (`List items) j
let rec take n = function x :: rest when n > 0 -> x :: take (n - 1) rest | _ -> []

(* Cut the long text field of one item by [excess] code points (plus escaping slack). *)
let shorten excess item =
  let cut k j =
    match Jsonx.str k j with
    | Some s ->
        let keep = max 0 (Utf8.length s - excess - 8) in
        Some (Jsonx.set k (`String (Utf8.take s keep ^ "…")) j)
    | None -> None
  in
  match cut "text" item with Some j -> Some j | None -> cut "snippet" item

let recount list_key j =
  match (Jsonx.member "count" j, Jsonx.get list_key j) with
  | Some _, `List l -> Jsonx.set "count" (`Int (List.length l)) j
  | _ -> j

let rec fit budget list_key items j =
  let candidate = recount list_key (set_list list_key items j) in
  let over = size candidate - budget in
  if over <= 0 then candidate
  else
    match items with
    | [] -> candidate
    | [ item ] -> (
        match shorten over item with
        | Some item' when item' <> item && size item' < size item ->
            fit budget list_key [ item' ] j
        | _ -> fit budget list_key [] j)
    | _ -> fit budget list_key (take (List.length items - 1) items) j

let apply_budget budget ~list_key json =
  match budget with
  | None -> json
  | Some b ->
      let items = match Jsonx.get list_key json with `List l -> l | _ -> [] in
      let flagged = Jsonx.set "truncated" (`Bool true) json in
      let fitted = fit b list_key items flagged in
      if Jsonx.get list_key fitted = `List items then
        Jsonx.set "truncated" (`Bool false) json
      else fitted
