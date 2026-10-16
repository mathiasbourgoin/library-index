(* The one campaign decision of every write path (INV-D, FR-131), the write-side twin of
   [Eligibility]. Every predicate that makes a row a campaign row is composed here and
   nowhere else ([checks/single-campaign-decision.js]). *)

let ( let* ) = Result.bind
let lower = String.lowercase_ascii

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* ------------------------------------------------------------ rules *)

let owner_rule ~(id : Identity.t) owner =
  let o = lower owner in
  List.exists (fun prefix -> starts_with ~prefix o) id.campaign_prefixes

let key_owner key =
  match String.index_opt key ':' with
  | None -> None
  | Some i -> (
      let path = String.sub key (i + 1) (String.length key - i - 1) in
      match String.rindex_opt path '/' with
      | Some j when j > 0 -> Some (String.sub path 0 j)
      | _ -> None)

let remote_rule ~id keys =
  List.exists
    (fun k -> match key_owner k with Some o -> owner_rule ~id o | None -> false)
    keys

let flag row = Jsonx.truthy (Jsonx.member "campaign" row)
let owner row = Option.value (Jsonx.str "owner" row) ~default:""

let row_rule ~id row =
  flag row
  || owner_rule ~id (owner row)
  ||
  match Jsonx.str "key" row with
  | Some k -> Identity.is_registered_campaign id k
  | None -> false

let checkout_remote_keys row =
  match Jsonx.member "checkout_remote_keys" row with
  | Some (`List l) -> Jsonx.strings l
  | _ -> []

let decide ~id ~call ~local ?stored ?(checkout : Local_scan.checkout option) row =
  let rows = row :: Option.to_list stored in
  let own, unreadable =
    match checkout with
    | Some c -> (Option.to_list c.remote_key @ c.all_remote_keys, c.remotes_unreadable)
    | None -> ([], false)
  in
  List.exists (row_rule ~id) rows
  || unreadable
  || remote_rule ~id
       (own
       @ List.concat_map checkout_remote_keys rows
       @ Local_keys.for_row local ~id ~call rows)
  || Local_keys.forced_row local ~id ~call rows

let of_owner ~id owner = owner_rule ~id owner

(* AM-17: the raw cache predates the flag; a row is campaign when its own rule holds, the
   stored row is campaign (sticky), or a remote of one of its cached checkouts (matched
   through [local_paths]) is under a campaign prefix. *)
let stored_flag db key =
  Db.query_one db "SELECT campaign FROM repos WHERE key = ?"
    [ Db.text (Key.normalize key) ]
    (fun r -> Db.to_bool r.(0))
  = Some true

let of_raw_cache ~id db ~local repos =
  let remotes = Hashtbl.create 64 in
  List.iter
    (fun e ->
      Option.iter
        (fun p ->
          let all =
            match Jsonx.member "all_remote_keys" e with Some (`List l) -> l | _ -> []
          in
          Hashtbl.replace remotes p
            (Option.to_list (Jsonx.str "remote_key" e) @ Jsonx.strings all))
        (Jsonx.str "path" e))
    local;
  List.map
    (fun row ->
      let paths =
        match Jsonx.member "local_paths" row with
        | Some (`List l) -> Jsonx.strings l
        | _ -> []
      in
      let remote_keys =
        List.concat_map
          (fun p -> Option.value (Hashtbl.find_opt remotes p) ~default:[])
          paths
      in
      let sticky =
        match Jsonx.str "key" row with Some k -> stored_flag db k | None -> false
      in
      let campaign = sticky || row_rule ~id row || remote_rule ~id remote_keys in
      Jsonx.set "campaign" (`Bool campaign) row)
    repos

let persisted db row =
  flag row || match Jsonx.str "key" row with Some k -> stored_flag db k | None -> false

(* ------------------------------------------------------------ gaps *)

let is_word_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

(* Every whole-token occurrence of [needle] in [s] (ASCII case-insensitive) replaced. *)
let replace_token ~needle ~by s =
  let n = String.length needle in
  if n = 0 then s
  else
    let ls = lower s and ln = lower needle in
    let b = Buffer.create (String.length s) in
    let rec go i =
      if i > String.length s - n then
        Buffer.add_string b (String.sub s i (String.length s - i))
      else if
        String.sub ls i n = ln
        && (i = 0 || not (is_word_char s.[i - 1]))
        && (i + n = String.length s || not (is_word_char s.[i + n]))
      then (
        Buffer.add_string b by;
        go (i + n))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
    in
    go 0;
    Buffer.contents b

let masked = "a campaign repository"

(* The names a gap line may use for a row: key, full name, name (longest first). *)
let campaign_names db =
  Db.query db
    "SELECT key, full_name, name FROM repos WHERE campaign = 1 AND host <> 'whitehat'" []
    (fun r -> List.filter_map Db.to_string_opt (Array.to_list r))
  |> List.concat
  |> List.filter (fun s -> String.trim s <> "")
  |> List.sort_uniq (fun a b -> compare (String.length b, b) (String.length a, a))

let mask_with names line =
  List.fold_left (fun l needle -> replace_token ~needle ~by:masked l) line names

let mask_gaps db lines =
  let names = campaign_names db in
  if names = [] then lines else List.map (mask_with names) lines

let remask_stored_gaps db =
  let names = campaign_names db in
  if names <> [] then
    List.iter
      (fun (ord, text) ->
        let m = mask_with names text in
        if m <> text then
          Db.run db "UPDATE gaps SET text = ? WHERE ord = ?" [ Db.text m; ord ])
      (Db.query db "SELECT ord, text FROM gaps" [] (fun r ->
           (r.(0), Option.value (Db.to_string_opt r.(1)) ~default:"")))

let gap_name db ~id key =
  if stored_flag db key || Identity.is_registered_campaign id key then masked else key

(* ------------------------------------------------------------ marking *)

let auto_cleared field =
  Printf.sprintf
    "%s = CASE WHEN EXISTS (SELECT 1 FROM field_meta f WHERE f.repo_id = repos.id AND \
     f.field = '%s' AND f.provenance = 'auto') THEN NULL ELSE %s END"
    field field field

let clear_auto_prefill db key =
  Db.run db
    (Printf.sprintf "UPDATE repos SET %s, %s WHERE key = ?" (auto_cleared "what")
       (auto_cleared "interest"))
    [ Db.text (Key.normalize key) ]

let mark db (id : Identity.t) key =
  let key = Key.normalize key in
  match
    Db.query_one db "SELECT name, description, synopsis FROM repos WHERE key = ?"
      [ Db.text key ]
      (fun r -> (Db.to_string_opt r.(0), Db.to_string_opt r.(1), Db.to_string_opt r.(2)))
  with
  | None -> ()
  | Some (name, description, synopsis) ->
      (* No README-derived text is kept for any campaign row, exempt names included. *)
      let description, _ =
        Classify.campaign_redaction ~id ~campaign:true
          ~name:(Option.value name ~default:"")
          ~description ~synopsis
      in
      Db.run db
        "UPDATE repos SET campaign = 1, readme = NULL, readme_sha256 = NULL, synopsis = \
         NULL, description = ? WHERE key = ?"
        [ Db.rtext_opt description; Db.text key ];
      clear_auto_prefill db key;
      ignore (Doc_store.purge_ineligible db);
      ignore (Api_store.purge_ineligible db);
      remask_stored_gaps db

let stored_campaign_row db key =
  clear_auto_prefill db key;
  remask_stored_gaps db

let mark_atomic db id key = Db.with_txn db (fun () -> Ok (mark db id key))

(* ------------------------------------------------------------ reclassification *)

let stored_row db key =
  Db.query_one db
    "SELECT key, host, owner, name, full_name, local_paths, campaign FROM repos WHERE \
     key = ?"
    [ Db.text (Key.normalize key) ]
    (fun r ->
      let s i = match Db.to_string_opt r.(i) with Some v -> `String v | None -> `Null in
      `Assoc
        [
          ("key", s 0);
          ("host", s 1);
          ("owner", s 2);
          ("name", s 3);
          ("full_name", s 4);
          ( "local_paths",
            match Option.bind (Db.to_string_opt r.(5)) Jsonx.parse with
            | Some j -> j
            | None -> `Null );
          ("campaign", `Bool (Db.to_bool r.(6)));
        ])

let reclassify db ~id ~call ~local =
  let keys =
    Db.query db "SELECT key FROM repos WHERE campaign = 0 AND host <> 'whitehat'" []
      (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")
  in
  List.filter
    (fun key ->
      match stored_row db key with
      | Some row when decide ~id ~call ~local row ->
          mark db id key;
          true
      | _ -> false)
    keys

let local_of (ctx : Ctx.t) ~call = function
  | Some l -> Lazy.force l
  | None -> Local_keys.configured ctx ~call

let reclassify_now (ctx : Ctx.t) ~call ?local () =
  let local = local_of ctx ~call local in
  let* () = Local_keys.check local in
  Db.with_txn ctx.db (fun () -> Ok (reclassify ctx.db ~id:ctx.identity ~call ~local))

let apply_now (ctx : Ctx.t) ~call ?local key =
  let local = local_of ctx ~call local in
  let* () = Local_keys.check local in
  match stored_row ctx.db key with
  | None -> Ok false
  | Some row ->
      if decide ~id:ctx.identity ~call ~local row then
        let* () = mark_atomic ctx.db ctx.identity key in
        Ok true
      else Ok false
