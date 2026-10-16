type t = {
  host : string option;
  owner : string option;
  language : string option;
  tier : string option;
  status : string option;
  domain : string option;
  private_ : bool option;
  maintained_only : bool option;
  has_docs : bool option;
  ownership : string option;
}

let keys =
  [
    "host";
    "owner";
    "language";
    "tier";
    "status";
    "domain";
    "private";
    "maintained_only";
    "has_docs";
    "ownership";
  ]

let empty =
  {
    host = None;
    owner = None;
    language = None;
    tier = None;
    status = None;
    domain = None;
    private_ = None;
    maintained_only = None;
    has_docs = None;
    ownership = None;
  }

let ( let* ) = Result.bind

let raw_string k = function
  | `String s when String.trim s <> "" -> Ok (String.trim s)
  | `Int n when k = "tier" -> Ok (string_of_int n)
  | _ -> Error (Printf.sprintf "filter '%s' must be a non-empty string" k)

let closed k allowed v =
  let* s = raw_string k v in
  match Vocab.canonical allowed s with
  | Some c -> Ok c
  | None -> Error (Vocab.invalid_value ~what:k s allowed)

let to_bool k = function
  | `Bool b -> Ok b
  | `String s when String.lowercase_ascii s = "true" -> Ok true
  | `String s when String.lowercase_ascii s = "false" -> Ok false
  | _ -> Error (Printf.sprintf "filter '%s' must be a boolean" k)

let set ~domains f (k, v) =
  match (k, v) with
  | _, `Null -> Ok f
  | "host", v -> Result.map (fun x -> { f with host = Some x }) (closed k Vocab.hosts v)
  | "tier", v -> Result.map (fun x -> { f with tier = Some x }) (closed k Vocab.tiers v)
  | "status", v ->
      Result.map (fun x -> { f with status = Some x }) (closed k Vocab.statuses v)
  | "domain", v -> Result.map (fun x -> { f with domain = Some x }) (closed k domains v)
  | "owner", v -> Result.map (fun x -> { f with owner = Some x }) (raw_string k v)
  | "language", v -> Result.map (fun x -> { f with language = Some x }) (raw_string k v)
  | "private", v -> Result.map (fun x -> { f with private_ = Some x }) (to_bool k v)
  | "maintained_only", v ->
      Result.map (fun x -> { f with maintained_only = Some x }) (to_bool k v)
  | "has_docs", v -> Result.map (fun x -> { f with has_docs = Some x }) (to_bool k v)
  | "ownership", v ->
      Result.map (fun x -> { f with ownership = Some x }) (closed k Vocab.ownerships v)
  | k, _ -> Error (Printf.sprintf "unknown filter key '%s'" k)

let of_assoc ?(domains = Vocab.default_domains) ?(extra_keys = []) members =
  match List.find_opt (fun (k, _) -> not (List.mem k keys)) members with
  | Some (k, _) ->
      Error
        (Printf.sprintf "unknown filter key '%s'; allowed keys: %s" k
           (String.concat ", " (keys @ extra_keys)))
  | None ->
      List.fold_left
        (fun acc m -> Result.bind acc (fun f -> set ~domains f m))
        (Ok empty) members

let where ~alias f =
  let col c = alias ^ "." ^ c in
  let conds =
    List.filter_map Fun.id
      [
        Option.map (fun v -> (col "host" ^ " = ?", Db.text v)) f.host;
        Option.map (fun v -> ("lower(" ^ col "owner" ^ ") = lower(?)", Db.text v)) f.owner;
        Option.map
          (fun v -> ("lower(" ^ col "language" ^ ") = lower(?)", Db.text v))
          f.language;
        Option.map (fun v -> (col "tier" ^ " = ?", Db.text v)) f.tier;
        Option.map (fun v -> (col "ownership" ^ " = ?", Db.text v)) f.ownership;
        Option.map (fun v -> (col "effective_domain" ^ " = ?", Db.text v)) f.domain;
        Option.map
          (fun b -> ("coalesce(" ^ col "private" ^ ", 0) = ?", Db.int (Bool.to_int b)))
          f.private_;
        Option.map
          (fun b ->
            ( (if b then "" else "NOT ")
              ^ "EXISTS (SELECT 1 FROM documents d WHERE d.repo_id = " ^ col "id" ^ ")",
              Sqlite3.Data.NULL ))
          f.has_docs;
      ]
  in
  ( String.concat "" (List.map (fun (c, _) -> " AND " ^ c) conds),
    List.filter_map (fun (c, p) -> if String.contains c '?' then Some p else None) conds
  )

let keep_status f s =
  match f.status with None -> true | Some want -> String.equal want s

let maintained_active f = f.maintained_only = Some true

let keep_maintained f ~archived ~maintained_copy =
  (not (maintained_active f)) || ((not archived) && maintained_copy = None)
