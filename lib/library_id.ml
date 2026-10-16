let fallback_service = "context7"

let index_label ~display_name =
  match String.split_on_char ' ' (String.trim display_name) with
  | first :: _ when first <> "" -> first ^ "'s index"
  | _ -> "the library index"

let no_match_hint ?(display_name = "") name =
  Printf.sprintf "not in %s — use %s resolve-library-id with libraryName=%s"
    (index_label ~display_name) fallback_service name

let max_candidates = 10
let name_pool = 25
let weights () = String.concat ", " (List.map (Printf.sprintf "%.1f") Search.weights)

(* Exact: the repository name, owner/name or key, or one of its package aliases (the
   names its manifests declare, e.g. an opam package named differently from the repo). *)
let exact_ids db name =
  Db.query db
    "SELECT id FROM repos WHERE host <> 'whitehat' AND (lower(name) = ? OR \
     lower(full_name) = ? OR key = ? OR EXISTS (SELECT 1 FROM json_each(CASE WHEN \
     json_valid(repos.aliases) THEN repos.aliases ELSE '[]' END) WHERE lower(value) = \
     ?))"
    [ Db.text name; Db.text name; Db.text name; Db.text name ]
    (fun r -> Option.value (Db.to_int_opt r.(0)) ~default:0)

(* Rows whose [owner/name] contains every token of the library name (substring,
   case-insensitive): a name lookup, never a description match. *)
let name_matches db tokens =
  match tokens with
  | [] -> []
  | _ ->
      let conds =
        String.concat " AND "
          (List.map (fun _ -> "instr(lower(full_name), ?) > 0") tokens)
      in
      Db.query db
        (Printf.sprintf
           "SELECT id FROM repos WHERE host <> 'whitehat' AND %s ORDER BY length(name), \
            key LIMIT ?"
           conds)
        (List.map (fun t -> Db.text (String.lowercase_ascii t)) tokens
        @ [ Db.int name_pool ])
        (fun r -> Option.value (Db.to_int_opt r.(0)) ~default:0)

let relevance db tokens =
  let tbl = Hashtbl.create 64 in
  (match tokens with
  | [] -> ()
  | _ ->
      Db.query db
        (Printf.sprintf
           "SELECT rowid, -bm25(repos_fts, %s) FROM repos_fts WHERE repos_fts MATCH ?"
           (weights ()))
        [ Db.text (Query.or_query tokens) ]
        (fun r ->
          match (Db.to_int_opt r.(0), r.(1)) with
          | Some id, Sqlite3.Data.FLOAT f -> Hashtbl.replace tbl id f
          | _ -> ())
      |> ignore);
  tbl

type cand = {
  id : int;
  key : string;
  host : string;
  name : string;
  json_of : maintained:string option -> doc_count:int -> Opts.detail -> Yojson.Safe.t;
  score : float;
  maintained : string option;
}

let non_empty v = match v with Some s when String.trim s <> "" -> Some s | _ -> None
let opt v = Jsonx.of_string_opt v
let round3 x = Float.round (x *. 1000.) /. 1000.

let candidate (ctx : Ctx.t) ~exact ~rel ~filters r =
  let s i = Db.to_string_opt r.(i) in
  let str i = Option.value (s i) ~default:"" in
  let id = Option.value (Db.to_int_opt r.(0)) ~default:0 in
  let archived = Db.to_bool r.(10) in
  let status =
    Status.effective ~now:(ctx.now ()) ~override:(s 11) ~archived ~last_commit:(s 12)
      ~pushed_at:(s 13)
  in
  let score =
    round3
      ((if List.mem id exact then 100. else 0.)
      +. Option.value (Hashtbl.find_opt rel id) ~default:0.)
  in
  let uri = Liburi.repo ~host:(str 2) ~owner:(str 3) ~name:(str 4) in
  let what = List.find_map non_empty [ s 6; s 7; s 8 ] in
  let json_of ~maintained ~doc_count (detail : Opts.detail) =
    let base =
      [ ("id", `String (str 1)); ("name", `String (str 4)) ]
      @ (if detail = Brief then []
         else [ ("full_name", `String (str 5)); ("what", opt what) ])
      @ [ ("maintained_copy", opt maintained) ]
      @ (if detail = Brief then []
         else [ ("score", `Float score); ("doc_count", `Int doc_count) ])
      @ [ ("uri", opt uri) ]
    in
    let full =
      [
        ("host", `String (str 2));
        ("tier", opt (s 9));
        ("status", `String status);
        ("language", opt (s 14));
        ("description", opt (s 8));
        ("pushed_at", opt (s 13));
      ]
    in
    `Assoc (if detail = Full then base @ full else base)
  in
  if Filters.keep_status filters status then
    Some
      ( archived,
        { id; key = str 1; host = str 2; name = str 4; json_of; score; maintained = None }
      )
  else None

let rows (ctx : Ctx.t) ids filters =
  let filter_sql, params = Filters.where ~alias:"r" filters in
  let marks = String.concat ", " (List.map (fun _ -> "?") ids) in
  Db.query ctx.db
    (Printf.sprintf
       "SELECT r.id, r.key, r.host, r.owner, r.name, r.full_name, r.what, r.synopsis, \
        r.description, r.tier, r.archived, r.status_override, r.last_commit, \
        r.pushed_at, r.language FROM repos r WHERE r.id IN (%s)%s"
       marks filter_sql)
    (List.map Db.int ids @ params)
    (fun r -> r)

(* Groups of one name ranked by their best score; inside a group the maintained copy,
   then GitHub, GitLab, local, then score and key. *)
let order cands =
  let lower = String.lowercase_ascii in
  let best = Hashtbl.create 16 in
  List.iter
    (fun c ->
      let k = lower c.name in
      let b = Option.value (Hashtbl.find_opt best k) ~default:neg_infinity in
      Hashtbl.replace best k (Float.max b c.score))
    cands;
  let group c = Hashtbl.find best (lower c.name) in
  let cmp a b =
    let by f g x y = match f x y with 0 -> g x y | c -> c in
    (by (fun a b -> compare (group b) (group a))
    @@ by (fun a b -> compare (lower a.name) (lower b.name))
    @@ by (fun a b -> compare (a.maintained <> None) (b.maintained <> None))
    @@ by (fun a b -> compare (Copies.host_rank a.host) (Copies.host_rank b.host))
    @@ by (fun a b -> compare b.score a.score)
    @@ fun a b -> compare a.key b.key)
      a b
  in
  List.sort cmp cands

let rec take n = function x :: rest when n > 0 -> x :: take (n - 1) rest | _ -> []

let resolve (ctx : Ctx.t) ~library_name ~query ~filters ~detail =
  let name = String.lowercase_ascii (String.trim library_name) in
  let name =
    if String.length name > 1 && name.[0] = '/' then
      String.sub name 1 (String.length name - 1)
    else name
  in
  if name = "" then Error "libraryName must be a non-empty string"
  else
    let name_tokens = Query.tokens name in
    let exact = exact_ids ctx.db name in
    let ids = List.sort_uniq compare (exact @ name_matches ctx.db name_tokens) in
    let rel =
      relevance ctx.db (name_tokens @ Query.tokens (Option.value query ~default:""))
    in
    let copies = Copies.load ctx.db in
    let cands =
      if ids = [] then []
      else
        rows ctx ids filters
        |> List.filter_map (candidate ctx ~exact ~rel ~filters)
        |> List.filter_map (fun (archived, c) ->
            let maintained = Copies.maintained_copy copies c.key in
            if Filters.keep_maintained filters ~archived ~maintained_copy:maintained then
              Some { c with maintained }
            else None)
    in
    let counts = Docs_query.counts ctx.db in
    let ordered =
      List.map
        (fun c ->
          c.json_of ~maintained:c.maintained
            ~doc_count:(Option.value (Hashtbl.find_opt counts c.id) ~default:0)
            detail)
        (take max_candidates (order cands))
    in
    Ok
      (`Assoc
         (("candidates", `List ordered)
         ::
         (if ordered = [] then
            [
              ( "hint",
                `String
                  (no_match_hint ~display_name:ctx.config.owner.display_name library_name)
              );
            ]
          else [])))
