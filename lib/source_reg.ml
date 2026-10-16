let ( let* ) = Result.bind
let sprintf = Printf.sprintf

type outcome = Reg_common.outcome

(* ------------------------------------------------------------ arguments *)

(* Every flag is tri-state: [None] keeps the entry's current value (FR-104 upsert). *)
type args = {
  kind : Config.kind;
  name : string;
  host : string option;
  own : bool option;
  probe : bool option;
  include_subgroups : bool option;
  confidential : Config.confidential option;
  campaign_prefix : bool option;
}

let keys =
  [
    "kind";
    "name";
    "host";
    "own";
    "probe";
    "include_subgroups";
    "confidential";
    "campaign_prefix";
  ]

let parse args =
  let* o = Args.object_ ~allowed:keys args in
  let* kind_s = Args.req_string o "kind" in
  let* kind = Reg_common.parse_kind kind_s in
  let* name = Args.opt_string o "name" in
  let* host = Args.opt_string o "host" in
  let* own = Args.opt_bool o "own" in
  let* probe = Args.opt_bool o "probe" in
  let* include_subgroups = Args.opt_bool o "include_subgroups" in
  let* campaign_prefix = Args.opt_bool o "campaign_prefix" in
  let* confidential = Args.opt_string o "confidential" in
  let* confidential =
    Reg_common.closed "confidential" Config.confidential_names confidential
  in
  (* An owner, group or host name never holds '?', '#', '@' or white space; a value that
     does may carry a credential (a query token, userinfo) and is refused. *)
  let suspicious s =
    Redact.contains_secret s
    || String.exists (fun c -> c = '?' || c = '#' || c = '@' || c = ' ' || c < ' ') s
  in
  let checked =
    Option.to_list host @ if kind = Config.Local_root then [] else Option.to_list name
  in
  let* () =
    if
      List.exists suspicious checked
      || Option.fold ~none:false ~some:Redact.contains_secret name
    then Error "a source name or host carrying a credential is refused"
    else Ok ()
  in
  Ok
    {
      kind;
      name = Option.value name ~default:"";
      host;
      own;
      probe;
      include_subgroups;
      confidential;
      campaign_prefix;
    }

let same_source (a : Config.source) (b : Config.source) =
  a.kind = b.kind && String.lowercase_ascii a.name = String.lowercase_ascii b.name

let merged ~(existing : Config.source option) (a : args) : Config.source =
  let base : Config.source =
    match existing with
    | Some s -> s
    | None ->
        {
          kind = a.kind;
          name = a.name;
          host = None;
          own = false;
          probe = false;
          include_subgroups = true;
          confidential = Not_confidential;
          campaign_prefix = false;
        }
  in
  let keep v d = Option.value v ~default:d in
  {
    base with
    host = (match a.host with Some h -> Some h | None -> base.host);
    own = keep a.own base.own;
    probe = keep a.probe base.probe;
    include_subgroups = keep a.include_subgroups base.include_subgroups;
    confidential = keep a.confidential base.confidential;
    campaign_prefix = keep a.campaign_prefix base.campaign_prefix;
  }

(* A local root must be an existing directory before anything is registered. *)
let dev_root (src : Config.source) =
  match src.kind with
  | Local_root ->
      let dir =
        match Reg_common.home () with
        | Some h -> Config.expand_home ~home:h src.name
        | None -> src.name
      in
      if String.trim src.name <> "" && Sys.file_exists dir && Sys.is_directory dir then
        Ok (Some dir)
      else
        Error
          (sprintf "local-root '%s' is not an existing directory (nothing registered)"
             src.name)
  | _ -> Ok None

(* ------------------------------------------------------------ ingestion *)

let union a b = a @ List.filter (fun x -> not (List.mem x a)) b

let strings = function
  | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
  | _ -> []

(* A row listed by this source keeps the labels other sources gave it. *)
let with_stored_sources (stored : Repo_refresh.stored) row =
  match stored.row with
  | None -> row
  | Some s ->
      let merged =
        union (strings (Jsonx.get "sources" s)) (strings (Jsonx.get "sources" row))
      in
      Jsonx.set "sources" (`List (List.map (fun x -> `String x) merged)) row

(* Every listed row gets the local facts and the checkout-remote campaign rule of a full
   refresh ({!Local_scan.enrich} over the checkouts of every configured local root, with
   the whole configuration's identity): a forge repository whose checkout has a campaign
   remote is a campaign repository whatever source lists it (AM-17). *)
let enrich_local ~call ~local (entries : Sources.entry list) =
  let rows =
    Local_scan.enrich ~call local (List.map (fun (e : Sources.entry) -> e.row) entries)
  in
  List.map2 (fun (e : Sources.entry) row -> { e with row }) entries rows

let was_fetched db key =
  Db.query_one db "SELECT fetched FROM repos WHERE key = ?"
    [ Db.text key ]
    (fun r -> Db.to_bool r.(0))
  = Some true

type tally = {
  mutable added : int;
  mutable updated : int;
  mutable gaps : string list;  (** Reversed. *)
  mutable keys : string list;
}

let record (ctx : Ctx.t) tl ~now ~fetched_before (r : Repo_refresh.result) =
  tl.gaps <- List.rev_append (Repo_refresh.gap_lines r) tl.gaps;
  match Repo_refresh.write ~id:ctx.identity ctx.db ~now ~recompute:true r with
  | Ok () ->
      tl.keys <- r.key :: tl.keys;
      if List.assoc_opt (Some r.key) fetched_before = Some true then
        tl.updated <- tl.updated + 1
      else tl.added <- tl.added + 1
  | Error e -> tl.gaps <- sprintf "%s: write failed (%s)" r.key e :: tl.gaps

(* Every listed row is processed and written with the whole configuration's identity
   ([ctx.identity]): the campaign rule of every source applies (AM-17). *)
let ingest (ctx : Ctx.t) env ~call ~local ~progress (entries : Sources.entry list) =
  let now = ctx.now () in
  let search =
    ( Globals.search_counts ctx.db "gh_search_commits",
      Globals.search_counts ctx.db "gh_search_prs" )
  in
  let tl = { added = 0; updated = 0; gaps = []; keys = [] } in
  let total = float_of_int (max 1 (List.length entries)) in
  let jobs =
    List.map
      (fun (e : Sources.entry) ->
        let key = Option.value (Norm.key e.row) ~default:"" in
        (e, Repo_refresh.load_stored ctx.db key, was_fetched ctx.db key))
      entries
  in
  let job ((e : Sources.entry), stored, _) =
    Repo_refresh.process ~id:ctx.identity ~call ~now ~full:false
      ~search:(Repo_refresh.search_for search e.row)
      ~stored ~local ?readme_url:e.readme_url
      (with_stored_sources stored e.row)
  in
  let done_ = ref 0 in
  let fetched_before =
    List.map (fun ((e : Sources.entry), _, f) -> (Norm.key e.row, f)) jobs
  in
  Pool.run ~workers:env.Repo_refresh.workers jobs job (function
    | Ok r ->
        incr done_;
        progress (1. +. (float_of_int !done_ /. total)) r.key;
        record ctx tl ~now ~fetched_before r
    | Error ex ->
        tl.gaps <- sprintf "a detail fetch failed (%s)" (Errors.classify ex) :: tl.gaps);
  tl

(* ------------------------------------------------------------ result *)

let past = function Some d -> Unix.gettimeofday () >= d | None -> false

let deadline_gap =
  "the registration deadline was reached before every repository was fetched: continue \
   with `library-index refresh`"

let docs (ctx : Ctx.t) env ~call ?deadline keys =
  List.filter_map
    (fun key ->
      if past deadline then None
      else
        Some
          (Doc_refresh.run ctx.db ~call ~now:(ctx.now ()) ~full:false ~workers:1
             ~file_workers:env.Repo_refresh.workers ~key ()))
    (List.sort compare keys)

let result ~(src : Config.source) ~outcome ~listed ~tl ~docs ~gaps : outcome =
  let sum f = List.fold_left (fun n d -> n + f d) 0 docs in
  let gaps = List.map Redact.text gaps in
  {
    json =
      Redact.json
      @@ `Assoc
           [
             ("source", Reg_common.source_json src);
             ("registered", `String (Reg_common.created_name outcome));
             ("complete", `Bool (gaps = []));
             ("listed", `Int listed);
             ("repos_added", `Int tl.added);
             ("repos_updated", `Int tl.updated);
             ( "docs",
               `Assoc
                 [
                   ("written", `Int (sum (fun (d : Doc_refresh.summary) -> d.written)));
                   ("unchanged", `Int (sum (fun (d : Doc_refresh.summary) -> d.unchanged)));
                   ("removed", `Int (sum (fun (d : Doc_refresh.summary) -> d.removed)));
                 ] );
             ("gaps", `List (List.map (fun g -> `String g) gaps));
           ];
    gaps;
    complete = gaps = [];
  }

(* ------------------------------------------------------------ register-source *)

let locked (ctx : Ctx.t) env ~progress ?deadline (a : args) =
  let* ctx = Ctx.reload ctx in
  let existing =
    List.find_opt
      (fun (s : Config.source) ->
        s.kind = a.kind && String.lowercase_ascii s.name = String.lowercase_ascii a.name)
      ctx.config.sources
  in
  let src = merged ~existing a in
  let* dev_root = dev_root src in
  let call = Repo_refresh.call env (Repo_refresh.limiter ?deadline ()) in
  (* Every checkout the run will classify (configured roots, and the new root) must be
     readable before anything is registered or fetched. *)
  let* () =
    Local_keys.check
      (Local_keys.of_checkouts
         (Reg_common.configured_checkouts ctx ~call
         @
         match dev_root with
         | Some d -> Local_scan.scan ~id:ctx.identity ~call d
         | None -> []))
  in
  let* outcome =
    Reg_common.edit_config ctx Source
      ~pred:(function `Source s -> same_source s src | `Repo _ -> false)
      ~block:(Config_edit.render_source src)
  in
  let* ctx = Ctx.reload ctx in
  progress 1. "registered";
  let checkouts = Reg_common.configured_checkouts ctx ~call in
  let local = Local_keys.index ctx checkouts in
  (* The configuration changed: every stored row is decided again now, also the rows this
     source does not list or when its listing fails (INV-C, FR-124: immediate effect). *)
  let* _ = Campaign.reclassify_now ctx ~call ~local:(lazy local) () in
  let id = Identity.for_source ctx.identity src in
  let listing = Sources.collect ~id ~call ~sleep:env.Repo_refresh.sleep ~dev_root in
  let entries = enrich_local ~call ~local:checkouts listing.entries in
  let tl = ingest ctx env ~call ~local ~progress entries in
  let docs = docs ctx env ~call ?deadline tl.keys in
  let doc_gaps = List.concat_map (fun (d : Doc_refresh.summary) -> d.gaps) docs in
  let late = if past deadline then [ deadline_gap ] else [] in
  let gaps = listing.failures @ List.rev tl.gaps @ doc_gaps @ late in
  Ok (result ~src ~outcome ~listed:(List.length listing.entries) ~tl ~docs ~gaps)

let register_source (ctx : Ctx.t) env ?(progress = fun _ _ -> ()) ?deadline args =
  Errors.guard (fun () ->
      let* a = parse args in
      Refresh.with_lock ctx.export_dir (fun () -> locked ctx env ~progress ?deadline a))
