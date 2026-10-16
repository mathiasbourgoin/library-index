let ( let* ) = Result.bind
let sprintf = Printf.sprintf

let strings = function
  | Some s -> (
      match Jsonx.parse s with
      | Some (`List l) -> List.filter_map (function `String x -> Some x | _ -> None) l
      | _ -> [])
  | None -> []

(* Deleting a row cascades to its documents and chunks (with their FTS entries), usage,
   field provenance and analysis records: no tombstone is kept. *)
let delete_row db id = Db.run db "DELETE FROM repos WHERE id = ?" [ Db.int id ]

(* A curated column without an [auto] provenance row counts as curated (Field_meta). *)
let curated_column c =
  sprintf
    "(r.%s IS NOT NULL AND r.%s <> '' AND NOT EXISTS (SELECT 1 FROM field_meta f WHERE \
     f.repo_id = r.id AND f.field = '%s' AND f.provenance = 'auto'))"
    c c c

(* The row holds something a person wrote: usage, a curated provenance, a curated column
   value that no pre-fill produced, or extra curated.json members ([extra]). Such a row
   is never deleted by an unregistration (FR-105, US-1 no loss). *)
let carries_curated db id =
  let sql =
    sprintf
      "SELECT EXISTS (SELECT 1 FROM usage WHERE repo_id = ?1) OR EXISTS (SELECT 1 FROM \
       field_meta WHERE repo_id = ?1 AND provenance = 'curated') OR EXISTS (SELECT 1 \
       FROM repos r WHERE r.id = ?1 AND ((r.extra IS NOT NULL AND trim(r.extra) NOT IN \
       ('', '{}', 'null')) OR %s))"
      (String.concat " OR " (List.map curated_column Curate.annotatable))
  in
  Db.query_one db sql [ Db.int id ] (fun r -> Db.to_bool r.(0)) = Some true

let export (ctx : Ctx.t) =
  match Export.after_write ctx ~whitehat_changed:false with
  | Ok () -> `Bool true
  | Error _ -> `Bool false

(* Removes one entry and applies [db_change] in one transaction: the new sources.toml is
   written as the transaction's last step, so a failed write rolls the DB back; a failed
   commit after the write restores the previous file. [db_change] receives the identity
   of the edited configuration. *)
let remove_consistently (ctx : Ctx.t) table ~pred ~db_change ~absent =
  Config_edit.with_lock ctx.data_dir (fun () ->
      let path = Reg_common.config_path ctx in
      let* text = Config_edit.read path in
      let* edited = Config_edit.remove text table ~pred in
      match edited with
      | None -> Error absent
      | Some text' -> (
          let* config = Config.of_string text' in
          let id = Identity.of_config config in
          let written = ref false in
          let restore () = if !written then ignore (Config_edit.write_atomic path text) in
          match
            Db.with_txn ctx.db (fun () ->
                let* v = db_change id in
                let* () = Config_edit.write_atomic path text' in
                written := true;
                Ok v)
          with
          | Ok _ as ok -> ok
          | Error _ as e ->
              restore ();
              e
          | exception ex ->
              restore ();
              raise ex))

(* ------------------------------------------------------------ unregister-repo *)

(* The key a target denotes: a forge URL or key without I/O, else the key of the
   registered entry whose target is that directory, else the checkout's own key. *)
let resolve_key (ctx : Ctx.t) target =
  match Target.lookup_key target with
  | Some k -> Ok k
  | None -> (
      let abs = try Unix.realpath target with Unix.Unix_error _ -> target in
      let by_target =
        List.find_map
          (fun (r : Config.repo) ->
            if r.target = target || r.target = abs then
              match r.key with
              | Some k -> Some (Key.normalize k)
              | None -> Target.lookup_key r.target
            else None)
          ctx.config.repos
      in
      match by_target with
      | Some k -> Ok k
      | None ->
          let call =
            Repo_refresh.call (Repo_refresh.default_env ()) (Repo_refresh.limiter ())
          in
          Result.map
            (fun (t : Target.t) -> t.key)
            (Target.parse ~id:ctx.identity ~call
               ~home:(Result.to_option (Ctx.home ()))
               target))

let registered_as key (r : Config.repo) =
  (match r.key with Some k -> Key.normalize k = key | None -> false)
  || Target.lookup_key r.target = Some key

let row_of db key =
  Db.query_one db "SELECT id, sources, created_by_registration FROM repos WHERE key = ?"
    [ Db.text key ]
    (fun r ->
      ( Option.value (Db.to_int_opt r.(0)) ~default:0,
        strings (Db.to_string_opt r.(1)),
        Db.to_bool r.(2) ))

(* Deleted only when this registration created the row, no source lists it and nobody
   curated it; otherwise only the registration (and its overrides) goes. *)
let drop_row db key =
  match row_of db key with
  | None -> "absent"
  | Some (id, [], true) when not (carries_curated db id) ->
      delete_row db id;
      "deleted"
  | Some _ -> "kept"

let unregister_locked (ctx : Ctx.t) target =
  let* ctx = Ctx.reload ctx in
  let* key = resolve_key ctx target in
  let* row =
    remove_consistently ctx Repo
      ~pred:(function `Repo r -> registered_as key r | `Source _ -> false)
      ~absent:
        (sprintf "'%s' is not registered (see library-index sources list)"
           (Redact.text target))
      ~db_change:(fun id ->
        let row = drop_row ctx.db key in
        Registry.sync ctx.db id;
        Ok row)
  in
  Ok (`Assoc [ ("key", `String key); ("row", `String row); ("exported", export ctx) ])

let unregister_repo (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "target" ] args in
      let* target = Args.req_string o "target" in
      Refresh.with_lock ctx.export_dir (fun () -> unregister_locked ctx target))

(* ------------------------------------------------------------ unregister-source *)

let source_rows db =
  Db.query db
    "SELECT id, sources, registered FROM repos WHERE host <> 'whitehat' AND sources IS \
     NOT NULL"
    [] (fun r ->
      ( Option.value (Db.to_int_opt r.(0)) ~default:0,
        strings (Db.to_string_opt r.(1)),
        Db.to_bool r.(2) ))

let set_sources db id rest =
  Db.run db "UPDATE repos SET sources = ? WHERE id = ?"
    [
      Db.text (Yojson.Safe.to_string (`List (List.map (fun l -> `String l) rest)));
      Db.int id;
    ]

(* Rows discovered only through the removed source go, unless registered or curated; the
   others lose its labels and keep their row and curated data. *)
let prune db labels =
  List.fold_left
    (fun (removed, kept) (id, srcs, registered) ->
      if not (List.exists (fun l -> List.mem l labels) srcs) then (removed, kept)
      else
        let rest = List.filter (fun l -> not (List.mem l labels)) srcs in
        if rest = [] && (not registered) && not (carries_curated db id) then (
          delete_row db id;
          (removed + 1, kept))
        else (
          set_sources db id rest;
          (removed, kept + 1)))
    (0, 0) (source_rows db)

(* The provenance labels only [src] writes: a label another remaining source still writes
   is not this source's alone. *)
let own_labels ~remaining src =
  let still = List.concat_map Sources.labels remaining in
  List.filter (fun l -> not (List.mem l still)) (Sources.labels src)

let source_locked (ctx : Ctx.t) ~kind ~name =
  let* ctx = Ctx.reload ctx in
  let matches (s : Config.source) =
    s.kind = kind && String.lowercase_ascii s.name = String.lowercase_ascii name
  in
  let absent =
    sprintf "no %s source named '%s' (see library-index sources list)"
      (Config.kind_name kind) (Redact.text name)
  in
  match List.find_opt matches ctx.config.sources with
  | None -> Error absent
  | Some src ->
      let remaining = List.filter (fun s -> not (matches s)) ctx.config.sources in
      let* removed, kept =
        remove_consistently ctx Source
          ~pred:(function `Source s -> matches s | `Repo _ -> false)
          ~absent
          ~db_change:(fun id ->
            let r = prune ctx.db (own_labels ~remaining src) in
            Registry.sync ctx.db id;
            Ok r)
      in
      Ok
        (`Assoc
           [
             ("source", Reg_common.source_json src);
             ("repos_removed", `Int removed);
             ("repos_kept", `Int kept);
             ("exported", export ctx);
           ])

let unregister_source (ctx : Ctx.t) args =
  Errors.guard (fun () ->
      let* o = Args.object_ ~allowed:[ "kind"; "name" ] args in
      let* kind_s = Args.req_string o "kind" in
      let* kind = Reg_common.parse_kind kind_s in
      let* name = Args.opt_string o "name" in
      let name = Option.value name ~default:"" in
      Refresh.with_lock ctx.export_dir (fun () -> source_locked ctx ~kind ~name))
