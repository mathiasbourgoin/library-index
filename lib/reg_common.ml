let ( let* ) = Result.bind

type outcome = { json : Yojson.Safe.t; gaps : string list; complete : bool }

let closed what names = function
  | None -> Ok None
  | Some s -> (
      match List.assoc_opt (String.lowercase_ascii (String.trim s)) names with
      | Some v -> Ok (Some v)
      | None -> Error (Vocab.invalid_value ~what s (List.map fst names)))

let parse_kind s =
  match List.assoc_opt (String.lowercase_ascii (String.trim s)) Config.kind_names with
  | Some k -> Ok k
  | None -> Error (Vocab.invalid_value ~what:"kind" s (List.map fst Config.kind_names))

let config_path (ctx : Ctx.t) = Filename.concat ctx.data_dir Config.file_name

let edit_config (ctx : Ctx.t) table ~pred ~block =
  Config_edit.with_lock ctx.data_dir (fun () ->
      let path = config_path ctx in
      let* text = Config_edit.read path in
      let* text', outcome = Config_edit.upsert text table ~pred ~block in
      let* () = Config_edit.write_atomic path text' in
      Ok outcome)

let created_name = function
  | Config_edit.Created -> "created"
  | Config_edit.Updated -> "updated"

let home () = Result.to_option (Ctx.home ())
let local_roots = Local_keys.local_roots
let configured_checkouts = Local_keys.configured_checkouts

let with_field k v = function
  | `Assoc m -> `Assoc (List.filter (fun (k', _) -> k' <> k) m @ [ (k, v) ])
  | j -> j

let source_json (s : Config.source) =
  Redact.json
    (`Assoc
       [
         ("kind", `String (Config.kind_name s.kind));
         ("name", `String s.name);
         ("host", Jsonx.of_string_opt s.host);
         ("own", `Bool s.own);
         ("probe", `Bool s.probe);
         ("include_subgroups", `Bool s.include_subgroups);
         ("confidential", `String (Config.confidential_name s.confidential));
         ("campaign_prefix", `Bool s.campaign_prefix);
       ])
