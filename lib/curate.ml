let ( let* ) = Result.bind

let annotatable =
  [
    "what";
    "interest";
    "domain";
    "consumers";
    "status_override";
    "maintained_side";
    "notes";
    "rules";
  ]

let whitehat_annotatable = [ "what"; "interest" ]
let max_chars = 4000

type value = Text of string | Strings of string list | Clear

let check_len field s =
  if Utf8.length s > max_chars then
    Error (Printf.sprintf "field '%s' is longer than %d characters" field max_chars)
  else Ok s

let closed field allowed s =
  match Vocab.canonical allowed s with
  | Some c -> Ok (Text c)
  | None -> Error (Vocab.invalid_value ~what:field s allowed)

let dedup l =
  List.rev (List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l)

let consumers_value = function
  | `List items ->
      let item = function
        | `String s when String.trim s <> "" -> check_len "consumers" (String.trim s)
        | _ -> Error "field 'consumers' must be an array of non-empty strings or null"
      in
      let* l =
        List.fold_right
          (fun x acc -> Result.bind acc (fun l -> Result.map (fun y -> y :: l) (item x)))
          items (Ok [])
      in
      Ok (Strings (dedup l))
  | _ -> Error "field 'consumers' must be an array of non-empty strings or null"

let validate ~domains (field, v) =
  if not (List.mem field annotatable) then
    Error
      (Printf.sprintf "field '%s' cannot be annotated; allowed fields: %s" field
         (String.concat ", " annotatable))
  else
    match (field, v) with
    | _, `Null -> Ok (field, Clear)
    | "consumers", v -> Result.map (fun x -> (field, x)) (consumers_value v)
    | "domain", `String s -> Result.map (fun x -> (field, x)) (closed field domains s)
    | "status_override", `String s ->
        Result.map (fun x -> (field, x)) (closed field Vocab.statuses s)
    | _, `String s -> Result.map (fun s -> (field, Text s)) (check_len field s)
    | _ -> Error (Printf.sprintf "field '%s' must be a string or null" field)

let validate_all ~domains fields =
  if fields = [] then Error "no fields to annotate"
  else
    let names = List.map fst fields in
    if List.length (List.sort_uniq compare names) <> List.length names then
      Error "a field is given more than once"
    else
      List.fold_right
        (fun f acc ->
          Result.bind acc (fun l -> Result.map (fun x -> x :: l) (validate ~domains f)))
        fields (Ok [])

let host_of db id =
  Option.join
    (Db.query_one db "SELECT host FROM repos WHERE id = ?"
       [ Db.int id ]
       (fun r -> Db.to_string_opt r.(0)))

let strings_json l =
  Yojson.Safe.to_string (`List (List.map (fun s -> `String (Redact.text s)) l))

(* [field] is one of [annotatable], so it is a fixed column name, never user text. *)
let write_field db id (field, v) =
  let value =
    match v with
    | Text s -> Db.rtext s
    | Strings l -> Db.text (strings_json l)
    | Clear -> Sqlite3.Data.NULL
  in
  Db.run db
    (Printf.sprintf "UPDATE repos SET %s = ? WHERE id = ?" field)
    [ value; Db.int id ]

(* A field set through a column must not also survive in [extra] (EC-2 leftovers). *)
let drop_from_extra db id names =
  match
    Db.query_one db "SELECT extra FROM repos WHERE id = ?"
      [ Db.int id ]
      (fun r -> Db.to_string_opt r.(0))
  with
  | Some (Some s) -> (
      match Jsonx.parse s with
      | Some (`Assoc m) ->
          let kept = List.filter (fun (k, _) -> not (List.mem k names)) m in
          let v =
            if kept = [] then Sqlite3.Data.NULL
            else Db.text (Yojson.Safe.to_string (`Assoc kept))
          in
          Db.run db "UPDATE repos SET extra = ? WHERE id = ?" [ v; Db.int id ]
      | _ -> ())
  | _ -> ()

let annotate (ctx : Ctx.t) ~repo ~fields =
  let* values = validate_all ~domains:ctx.identity.domains fields in
  let* { Resolve.id; key } = Resolve.resolve ctx.db repo in
  let whitehat = host_of ctx.db id = Some "whitehat" in
  let* () =
    match
      List.find_opt
        (fun (f, _) -> whitehat && not (List.mem f whitehat_annotatable))
        values
    with
    | Some (f, _) ->
        Error
          (Printf.sprintf
             "field '%s' cannot be annotated on a whitehat row; allowed fields: %s" f
             (String.concat ", " whitehat_annotatable))
    | None -> Ok ()
  in
  let* () =
    Db.with_txn ctx.db (fun () ->
        List.iter (write_field ctx.db id) values;
        drop_from_extra ctx.db id (List.map fst values);
        Field_meta.mark_curated ctx.db ~repo_id:id ~fields:(List.map fst values)
          ~now:(Ctx.rfc3339 (ctx.now ()));
        Ok ())
  in
  let* () = Export.after_write ctx ~whitehat_changed:whitehat in
  Ok
    (`Assoc
       [
         ("key", `String key);
         ("updated", `List (List.map (fun (f, _) -> `String f) values));
         ("exported", `Bool true);
       ])

let stored_consumers db id =
  Db.query_one db "SELECT consumers FROM repos WHERE id = ?"
    [ Db.int id ]
    (fun r -> Db.to_string_opt r.(0))
  |> Option.join |> Jsonx.stored_strings

let text_arg name s =
  let s = String.trim s in
  if s = "" then Error (Printf.sprintf "%s must be a non-empty string" name)
  else if Utf8.length s > max_chars then
    Error (Printf.sprintf "%s is longer than %d characters" name max_chars)
  else Ok s

let record_usage (ctx : Ctx.t) ~repo ~consumer ~note =
  let* consumer = text_arg "consumer" consumer in
  let* note =
    match note with
    | None -> Ok None
    | Some n -> Result.map Option.some (text_arg "note" n)
  in
  let* { Resolve.id; key } = Resolve.resolve ctx.db repo in
  let* () =
    if host_of ctx.db id = Some "whitehat" then
      Error "usage cannot be recorded on a whitehat row"
    else Ok ()
  in
  let consumer = Redact.text consumer in
  let at = Ctx.rfc3339 (ctx.now ()) in
  let* consumers, count =
    Db.with_txn ctx.db (fun () ->
        Db.run ctx.db "INSERT INTO usage(repo_id, consumer, note, at) VALUES (?, ?, ?, ?)"
          [ Db.int id; Db.rtext consumer; Db.rtext_opt note; Db.text at ];
        let current = stored_consumers ctx.db id in
        let consumers =
          if List.mem consumer current then current else current @ [ consumer ]
        in
        Db.run ctx.db "UPDATE repos SET consumers = ? WHERE id = ?"
          [ Db.text (strings_json consumers); Db.int id ];
        Field_meta.mark_curated ctx.db ~repo_id:id ~fields:[ "consumers" ] ~now:at;
        let count =
          Option.join
            (Db.query_one ctx.db "SELECT count(*) FROM usage WHERE repo_id = ?"
               [ Db.int id ]
               (fun r -> Db.to_int_opt r.(0)))
        in
        Ok (consumers, Option.value count ~default:0))
  in
  let* () = Export.after_write ctx ~whitehat_changed:false in
  Ok
    (`Assoc
       [
         ("key", `String key);
         ("consumer", `String consumer);
         ("consumers", `List (List.map (fun s -> `String s) consumers));
         ("usage_count", `Int count);
         ("exported", `Bool true);
       ])
