(** Testing ------- Component: library-index import Invocation: dune exec
    test/test_import.exe Subject: [import] makes each listed library's usage rows equal to
    its [curated.json] [usage] list (edit, removal, duplicates, idempotence), leaves
    unlisted libraries alone, and refuses while [export_dirty] is set unless
    [--prefer-file]. *)

open Library_index
open Test_support.Support

let key = "github:example-org/acme-mcp"

let usage ?note ctx repo consumer =
  ok_or_fail
    (Ops.record_usage ctx
       (`Assoc
          ([ ("repo", `String repo); ("consumer", `String consumer) ]
          @ match note with Some n -> [ ("note", `String n) ] | None -> [])))
  |> ignore

(* A fresh DB whose export dir holds both files, as a data directory does. *)
let data_ctx () =
  let ctx = fresh_ctx () in
  ok_or_fail (Export.export ctx ~dir:ctx.export_dir);
  ctx

let curated_path (ctx : Ctx.t) = Filename.concat ctx.export_dir Export.curated_file
let exported ctx = ok_or_fail (Curated_io.read_json (curated_path ctx))

let write_curated ctx v =
  let oc = open_out_bin (curated_path ctx) in
  output_string oc (Yojson.Safe.pretty_to_string v);
  close_out oc

(* The usage rows of [k] as "consumer|note|at", in export order. *)
let rows (ctx : Ctx.t) k =
  Db.query ctx.db
    "SELECT u.consumer || '|' || coalesce(u.note, '-') || '|' || u.at FROM usage u JOIN \
     repos r ON r.id = u.repo_id WHERE r.key = ? ORDER BY u.at, u.id"
    [ Db.text k ]
    (fun r -> Option.value ~default:"" (Db.to_string_opt r.(0)))

let exported_notes ctx k =
  match member "usage" (member k (exported ctx)) with
  | `List l -> List.map (fun u -> to_str (member "note" u)) l
  | `Null -> []
  | j -> Alcotest.failf "usage is not a list: %s" (Yojson.Safe.to_string j)

(* Replace the [usage] list of entry [k] in curated.json ([None] drops the key). *)
let edit_usage ctx k f =
  let set_usage = function
    | `Assoc fields ->
        let others = List.filter (fun (n, _) -> n <> "usage") fields in
        let old = Option.value ~default:(`List []) (List.assoc_opt "usage" fields) in
        `Assoc (match f old with None -> others | Some u -> others @ [ ("usage", u) ])
    | j -> j
  in
  match exported ctx with
  | `Assoc entries ->
      write_curated ctx
        (`Assoc
           (List.map (fun (n, v) -> if n = k then (n, set_usage v) else (n, v)) entries))
  | _ -> Alcotest.fail "curated.json is not an object"

let import ?prefer_file (ctx : Ctx.t) =
  Init.import ~id ctx.db ~from_dir:ctx.export_dir ?prefer_file ()

let counts label ~added ~removed = function
  | Ok (s : Curated_io.summary) ->
      Alcotest.(check (pair int int))
        label (added, removed)
        (s.usage_added, s.usage_removed)
  | Error e -> Alcotest.failf "%s: import failed: %s" label e

let rename_note ~from ~to_ = function
  | `List l ->
      Some
        (`List
           (List.map
              (function
                | `Assoc u when List.assoc_opt "note" u = Some (`String from) ->
                    `Assoc
                      (List.map
                         (fun (n, v) -> if n = "note" then (n, `String to_) else (n, v))
                         u)
                | u -> u)
              l))
  | j -> Some j

(* The 2026-10-15 repro: record-usage, export, edit the note, import, export. *)
let edited_note_replaces () =
  let ctx = data_ctx () in
  usage ctx key "notes-app" ~note:"old note";
  Alcotest.(check (list string)) "exported" [ "old note" ] (exported_notes ctx key);
  edit_usage ctx key (rename_note ~from:"old note" ~to_:"new note");
  counts "edit = 1 added, 1 removed" ~added:1 ~removed:1 (import ctx);
  Alcotest.(check int) "one row" 1 (List.length (rows ctx key));
  ok_or_fail (Export.export ctx ~dir:ctx.export_dir);
  Alcotest.(check (list string))
    "re-export has only the new note" [ "new note" ] (exported_notes ctx key);
  counts "idempotent" ~added:0 ~removed:0 (import ctx)

let removed_and_absent () =
  let ctx = data_ctx () in
  usage ctx key "notes-app" ~note:"first";
  usage ctx key "notes-app" ~note:"second";
  edit_usage ctx key (function
    | `List [ first; _ ] -> Some (`List [ first; first ])
    | j -> Alcotest.failf "two records expected: %s" (Yojson.Safe.to_string j));
  counts "duplicate kept as a multiset" ~added:1 ~removed:1 (import ctx);
  Alcotest.(check int) "two identical rows" 2 (List.length (rows ctx key));
  edit_usage ctx key (fun _ -> None);
  counts "absent usage key = no usage" ~added:0 ~removed:2 (import ctx);
  Alcotest.(check (list string)) "no row" [] (rows ctx key);
  ok_or_fail (Export.export ctx ~dir:ctx.export_dir);
  Alcotest.(check (list string)) "nothing re-exported" [] (exported_notes ctx key)

(* A library not listed in curated.json keeps its rows (import never deletes rows). *)
let unlisted_untouched () =
  let ctx = data_ctx () in
  usage ctx key "notes-app" ~note:"kept";
  let before = rows ctx key in
  (match exported ctx with
  | `Assoc entries -> write_curated ctx (`Assoc (List.remove_assoc key entries))
  | _ -> Alcotest.fail "curated.json is not an object");
  counts "nothing for an unlisted key" ~added:0 ~removed:0 (import ctx);
  Alcotest.(check (list string)) "rows kept" before (rows ctx key)

(* Set ([Some v]) or drop ([None]) the string field [f] of entry [k] in curated.json. *)
let edit_field ctx k f v =
  match exported ctx with
  | `Assoc entries ->
      let set = function
        | `Assoc fields ->
            let others = List.filter (fun (n, _) -> n <> f) fields in
            `Assoc (match v with None -> others | Some s -> others @ [ (f, `String s) ])
        | j -> j
      in
      write_curated ctx
        (`Assoc (List.map (fun (n, e) -> if n = k then (n, set e) else (n, e)) entries))
  | _ -> Alcotest.fail "curated.json is not an object"

let notes (ctx : Ctx.t) k =
  Db.query_one ctx.db "SELECT notes FROM repos WHERE key = ?"
    [ Db.text k ]
    (fun r -> Db.to_string_opt r.(0))
  |> Option.join

(* A clean import: curated.json is the source of truth, a field removed there is cleared;
   only [--prefer-file] (a dirty DB) keeps a field the file lacks. *)
let field_removed_clears () =
  let ctx = data_ctx () in
  usage ctx key "notes-app" ~note:"keeps the entry listed";
  edit_field ctx key "notes" (Some "hand-written");
  counts "field set" ~added:0 ~removed:0 (import ctx);
  Alcotest.(check (option string)) "set" (Some "hand-written") (notes ctx key);
  edit_field ctx key "notes" None;
  counts "field dropped" ~added:0 ~removed:0 (import ctx);
  Alcotest.(check (option string)) "cleared" None (notes ctx key)

(* While an export failed (AM-10), the DB may hold usage the file lacks: refuse. *)
let dirty_refused () =
  let ctx = data_ctx () in
  usage ctx key "notes-app" ~note:"recorded";
  edit_usage ctx key (fun _ -> None);
  Db.set_meta ctx.db "export_dirty" "1";
  let before = rows ctx key in
  (match import ctx with
  | Ok _ -> Alcotest.fail "dirty import accepted"
  | Error e ->
      Alcotest.(check bool)
        "error names export" true
        (Re.execp (Re.compile (Re.str "export")) e);
      Alcotest.(check bool)
        "error names --prefer-file" true
        (Re.execp (Re.compile (Re.str "--prefer-file")) e));
  Alcotest.(check (list string)) "rows unchanged" before (rows ctx key)

(* The explicit override (FR-129): the file wins, its usage list included. *)
let dirty_prefer_file () =
  let ctx = data_ctx () in
  usage ctx key "notes-app" ~note:"recorded";
  edit_usage ctx key (fun _ -> None);
  Db.set_meta ctx.db "export_dirty" "1";
  counts "file wins" ~added:0 ~removed:1 (import ~prefer_file:true ctx);
  Alcotest.(check (list string)) "no row" [] (rows ctx key)

let () =
  run "import"
    [
      ( "usage-sync",
        [
          Alcotest.test_case "edited note replaces the old one" `Quick
            edited_note_replaces;
          Alcotest.test_case "removed records and duplicates" `Quick removed_and_absent;
          Alcotest.test_case "unlisted library untouched" `Quick unlisted_untouched;
        ] );
      ( "fields",
        [ Alcotest.test_case "removed field cleared" `Quick field_removed_clears ] );
      ( "dirty",
        [
          Alcotest.test_case "import refuses while dirty" `Quick dirty_refused;
          Alcotest.test_case "--prefer-file overrides" `Quick dirty_prefer_file;
        ] );
    ]
