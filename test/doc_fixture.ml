(* Fixtures for the documents store tests (S12): hand-built repository rows, real git
   checkouts in a temp dev root, and a runner that sends git to the real (read-only)
   binary and gh/glab to the fake forge. Never touches ~/dev, ~/notes or the network. *)

open Library_index

let rec mkdir_p d =
  if not (Sys.file_exists d) then (
    mkdir_p (Filename.dirname d);
    Unix.mkdir d 0o755)

let write_file path s =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* A git working tree whose index holds [files]: `git ls-files` lists them. *)
let git_checkout dir files =
  List.iter (fun (p, s) -> write_file (Filename.concat dir p) s) files;
  let q = Filename.quote dir in
  let cmd =
    Printf.sprintf
      "GIT_CONFIG_NOSYSTEM=1 git -C %s init -q && GIT_CONFIG_NOSYSTEM=1 git -C %s add -A"
      q q
  in
  if Sys.command cmd <> 0 then failwith "git init/add failed"

let git_add dir = ignore (Sys.command ("git -C " ^ Filename.quote dir ^ " add -A"))

(* Insert a fetched repository row directly (derived columns set by hand). *)
let insert_repo (ctx : Ctx.t) ?(host = "github") ?(tier = "1") ?(campaign = false)
    ?local_dir ?(pushed = "2026-10-01T00:00:00Z") ?gl_id ?(fork = false) ?parent
    ?(archived = false) ?what ?last_commit ~owner name =
  let full_name = if host = "local" then name else owner ^ "/" ^ name in
  let key =
    String.lowercase_ascii
      (if host = "local" then "local:" ^ name else host ^ ":" ^ full_name)
  in
  let opt = function Some v -> Sqlite3.Data.TEXT v | None -> Sqlite3.Data.NULL in
  Db.run ctx.db
    {|INSERT INTO repos(key, host, owner, name, full_name, private, fork, archived,
  default_branch, pushed_at, last_commit, fetched, campaign, tier, local_paths, gl_id,
  what, parent)
VALUES (?, ?, ?, ?, ?, 0, ?, ?, 'main', ?, ?, 1, ?, ?, ?, ?, ?, ?)|}
    [
      Db.text key;
      Db.text host;
      Db.text owner;
      Db.text name;
      Db.text full_name;
      Db.int (Bool.to_int fork);
      Db.int (Bool.to_int archived);
      Db.text pushed;
      opt last_commit;
      Db.int (Bool.to_int campaign);
      Db.text tier;
      opt (Option.map (fun d -> Yojson.Safe.to_string (`List [ `String d ])) local_dir);
      (match gl_id with Some n -> Db.int n | None -> Sqlite3.Data.NULL);
      opt what;
      opt parent;
    ];
  key

let mixed_env forge =
  let fake = Fake_forge.runner forge in
  {
    (Fake_forge.env forge) with
    run =
      (fun ~timeout argv ->
        if Array.length argv > 0 && argv.(0) = "git" then Proc.run ~timeout argv
        else fake ~timeout argv);
  }

let ingest ?(full = false) ?key (ctx : Ctx.t) env =
  let call = Repo_refresh.call env (Repo_refresh.limiter ()) in
  Doc_refresh.run ctx.db ~call ~now:(ctx.now ()) ~full ~workers:2 ~file_workers:2 ?key ()

let docs (ctx : Ctx.t) key =
  Db.query ctx.db
    {|SELECT d.path, d.kind, d.source FROM documents d JOIN repos r ON r.id = d.repo_id
WHERE r.key = ? ORDER BY d.path|}
    [ Db.text key ]
    (fun r ->
      let s i = Option.value (Db.to_string_opt r.(i)) ~default:"" in
      (s 0, s 1, s 2))

let chunk_titles (ctx : Ctx.t) key path =
  Db.query ctx.db
    {|SELECT c.title FROM doc_chunks c JOIN documents d ON d.id = c.doc_id
JOIN repos r ON r.id = d.repo_id WHERE r.key = ? AND d.path = ? ORDER BY c.ord|}
    [ Db.text key; Db.text path ]
    (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")

let chunk_text (ctx : Ctx.t) key path title =
  Db.query_one ctx.db
    {|SELECT c.text FROM doc_chunks c JOIN documents d ON d.id = c.doc_id
JOIN repos r ON r.id = d.repo_id WHERE r.key = ? AND d.path = ? AND c.title = ?|}
    [ Db.text key; Db.text path; Db.text title ]
    (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")
  |> Option.value ~default:""

let docs_integrity (ctx : Ctx.t) =
  Db.exec ctx.db "INSERT INTO doc_chunks_fts(doc_chunks_fts) VALUES('integrity-check')"

let guide_md =
  "## Installing\n\n\
   Run `opam install demo` to install the library.\n\n\
   ## Usage\n\n\
   Call Demo.run with a configuration record.\n"

let itf_mli =
  "(** Itf: ITF trace files.\n\n\
  \    Parsing and replay of Apalache traces. *)\n\n\
   (** Parse an ITF trace file from disk. *)\n\
   val parse_file : string -> (t, string) result\n\n\
   type t = { states : int }\n\
   (** A parsed trace. *)\n\n\
   module Sub : sig\n\
  \  val inner : int\n\
   end\n\n\
   (** {1 Errors} *)\n\n\
   exception Bad of string\n\
   (** Raised on bad input. *)\n\n\
   external fast : int -> int = \"c_fast\"\n\n\
   val ( >>= ) : int -> int -> int\n\n\
   open Stdlib\n\n\
   type 'a box = Box of 'a\n\
   and other = int\n"

(* ------------------------------------------------------------ resolve fixtures *)

let add_doc (ctx : Ctx.t) key ?(kind = Doc_select.Doc) path text =
  let id =
    Option.value ~default:0
      (Option.join
         (Db.query_one ctx.db "SELECT id FROM repos WHERE key = ?"
            [ Db.text key ]
            (fun r -> Db.to_int_opt r.(0))))
  in
  ignore
    (Doc_store.write ctx.db ~repo_id:id ~now:"2026-10-03T00:00:00Z"
       { path; kind; source = Local; text; blob_sha = None })

(* github:example-org/acme-mcp, its GitLab mirror, a same-host fork, and a consumer. *)
let mcp_fixture () =
  let ctx = Support.empty_ctx () in
  let main =
    insert_repo ctx ~owner:"example-org" ~what:"MCP server library for OCaml"
      ~last_commit:"2026-10-01" "acme-mcp"
  in
  let mirror =
    insert_repo ctx ~host:"gitlab" ~gl_id:7 ~owner:"acme" ~what:"GitLab copy of acme-mcp"
      ~last_commit:"2026-10-01" "acme-mcp"
  in
  let fork =
    insert_repo ctx ~owner:"example-user" ~fork:true ~parent:"example-org/acme-mcp"
      ~last_commit:"2026-09-01" ~what:"Fork with experiments" "acme-mcp"
  in
  let user =
    insert_repo ctx ~owner:"example-user" ~what:"Notes server built on acme-mcp"
      "notes-app"
  in
  add_doc ctx main "docs/guide.md" guide_md;
  (ctx, main, mirror, fork, user)

(* ------------------------------------------------------------ forge tree fixtures *)

let gh_tree entries =
  `Assoc
    [
      ( "tree",
        `List
          (List.map
             (fun (p, sha) ->
               `Assoc
                 [ ("path", `String p); ("type", `String "blob"); ("sha", `String sha) ])
             entries
          @ [
              `Assoc
                [
                  ("path", `String "docs"); ("type", `String "tree"); ("sha", `String "t");
                ];
            ]) );
      ("truncated", `Bool false);
    ]

let file_path fn p = Printf.sprintf "repos/%s/contents/%s" fn p
let tree_path fn = Printf.sprintf "repos/%s/git/trees/main?recursive=1" fn

let file_calls forge =
  List.length
    (List.filter
       (fun a -> Re.execp (Re.compile (Re.str "/contents/")) (Fake_forge.key_of a))
       (Fake_forge.calls forge))
