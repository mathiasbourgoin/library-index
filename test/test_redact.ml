(* Redaction (CHECK-16, C-32, FR-070): every closed-list pattern, and the write paths. *)

open Library_index
open Test_support.Support

let r = Redact.text
let check label expected got = Alcotest.(check string) label expected got
let rep n c = String.make n c

let patterns () =
  check "ghp" "x [redacted] y" (r ("x ghp_" ^ rep 36 'a' ^ " y"));
  List.iter
    (fun p -> check ("gh" ^ p) "[redacted]" (r ("gh" ^ p ^ "_" ^ rep 40 'B')))
    [ "o"; "u"; "s"; "r" ];
  check "github_pat" "[redacted]" (r ("github_pat_" ^ rep 22 '_'));
  check "glpat" "t=[redacted]" (r ("t=glpat-" ^ rep 20 'x'));
  check "glptt" "[redacted]" (r ("glptt-" ^ rep 25 '-'));
  check "slack" "[redacted]" (r "xoxb-1234567890-abc");
  check "sk" "[redacted]" (r ("sk-" ^ rep 32 '9'));
  check "npm" "[redacted]" (r ("npm_" ^ rep 36 'z'));
  check "aws" "id [redacted]." (r "id AKIAABCDEFGHIJKLMNOP.");
  check "pem" "a\n[redacted]\nb"
    (r "a\n-----BEGIN RSA PRIVATE KEY-----\nMIIE\nabc\n-----END RSA PRIVATE KEY-----\nb");
  check "two pem blocks stay two" "[redacted] mid [redacted]"
    (r
       "-----BEGIN PRIVATE KEY-----\n\
        x\n\
        -----END PRIVATE KEY----- mid -----BEGIN OPENSSH PRIVATE KEY-----\n\
        y\n\
        -----END OPENSSH PRIVATE KEY-----");
  check "unterminated pem" "k: [redacted]" (r "k: -----BEGIN EC PRIVATE KEY-----\nzzz");
  check "url userinfo" "clone https://host/x.git" (r "clone https://u:p@host/x.git");
  check "url user only" "ssh://github.com/a" (r "ssh://git@github.com/a");
  check "token in url" "https://github.com"
    (r ("https://x:ghp_" ^ rep 36 'q' ^ "@github.com"))

(* AM-19. Every token-shaped string is assembled at run time ("sk" ^ "-ant-" ...), so no
   literal token sits in the source. *)
let am19_patterns () =
  let body n =
    String.concat "" (List.init n (fun i -> String.make 1 "aZ9_-".[i mod 5]))
  in
  check "sk-ant" "k=[redacted] ." (r ("k=" ^ "sk" ^ "-ant-api03-" ^ body 40 ^ " ."));
  check "sk-proj" "[redacted]" (r ("sk" ^ "-proj-" ^ body 48));
  check "sk-svcacct" "[redacted]" (r ("sk" ^ "-svcacct-" ^ body 30));
  check "glrt" "runner [redacted]" (r ("runner " ^ "gl" ^ "rt-" ^ body 20));
  check "gldt" "[redacted]" (r ("gl" ^ "dt-" ^ body 24));
  check "glcbt" "[redacted]" (r ("gl" ^ "cbt-" ^ body 24));
  let jwt = "ey" ^ "J" ^ body 20 ^ ".ey" ^ "J" ^ body 30 ^ "." ^ body 43 in
  check "jwt" "t [redacted] u" (r ("t " ^ jwt ^ " u"));
  check "bearer header" "Authorization: Bearer [redacted]"
    (r ("Authorization: Bearer " ^ body 32));
  check "bearer lowercase" "bearer [redacted]" (r ("bearer " ^ rep 24 'q'));
  check "pgp block" "a [redacted] b"
    (r
       ("a -----BEGIN PGP " ^ "PRIVATE KEY BLOCK-----\nlQOYBF\nxyz\n-----END PGP "
      ^ "PRIVATE KEY BLOCK----- b"));
  check "unterminated pgp" "k: [redacted]"
    (r ("k: -----BEGIN PGP " ^ "PRIVATE KEY BLOCK-----\nlQOY"));
  List.iter
    (fun s -> check ("untouched: " ^ s) s (r s))
    [
      "Bearer tokens are sent in a header";
      "use a Bearer token";
      "sk-ant-short";
      "eyJ.eyJ.x";
      "-----BEGIN PGP PUBLIC KEY BLOCK-----";
    ]

(* Base58check secret keys (edsk... families), assembled at run time like the AM-19 tokens: no key sits in the
   source. Public keys and key hashes are not secrets. *)
let base58_secret_keys () =
  let b58 n =
    String.init n (fun i ->
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".[i * 7 mod 58])
  in
  List.iter
    (fun prefix ->
      check ("unencrypted " ^ prefix) "k [redacted]."
        (r ("k " ^ prefix ^ "sk" ^ b58 50 ^ ".")))
    [ "ed"; "sp"; "p2"; "BL" ];
  check "full ed25519 key" "[redacted]" (r ("ed" ^ "sk" ^ b58 94));
  check "unencrypted: prefix" "unencrypted:[redacted]"
    (r ("unencrypted:" ^ "ed" ^ "sk" ^ b58 50));
  List.iter
    (fun prefix ->
      check ("encrypted " ^ prefix) "[redacted]" (r (prefix ^ "esk" ^ b58 83)))
    [ "ed"; "sp"; "p2"; "BL" ];
  Alcotest.(check bool)
    "contains_secret" true
    (Redact.contains_secret ("ed" ^ "sk" ^ b58 50));
  List.iter
    (fun s -> check ("untouched: " ^ s) s (r s))
    [
      "ed" ^ "pk" ^ b58 50;
      "BL" ^ "pk" ^ b58 72;
      "tz1" ^ b58 33;
      "the edsk prefix";
      "ed" ^ "sk" ^ b58 20;
    ]

(* AM-20: invalid UTF-8 becomes U+FFFD before storage; valid text is unchanged. *)
let utf8 () =
  let fffd = "\xef\xbf\xbd" in
  check "valid unchanged" "café — naïve 日本" (Utf8.sanitize "café — naïve 日本");
  check "invalid bytes" ("a" ^ fffd ^ fffd ^ "b") (Utf8.sanitize "a\xff\xfeb");
  check "truncated sequence" ("x" ^ fffd) (Utf8.sanitize "x\xc3");
  check "overlong" (fffd ^ fffd) (Utf8.sanitize "\xc0\xaf");
  check "surrogate" (fffd ^ fffd ^ fffd) (Utf8.sanitize "\xed\xa0\x80");
  check "through Redact.text" ("r" ^ fffd) (r "r\xff");
  Alcotest.(check bool)
    "valid output" true
    (String.is_valid_utf_8 (Utf8.sanitize "\x80\xbf\xf8\xf4\x90\x80\x80"));
  match Utf8.sanitize_json (`Assoc [ ("k\xff", `List [ `String "v\xfe" ]) ]) with
  | `Assoc [ (k, `List [ `String v ]) ] ->
      check "json key" ("k" ^ fffd) k;
      check "json value" ("v" ^ fffd) v
  | _ -> Alcotest.fail "sanitize_json changed the shape"

let non_secrets () =
  List.iter
    (fun s -> check s s (r s))
    [
      "ghp_short";
      "sk-learn is a library";
      "mail me at a@b.org";
      "https://example.com/path@v2";
      "see https://e.com/?q=a@b";
      "mcp-kit: OCaml library for MCP servers";
      "café — naïve";
    ];
  let s = "x ghp_" ^ rep 36 'a' in
  check "idempotent" (r s) (r (r s));
  let j = Redact.json (`Assoc [ ("a", `List [ `String ("AKIA" ^ rep 16 'A') ]) ]) in
  check "json recursion" {|{"a":["[redacted]"]}|} (Yojson.Safe.to_string j)

let write_paths () =
  let dir = temp_dir () in
  let src = Filename.concat dir "src" in
  Unix.mkdir src 0o755;
  let token = "ghp_" ^ rep 36 'k' in
  let write name s =
    let oc = open_out_bin (Filename.concat src name) in
    output_string oc s;
    close_out oc
  in
  write "curated.json"
    (Printf.sprintf
       {|{"_doc": "doc %s", "github:a/b": {"what": "w %s", "consumers": ["c %s"], "custom": "x %s", "usage": [{"consumer": "u %s", "note": "n %s", "at": "2026-01-01T00:00:00Z"}]}}|}
       token token token token token token);
  write "whitehat_tools.json"
    (Printf.sprintf
       {|{"_policy": "p", "tools": [{"campaign": "c", "path": "p/", "what": "https://u:pw@h/x", "language": "OCaml", "last_commit": "2026-01-01", "reuse": "r %s"}]}|}
       token);
  let db_path = Filename.concat dir "t.db" in
  ignore (ok_or_fail (Init.init ~id ~db_path ~from_dir:src ~force:false ()));
  let ctx = open_ctx db_path in
  let out = temp_dir () in
  ok_or_fail (Export.export ctx ~dir:out);
  let dump =
    String.concat "\n"
      (Db.query ctx.db
         "SELECT coalesce(what,'') || coalesce(consumers,'') || coalesce(extra,'') || \
          coalesce(interest,'') FROM repos"
         [] (fun row -> Option.value ~default:"" (Db.to_string_opt row.(0))))
  in
  let all =
    String.concat "\n"
      [
        dump;
        read_file (Filename.concat out "curated.json");
        read_file (Filename.concat out "whitehat_tools.json");
        String.concat ""
          (Db.query ctx.db "SELECT consumer || coalesce(note,'') FROM usage" []
             (fun row -> Option.value ~default:"" (Db.to_string_opt row.(0))));
        Option.value ~default:"" (Db.get_meta ctx.db "_doc");
      ]
  in
  Alcotest.(check bool)
    "no token stored or exported" false
    (Re.execp (Re.compile (Re.str token)) all);
  Alcotest.(check bool)
    "no URL password" false
    (Re.execp (Re.compile (Re.str "u:pw")) all);
  Alcotest.(check bool)
    "marker present" true
    (Re.execp (Re.compile (Re.str "[redacted]")) all)

let () =
  run "redact"
    [
      ("patterns", [ Alcotest.test_case "closed pattern list" `Quick patterns ]);
      ("am19", [ Alcotest.test_case "AM-19 credential formats" `Quick am19_patterns ]);
      ( "base58-secret-keys",
        [ Alcotest.test_case "base58check secret keys" `Quick base58_secret_keys ] );
      ("utf8", [ Alcotest.test_case "AM-20 UTF-8 sanitation" `Quick utf8 ]);
      ("non-secrets", [ Alcotest.test_case "plain text untouched" `Quick non_secrets ]);
      ( "write-paths",
        [ Alcotest.test_case "import + export never store tokens" `Quick write_paths ] );
    ]
