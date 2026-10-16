(* Review round 1 fixes on the fetch and subprocess paths:
   - am18 (AM-18, FR-034): a failed or skipped README / synopsis fetch never yields a
     synopsis or README value that would overwrite the stored one.
   - synopsis-precedence (C-19): local manifests, else API root-listing manifests, else
     the README's first paragraph, even when a checkout exists without a manifest.
   - proc-bounds (AM-15): the timeout also bounds the wait for a child that closed its
     pipes; stderr kept for classification is capped.
   - classification: failure classes match whole phrases only. *)

open Library_index
open Test_support.Support
module Fake_forge = Test_support.Fake_forge

let due = { Decision.readme = true; detail = false }
let fn = "example-user/demo"

let row ?local_dir ?(host = "github") () =
  `Assoc
    ([
       ("key", `String (host ^ ":" ^ fn));
       ("host", `String host);
       ("owner", `String (if host = "local" then "(local)" else "example-user"));
       ("name", `String "demo");
       ("full_name", `String (if host = "local" then "demo" else fn));
       ("fork", `Bool false);
       ("archived", `Bool false);
     ]
    @
    match local_dir with
    | Some d -> [ ("local_paths", `List [ `String d ]) ]
    | None -> [])

let fetch forge r =
  let call (c : Commands.t) = Fake_forge.runner forge ~timeout:30. c.argv in
  Fetch.run ~id ~call ~row:r ~search:(0, 0) due

let synopsis (o : Fetch.outcome) = List.assoc_opt "synopsis" o.fields
let readme = "# Demo\n\nDemo indexes repositories for agents, from its README text.\n"

let am18 () =
  let forge = Fake_forge.create () in
  Fake_forge.fail forge ("repos/" ^ fn ^ "/readme") `Network;
  Fake_forge.json forge ("repos/" ^ fn ^ "/contents/") (`List []);
  let o = fetch forge (row ()) in
  Alcotest.(check bool) "README failed: no README value" true (o.readme = None);
  Alcotest.(check bool) "README failed: synopsis untouched" true (synopsis o = None);
  Alcotest.(check bool)
    "failure recorded" true
    (List.mem_assoc Commands.Readme o.failures);
  (* A manifest fetch failure also keeps the stored synopsis. *)
  let forge = Fake_forge.create () in
  Fake_forge.ok forge ("repos/" ^ fn ^ "/readme") readme;
  Fake_forge.json forge
    ("repos/" ^ fn ^ "/contents/")
    (`List [ `Assoc [ ("name", `String "demo.opam"); ("type", `String "file") ] ]);
  Fake_forge.fail forge ("repos/" ^ fn ^ "/contents/demo.opam") `Network;
  let o = fetch forge (row ()) in
  Alcotest.(check bool) "manifest failed: synopsis untouched" true (synopsis o = None);
  Alcotest.(check bool) "README itself stored" true (o.readme <> None);
  (* A [local:] row whose checkout is gone: skipped, not "no README". *)
  let o = fetch (Fake_forge.create ()) (row ~host:"local" ()) in
  Alcotest.(check bool) "checkout gone: README untouched" true (o.readme = None);
  Alcotest.(check bool) "checkout gone: synopsis untouched" true (synopsis o = None);
  (* A genuine "no README" (404) still records the absence. *)
  let forge = Fake_forge.create () in
  Fake_forge.json forge ("repos/" ^ fn ^ "/contents/") (`List []);
  let o = fetch forge (row ()) in
  Alcotest.(check bool) "404 README is an absence" true (o.readme = Some None);
  Alcotest.(check bool) "absence clears the synopsis" true (synopsis o = Some `Null)

let checkout files =
  let d = temp_dir () in
  List.iter
    (fun (name, content) ->
      Out_channel.with_open_bin (Filename.concat d name) (fun oc ->
          output_string oc content))
    files;
  d

let precedence () =
  let api_forge () =
    let forge = Fake_forge.create () in
    Fake_forge.ok forge ("repos/" ^ fn ^ "/readme") readme;
    Fake_forge.json forge
      ("repos/" ^ fn ^ "/contents/")
      (`List [ `Assoc [ ("name", `String "demo.opam"); ("type", `String "file") ] ]);
    Fake_forge.ok forge
      ("repos/" ^ fn ^ "/contents/demo.opam")
      "synopsis: \"From the API\"\n";
    forge
  in
  let syn o = match synopsis o with Some (`String s) -> s | _ -> "<none>" in
  (* Checkout without a manifest: the API listing's manifest wins over the README. *)
  let d =
    checkout
      [ ("README.md", "# Local\n\nThe local README first paragraph, long enough.\n") ]
  in
  Alcotest.(check string)
    "API manifest before README paragraph" "From the API"
    (syn (fetch (api_forge ()) (row ~local_dir:d ())));
  (* Checkout with a manifest: the local manifest wins. *)
  let d = checkout [ ("x.opam", "synopsis: \"From the checkout\"\n") ] in
  Alcotest.(check string)
    "local manifest first" "From the checkout"
    (syn (fetch (api_forge ()) (row ~local_dir:d ())));
  (* No manifest anywhere: the checkout README paragraph, then the fetched one. *)
  let forge = Fake_forge.create () in
  Fake_forge.ok forge ("repos/" ^ fn ^ "/readme") readme;
  Fake_forge.json forge ("repos/" ^ fn ^ "/contents/") (`List []);
  let d =
    checkout
      [ ("README.md", "# Local\n\nThe local README first paragraph, long enough.\n") ]
  in
  Alcotest.(check string)
    "local README paragraph" "The local README first paragraph, long enough."
    (syn (fetch forge (row ~local_dir:d ())));
  Alcotest.(check string)
    "fetched README paragraph"
    "Demo indexes repositories for agents, from its README text."
    (syn (fetch forge (row ())))

let proc_bounds () =
  let t0 = Unix.gettimeofday () in
  (match Proc.run ~timeout:0.5 [| "sh"; "-c"; "exec >&- 2>&-; sleep 5" |] with
  | Error `Network -> ()
  | _ -> Alcotest.fail "a child that closed its pipes must time out as network");
  let waited = Unix.gettimeofday () -. t0 in
  Alcotest.(check bool) (Printf.sprintf "bounded wait (%.2fs)" waited) true (waited < 2.);
  (* Only the first 64 KiB of stderr are kept: a class phrase after them is not seen. *)
  let noisy where =
    let pad =
      Printf.sprintf "head -c %d /dev/zero | tr '\\0' x >&2" (Proc.max_stderr * 2)
    in
    let phrase = "echo ' HTTP 404 ' >&2" in
    let body = if where = `First then phrase ^ "; " ^ pad else pad ^ "; " ^ phrase in
    Proc.run ~timeout:5. [| "sh"; "-c"; body ^ "; exit 1" |]
  in
  Alcotest.(check bool) "phrase within the cap" true (noisy `First = Error `Not_found);
  Alcotest.(check bool) "phrase beyond the cap dropped" true (noisy `Last = Error `Other)

let classification () =
  let c = Proc.classify_failure in
  let check name expected s =
    Alcotest.(check string) name (Proc.describe expected) (Proc.describe (c s))
  in
  check "gh 404" `Not_found "gh: Not Found (HTTP 404)";
  check "glab 404" `Not_found "ERROR: 404 Not Found";
  check "command not found" `Other "sh: 1: gh: command not found";
  check "branch not found" `Other "fatal: branch not found";
  check "tls inside a word" `Other "the settlement thereof failed";
  check "eof inside a word" `Other "see the theeof column";
  check "tls handshake" `Network "net/http: TLS handshake timeout";
  check "unexpected eof" `Network "read: unexpected EOF";
  check "5xx" `Network "gh: Server Error (HTTP 502)";
  check "rate limit first" `Rate_limit "HTTP 403: API rate limit exceeded (HTTP 404)"

let () =
  run "fetch-edges"
    [
      ("am18", [ Alcotest.test_case "failed/skipped keeps stored text" `Quick am18 ]);
      ( "synopsis-precedence",
        [ Alcotest.test_case "C-19 with a checkout" `Quick precedence ] );
      ( "proc-bounds",
        [ Alcotest.test_case "wait deadline, stderr cap" `Quick proc_bounds ] );
      ("classification", [ Alcotest.test_case "whole phrases" `Quick classification ]);
    ]
