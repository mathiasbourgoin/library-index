(* Resolver (CHECK-7, C-14) and the injected-clock status function (C-12). *)

open Library_index
open Test_support.Support

let resolved ctx ident =
  Result.map (fun (r : Resolve.t) -> r.key) (Resolve.resolve ctx.Ctx.db ident)

let contains s sub = Re.execp (Re.compile (Re.str sub)) s

let resolve () =
  let ctx = fresh_ctx () in
  let check_ok ident expected =
    Alcotest.(check (result string string)) ident (Ok expected) (resolved ctx ident)
  in
  check_ok "github:example-org/acme-mcp" "github:example-org/acme-mcp";
  check_ok "GitHub:Example-Org/Acme-MCP" "github:example-org/acme-mcp";
  check_ok "example-org/acme-mcp" "github:example-org/acme-mcp";
  check_ok "acme/acme-mcp" "gitlab:acme/acme-mcp";
  check_ok "sample-lib" "local:sample-lib";
  check_ok "  sample-lib  " "local:sample-lib";
  check_ok "proof-checker" "whitehat:shared-tools (shared)/proof-checker/";
  (match resolved ctx "acme-mcp" with
  | Ok k -> Alcotest.failf "bare ambiguous name resolved to %s" k
  | Error e ->
      Alcotest.(check bool)
        "lists github key" true
        (contains e "github:example-org/acme-mcp");
      Alcotest.(check bool) "lists gitlab key" true (contains e "gitlab:acme/acme-mcp"));
  (match resolved ctx "no-such-repo" with
  | Ok _ -> Alcotest.fail "unknown resolved"
  | Error e -> Alcotest.(check string) "unknown" "unknown repo 'no-such-repo'" e);
  Alcotest.(check bool) "empty rejected" true (Result.is_error (resolved ctx "   "));
  (* get goes through the same resolver, with the same messages. *)
  match Ops.get ctx (`Assoc [ ("repo", `String "acme-mcp") ]) with
  | Ok _ -> Alcotest.fail "get resolved an ambiguous name"
  | Error e ->
      Alcotest.(check bool)
        "get lists candidates" true
        (contains e "gitlab:acme/acme-mcp")

let at s = Option.get (Status.parse_time s)
let now = at "2026-10-03T12:00:00Z"

let days_ago n =
  Ptime.to_rfc3339 (Option.get (Ptime.sub_span now (Ptime.Span.of_int_s (n * 86400))))

let st ?(archived = false) ?override ?last_commit ?pushed_at () =
  Status.effective ~now ~override ~archived ~last_commit ~pushed_at

let status () =
  let check label expected got = Alcotest.(check string) label expected got in
  check "90 days active" "active" (st ~pushed_at:(days_ago 90) ());
  check "91 days dormant" "dormant" (st ~pushed_at:(days_ago 91) ());
  check "730 days dormant" "dormant" (st ~pushed_at:(days_ago 730) ());
  check "731 days stale" "stale" (st ~pushed_at:(days_ago 731) ());
  check "last_commit preferred" "active"
    (st ~last_commit:(days_ago 1) ~pushed_at:(days_ago 900) ());
  check "empty last_commit falls back" "stale"
    (st ~last_commit:"" ~pushed_at:(days_ago 900) ());
  check "archived wins" "archived" (st ~archived:true ~pushed_at:(days_ago 1) ());
  check "override wins" "active" (st ~archived:true ~override:"active" ());
  check "no date unknown" "unknown" (st ());
  check "unparseable unknown" "unknown" (st ~pushed_at:"yesterday" ());
  check "offset + fraction" "active" (st ~last_commit:"2026-09-30T23:30:00.123+02:00" ());
  check "date only" "dormant" (st ~last_commit:"2026-06-27" ());
  Alcotest.(check int)
    "floor of partial days" 0
    (Status.age_days ~now (at "2026-10-03T00:00:01Z"));
  Alcotest.(check int)
    "future is negative" (-1)
    (Status.age_days ~now (at "2026-10-04T00:00:00Z"))

let () =
  run "store"
    [
      ( "resolve",
        [ Alcotest.test_case "key, owner/name, bare name, ambiguity" `Quick resolve ] );
      ("status", [ Alcotest.test_case "injected clock thresholds" `Quick status ]);
    ]
