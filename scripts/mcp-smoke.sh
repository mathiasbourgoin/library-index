#!/usr/bin/env bash
# MCP stdio smoke test (spec CHECK-14, AM-8).
#
# Initialises a throw-away data dir and DB from the synthetic test fixtures
# (test/fixtures/sample) in a temp dir, pipes
# initialize + notifications/initialized + tools/list + one tools/call into
# `library-index mcp`, and asserts: every stdout line is a JSON-RPC message, the tool set is
# exactly $EXPECTED_TOOLS, every query and registration tool declares an outputSchema
# (FR-092), and the
# tools/call succeeded with structuredContent.
#
# Exit codes: 0 = pass, 1 = an assertion fired, 2 = setup error (build, init, python3).
# Never touches a real data directory, notes directory, curated.json or INDEX.md.
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
EXPECTED_TOOLS=${EXPECTED_TOOLS:-"analyze-repo annotate copies explain find-capability get list list-sources query-docs record_usage refresh register-repo register-source resolve-library-id search unregister-repo unregister-source whats-new"}
CALL_TOOL=${CALL_TOOL:-search}
CALL_ARGS=${CALL_ARGS:-'{"query":"ocaml mcp","limit":3}'}

command -v python3 >/dev/null 2>&1 || { echo "mcp-smoke: python3 not found" >&2; exit 2; }

BIN=${LIBRARY_INDEX_BIN:-}
if [ -z "$BIN" ]; then
  (cd "$ROOT" && dune build ./bin/main.exe 2>&1) >/dev/null || {
    echo "mcp-smoke: build failed" >&2
    exit 2
  }
  BIN="$ROOT/_build/default/bin/main.exe"
fi
[ -x "$BIN" ] || { echo "mcp-smoke: binary not found: $BIN" >&2; exit 2; }

T=$(mktemp -d "${TMPDIR:-/tmp}/library-index-smoke.XXXXXX") || exit 2
trap 'rm -r -- "$T"' EXIT
export LIBRARY_INDEX_DATA="$T"
export LIBRARY_INDEX_DB="$T/smoke.db"

"$BIN" init --from "$ROOT/test/fixtures/sample" >"$T/init.out" 2>"$T/init.err" || {
  echo "mcp-smoke: init failed" >&2
  exit 2
}

{
  printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"mcp-smoke","version":"0"}}}'
  printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'
  printf '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"%s","arguments":%s}}\n' "$CALL_TOOL" "$CALL_ARGS"
} | "$BIN" mcp >"$T/out" 2>"$T/err"
status=$?
[ "$status" -eq 0 ] || { echo "mcp-smoke: server exited $status" >&2; exit 1; }

EXPECTED_TOOLS="$EXPECTED_TOOLS" python3 - "$T/out" <<'PY'
import json, os, sys
lines = [l for l in open(sys.argv[1], encoding="utf-8").read().splitlines() if l.strip()]
try:
    msgs = [json.loads(l) for l in lines]
except ValueError:
    print("mcp-smoke: non-JSON output on stdout", file=sys.stderr); sys.exit(1)
if any(m.get("jsonrpc") != "2.0" for m in msgs):
    print("mcp-smoke: non-JSON-RPC message on stdout", file=sys.stderr); sys.exit(1)
by_id = {m.get("id"): m for m in msgs}
if sorted(by_id) != [1, 2, 3] or len(msgs) != 3:
    print(f"mcp-smoke: expected responses to ids 1,2,3, got {sorted(map(str, by_id))}", file=sys.stderr); sys.exit(1)
if "result" not in by_id[1] or "untrusted" not in by_id[1]["result"].get("instructions", ""):
    print("mcp-smoke: initialize failed or instructions lack the untrusted-content notice", file=sys.stderr); sys.exit(1)
names = sorted(t["name"] for t in by_id[2]["result"]["tools"])
expected = sorted(os.environ["EXPECTED_TOOLS"].split())
if names != expected:
    print(f"mcp-smoke: tools {names} != expected {expected}", file=sys.stderr); sys.exit(1)
if any("inputSchema" not in t for t in by_id[2]["result"]["tools"]):
    print("mcp-smoke: a tool lacks inputSchema", file=sys.stderr); sys.exit(1)
query_tools = {"search", "get", "list", "resolve-library-id", "query-docs", "list-sources", "find-capability", "copies", "explain", "whats-new"}
registration_tools = {"register-repo", "register-source", "unregister-repo", "unregister-source", "analyze-repo"}
lacking = [t["name"] for t in by_id[2]["result"]["tools"]
           if t["name"] in query_tools | registration_tools and "outputSchema" not in t]
if lacking:
    print(f"mcp-smoke: query tools without outputSchema: {lacking}", file=sys.stderr); sys.exit(1)
res = by_id[3].get("result", {})
if res.get("isError") is not False:
    print("mcp-smoke: tools/call returned an error", file=sys.stderr); sys.exit(1)
if "structuredContent" not in res:
    print("mcp-smoke: tools/call result lacks structuredContent", file=sys.stderr); sys.exit(1)
print(f"mcp-smoke: ok ({len(names)} tools: {', '.join(names)})")
PY
