#!/usr/bin/env bash
# CHECK-15: register-mcp.sh --dry-run under a temporary HOME prints both registrations
# and modifies nothing. Exit 0 pass, 1 assertion fired, 2 setup error.
set -uo pipefail
unset XDG_DATA_HOME LIBRARY_INDEX_DATA LIBRARY_INDEX_DB
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/_build/default/bin/main.exe"
[ -x "$BIN" ] || { echo "check-register: build first (dune build)" >&2; exit 2; }
H="$(mktemp -d)" || exit 2
trap 'rm -r "$H"' EXIT
mkdir -p "$H/.codex" && printf '[mcp_servers.other]\ncommand = "x"\n' >"$H/.codex/config.toml"
before="$(find "$H" -type f -exec md5sum {} + | sort)"
out="$(HOME="$H" bash "$ROOT/scripts/register-mcp.sh" --dry-run --bin "$BIN" 2>&1)" || { echo "$out"; exit 1; }
after="$(find "$H" -type f -exec md5sum {} + | sort)"
[ "$before" = "$after" ] || { echo "check-register: HOME was modified"; exit 1; }
grep -q 'claude mcp add --transport stdio --env LIBRARY_INDEX_DATA=.* --scope user library-index -- .* mcp' <<<"$out" || { echo "check-register: claude command missing"; exit 1; }
# The default data dir is the XDG one.
grep -qF "LIBRARY_INDEX_DATA = \"$H/.local/share/library-index\"" <<<"$out" || { echo "check-register: default data dir is not XDG"; echo "$out"; exit 1; }
grep -q '^\[mcp_servers.library-index\]' <<<"$out" || { echo "check-register: codex block missing"; exit 1; }
# Idempotence: an existing table is reported, not duplicated.
printf '[mcp_servers.library-index]\ncommand = "old"\n' >>"$H/.codex/config.toml"
out2="$(HOME="$H" bash "$ROOT/scripts/register-mcp.sh" --dry-run --bin "$BIN" 2>&1)" || exit 1
grep -q 'already present' <<<"$out2" || { echo "check-register: idempotence not reported"; exit 1; }
# Relative --bin/--data-dir are made absolute (US-7.2).
out3="$(cd "$ROOT" && HOME="$H" bash scripts/register-mcp.sh --dry-run --bin _build/default/bin/main.exe --data-dir rel-data 2>&1)" || { echo "$out3"; exit 1; }
grep -qF -- "-- $BIN mcp" <<<"$out3" || { echo "check-register: --bin not made absolute"; exit 1; }
grep -qF "LIBRARY_INDEX_DATA = \"$ROOT/rel-data\"" <<<"$out3" || { echo "check-register: --data-dir not made absolute"; exit 1; }
# TOML escaping of a double quote and a backslash.
out4="$(HOME="$H" bash "$ROOT/scripts/register-mcp.sh" --dry-run --bin "$BIN" --data-dir "$H/a\"b\\c" 2>&1)" || { echo "$out4"; exit 1; }
grep -qF "LIBRARY_INDEX_DATA = \"$H/a\\\"b\\\\c\" }" <<<"$out4" || { echo "check-register: TOML value not escaped"; echo "$out4"; exit 1; }
# C-29: --dry-run --install with a missing binary passes only when the build output exists.
HOME="$H" bash "$ROOT/scripts/register-mcp.sh" --dry-run --install --bin "$H/nope" >/dev/null 2>&1 || { echo "check-register: --dry-run --install refused although the build output exists"; exit 1; }
mkdir -p "$H/repo/scripts" && cp "$ROOT/scripts/register-mcp.sh" "$H/repo/scripts/"
if HOME="$H" bash "$H/repo/scripts/register-mcp.sh" --dry-run --install --bin "$H/nope" >/dev/null 2>&1; then
  echo "check-register: --dry-run --install accepted a missing binary with no build output"; exit 1
fi
echo "check-register: ok"
