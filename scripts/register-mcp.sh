#!/usr/bin/env bash
# Register the library-index MCP server (stdio) in Claude Code (user scope) and Codex.
#
#   scripts/register-mcp.sh [--dry-run] [--install] [--bin PATH] [--data-dir DIR]
#
#   --install       build and copy the binary to --bin first (dune build in this tree's
#                   opam switch: OPAMSWITCH or `opam env`)
#   --bin PATH      server binary (default: $HOME/.local/bin/library-index)
#   --data-dir DIR  data directory: sources.toml, library-index.db, the curated exports
#                   (default: $XDG_DATA_HOME/library-index, else
#                   $HOME/.local/share/library-index)
#   --dry-run       print what would be done; modify nothing (still refuses a missing
#                   binary; with --install, the build output must exist instead)
#
# The registrations set LIBRARY_INDEX_DATA to the data directory; the database lives in
# it. --bin and --data-dir are made absolute (MCP clients resolve relative paths against
# their own working directory) and are TOML-escaped in the Codex block.
#
# Idempotent (spec C-29): an existing `library-index` server in Claude Code, or an existing
# [mcp_servers.library-index] table in ~/.codex/config.toml, is left untouched and the
# expected configuration is printed instead. Never writes credentials; the server needs
# none (gh/glab handle their own auth for `refresh`).
# Exit: 0 ok, 1 usage/missing binary/registration failure.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DRY=0 INSTALL=0
BIN="$HOME/.local/bin/library-index"
DATA="${XDG_DATA_HOME:-$HOME/.local/share}/library-index"
while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run) DRY=1 ;;
    --install) INSTALL=1 ;;
    --bin) BIN="$2"; shift ;;
    --data-dir) DATA="$2"; shift ;;
    -h|--help) sed -n '2,20p' "$0"; exit 0 ;;
    *) echo "register-mcp: unknown argument: $1" >&2; exit 1 ;;
  esac
  shift
done

BIN="$(realpath -m -- "$BIN")"
DATA="$(realpath -m -- "$DATA")"
BUILT="$ROOT/_build/default/bin/main.exe"

if [ "$INSTALL" = 1 ]; then
  if [ "$DRY" = 1 ]; then
    if [ ! -x "$BIN" ] && [ ! -x "$BUILT" ]; then
      echo "register-mcp: binary not found: neither $BIN nor the build output $BUILT exists (run dune build first)" >&2
      exit 1
    fi
    echo "[dry-run] would build ($ROOT) and install $BUILT to $BIN"
  else
    (cd "$ROOT" && eval "$(opam env --set-switch)" && dune build bin/main.exe)
    mkdir -p "$(dirname "$BIN")"
    install -m 0755 "$BUILT" "$BIN"
    echo "installed $BIN"
  fi
fi

if [ ! -x "$BIN" ] && ! { [ "$DRY" = 1 ] && [ "$INSTALL" = 1 ]; }; then
  echo "register-mcp: binary not found or not executable: $BIN (run with --install)" >&2
  exit 1
fi

# TOML basic string: escape backslash first, then double quote; refuse control
# characters (newline, tab, ...), which would need \uXXXX escapes.
toml_str() {
  case "$1" in
    *[[:cntrl:]]*) echo "register-mcp: control character in a path; refusing to write TOML" >&2; exit 1 ;;
  esac
  local v="${1//\\/\\\\}"
  printf '"%s"' "${v//\"/\\\"}"
}
BIN_TOML="$(toml_str "$BIN")" || exit 1
DATA_TOML="$(toml_str "$DATA")" || exit 1

CODEX_CFG="$HOME/.codex/config.toml"
codex_block() {
  cat <<EOF
[mcp_servers.library-index]
command = $BIN_TOML
args = ["mcp"]
env = { LIBRARY_INDEX_DATA = $DATA_TOML }
EOF
}

# --- Claude Code (user scope) -------------------------------------------------------
claude_cmd=(claude mcp add --transport stdio --env "LIBRARY_INDEX_DATA=$DATA" --scope user
  library-index -- "$BIN" mcp)
shown="$(printf '%q ' "${claude_cmd[@]}")"
shown="${shown% }"
if [ "$DRY" = 1 ]; then
  echo "[dry-run] Claude Code: $shown"
elif ! command -v claude >/dev/null 2>&1; then
  echo "Claude Code: 'claude' not on PATH; run manually: $shown"
elif claude mcp get library-index >/dev/null 2>&1; then
  echo "Claude Code: 'library-index' already registered; left unchanged (expected: $shown)"
else
  "${claude_cmd[@]}"
  echo "Claude Code: registered library-index (user scope)"
fi

# --- Codex ----------------------------------------------------------------------------
if [ -f "$CODEX_CFG" ] && grep -q '^\[mcp_servers\.library-index\]' "$CODEX_CFG"; then
  echo "Codex: [mcp_servers.library-index] already present in $CODEX_CFG; left unchanged. Expected:"
  codex_block
elif [ "$DRY" = 1 ]; then
  echo "[dry-run] Codex: would append to $CODEX_CFG:"
  codex_block
else
  mkdir -p "$(dirname "$CODEX_CFG")"
  { echo; codex_block; } >>"$CODEX_CFG"
  echo "Codex: appended [mcp_servers.library-index] to $CODEX_CFG"
fi
