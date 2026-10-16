#!/usr/bin/env node
// Ratchet check for review finding tool/lib/register.ml:389:security (spec FR-104, AM-17,
// FR-071; CHECK-39).
//
// Re-registering an existing source used to rewrite its whole [[source]] block from the
// call's arguments, with defaults for every absent flag: campaign_prefix and
// confidential were silently dropped (with the block's comments) and the immediate fetch
// stored the campaign repositories' README and description.
//
// Scenario (temp dir; fake gh lists the org acme-campaigns with one repository whose
// description and README carry a sentinel; temp HOME; no network): sources.toml holds
// that org with confidential = "generic", campaign_prefix = true and a comment; init;
// `register-source github-org acme-campaigns` (no flag).
// Asserts: the entry keeps campaign_prefix, confidential and its comment; the row is
// campaign and the sentinel appears nowhere in `get`.
//
// Exit: 0 = fixed, 1 = bug present (assertion fired), >= 2 = setup error.
// Binary: $LIBRARY_INDEX_BIN, else built from the nearest dune project (switch from
// $LIBRARY_INDEX_SWITCH or the nearest `_opam`), so the check also runs on a pre-fix copy.
"use strict";

const fs = require("fs");
const os = require("os");
const path = require("path");
const { spawnSync } = require("child_process");

class SetupError extends Error {}
class AssertionFailed extends Error {}
const setup = (msg) => {
  throw new SetupError(msg);
};
const assert = (cond, msg) => {
  if (!cond) throw new AssertionFailed(msg);
};

function projectRoot() {
  // The nearest ancestor of this file holding dune-project + bin/main.ml: the current
  // tree (tool/checks/) or a pre-fix scratch copy.
  let d = path.resolve(__dirname, "..");
  for (let i = 0; i < 4; i++) {
    if (fs.existsSync(path.join(d, "dune-project")) && fs.existsSync(path.join(d, "bin", "main.ml")))
      return d;
    d = path.dirname(d);
  }
  return setup("no dune project with bin/main.ml above the check");
}

function opamSwitch(root) {
  if (process.env.LIBRARY_INDEX_SWITCH) return process.env.LIBRARY_INDEX_SWITCH;
  let d = root;
  for (let i = 0; i < 4; i++) {
    if (fs.existsSync(path.join(d, "_opam"))) return d;
    d = path.dirname(d);
  }
  return null;
}

// `dune <args>` in the project's opam switch.
function dune(root, args, opts) {
  const sw = opamSwitch(root);
  const [cmd, argv] = sw
    ? ["opam", ["exec", `--switch=${sw}`, "--", "dune", ...args]]
    : ["dune", args];
  return spawnSync(cmd, argv, Object.assign({ cwd: root, encoding: "utf8" }, opts));
}

function binary() {
  const env = process.env.LIBRARY_INDEX_BIN;
  if (env) {
    if (!fs.existsSync(env)) setup(`LIBRARY_INDEX_BIN not found: ${env}`);
    return path.resolve(env);
  }
  const root = projectRoot();
  const r = dune(root, ["build", "--root", root, "./bin/main.exe"]);
  if (r.error || r.status !== 0)
    setup(`dune build failed: ${r.error ? r.error.message : r.stderr}`);
  return path.join(root, "_build/default/bin/main.exe");
}

function run(cmd, args, opts) {
  const r = spawnSync(cmd, args, Object.assign({ encoding: "utf8" }, opts));
  if (r.error) setup(`cannot run ${cmd}: ${r.error.message}`);
  return r;
}

function must(cmd, args, opts, ok) {
  const r = run(cmd, args, opts);
  if (!(ok || [0]).includes(r.status))
    setup(`${cmd} ${args[0]} exited ${r.status}: ${r.stderr}`);
  return r;
}

function write(p, text, mode) {
  fs.mkdirSync(path.dirname(p), { recursive: true });
  fs.writeFileSync(p, text, mode ? { mode } : undefined);
}

function parse(out, what) {
  try {
    return JSON.parse(out);
  } catch (e) {
    return setup(`${what} output is not JSON: ${e.message}`);
  }
}

const NAME = "register-source-keeps-flags";
const SENTINEL = "ZQX-SOURCE-FLAGS-SENTINEL-8e21";
const KEY = "github:acme-campaigns/target";

// Fake gh: `gh api -X GET <path> [--paginate]`.
const GH = `#!/bin/sh
p="$4"
repo='{"full_name":"acme-campaigns/target","owner":{"login":"acme-campaigns"},"name":"target","html_url":"https://github.com/acme-campaigns/target","private":false,"fork":false,"archived":false,"language":"OCaml","description":"${SENTINEL} description","default_branch":"main","pushed_at":"2026-10-01T00:00:00Z","updated_at":"2026-10-01T00:00:00Z"}'
case "$p" in
  orgs/acme-campaigns) echo '{"login":"acme-campaigns"}' ;;
  orgs/acme-campaigns/repos*) echo "[$repo]" ;;
  repos/acme-campaigns/target) echo "$repo" ;;
  repos/acme-campaigns/target/readme) printf '# target\\n\\n${SENTINEL} exploit notes.\\n' ;;
  *) echo 'gh: Not Found (HTTP 404)' >&2; exit 1 ;;
esac
`;

const CONFIG =
  '[owner]\nauthor_regex = "example"\n\n' +
  "[[source]]\n" +
  'kind = "github-org"\n' +
  'name = "acme-campaigns"\n' +
  "# owner note: confidential campaign organisation\n" +
  'confidential = "generic"\n' +
  "campaign_prefix = true\n";

function body(tmp) {
  const bin = binary();
  const home = path.join(tmp, "home");
  const fake = path.join(tmp, "fakebin");
  const data = path.join(tmp, "data");
  for (const d of [home, data, fake]) fs.mkdirSync(d, { recursive: true });
  write(path.join(fake, "gh"), GH, 0o755);
  write(path.join(fake, "glab"), "#!/bin/sh\necho '[]'\n", 0o755);
  write(path.join(data, "curated.json"), "{}\n");
  write(path.join(data, "whitehat_tools.json"), '{"tools": []}\n');
  write(path.join(data, "sources.toml"), CONFIG);
  const env = {
    PATH: `${fake}:${process.env.PATH}`,
    HOME: home,
    LANG: "C.UTF-8",
    LIBRARY_INDEX_DATA: data,
  };
  const li = (args, ok) => must(bin, args, { env }, ok);
  li(["init"]);
  li(["register-source", "github-org", "acme-campaigns"], [0, 2]);
  const text = fs.readFileSync(path.join(data, "sources.toml"), "utf8");
  const got = run(bin, ["get", KEY], { env });

  assert(/campaign_prefix\s*=\s*true/.test(text), "re-registration dropped campaign_prefix");
  assert(/confidential\s*=\s*"generic"/.test(text), "re-registration dropped confidential");
  assert(text.includes("# owner note"), "re-registration dropped the entry's comment");
  if (got.status !== 0) setup(`the listed repository was not indexed: ${got.stderr}`);
  assert(!got.stdout.includes(SENTINEL), "`get` returns the campaign repository's text");
  const row = parse(got.stdout, "get");
  assert(row.campaign === true, `row ${KEY} is not flagged campaign`);
}

function main() {
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "li-check-"));
  let code = 0;
  try {
    body(tmp);
    console.log(`${NAME}: ok`);
  } catch (e) {
    if (e instanceof AssertionFailed) {
      console.error(`${NAME}: FAIL: ${e.message}`);
      code = 1;
    } else {
      console.error(`${NAME}: setup error: ${e.message}`);
      code = 2;
    }
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
  process.exit(code);
}

main();
