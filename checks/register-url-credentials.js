#!/usr/bin/env node
// Ratchet check for review finding tool/lib/target.ml:32:security (spec C-32, FR-070,
// FR-104; CHECK-41).
//
// A register target URL carrying credentials (https://user:TOKEN@github.com/o/r, the
// usual personal-access-token remote form) used to be written verbatim into sources.toml
// and echoed by `sources list` / list-sources.
//
// Scenario (temp dir; fake gh knows example-user/lib; temp HOME; no network): init;
// `register https://x-access-token:<token>@github.com/example-user/lib`; `sources list`;
// `unregister` the same URL.
// Asserts: the token is in neither sources.toml, the register output nor `sources list`;
// the stored target is the canonical URL; unregistering by the token URL still works.
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

const NAME = "register-url-credentials";
// A synthetic token in GitHub's personal-access-token shape (not a real credential).
const TOKEN = "ghp_ZqxSyntheticToken0123456789abcdefABCD";
const URL = `https://x-access-token:${TOKEN}@github.com/example-user/lib`;

const GH = `#!/bin/sh
p="$4"
repo='{"full_name":"example-user/lib","owner":{"login":"example-user"},"name":"lib","html_url":"https://github.com/example-user/lib","private":false,"fork":false,"archived":false,"language":"OCaml","description":"A lib","default_branch":"main","pushed_at":"2026-10-01T00:00:00Z","updated_at":"2026-10-01T00:00:00Z"}'
case "$p" in
  repos/example-user/lib) echo "$repo" ;;
  *) echo 'gh: Not Found (HTTP 404)' >&2; exit 1 ;;
esac
`;

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
  write(path.join(data, "sources.toml"), '[owner]\nauthor_regex = "example"\n');
  const env = {
    PATH: `${fake}:${process.env.PATH}`,
    HOME: home,
    LANG: "C.UTF-8",
    LIBRARY_INDEX_DATA: data,
  };
  const li = (args, ok) => must(bin, args, { env }, ok);
  li(["init"]);
  const reg = li(["register", URL], [0, 2]);
  const text = fs.readFileSync(path.join(data, "sources.toml"), "utf8");
  const listed = li(["sources", "list"]).stdout;
  const un = run(bin, ["unregister", URL], { env });

  assert(!text.includes(TOKEN), "sources.toml holds the token");
  assert(!`${reg.stdout}${reg.stderr}`.includes(TOKEN), "register prints the token");
  assert(!listed.includes(TOKEN), "`sources list` prints the token");
  assert(
    text.includes('target = "https://github.com/example-user/lib"'),
    "the stored target is not the canonical URL"
  );
  assert(un.status === 0, `unregister by the token URL failed: ${un.stderr}`);
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
