#!/usr/bin/env node
// Ratchet check for review finding tool/lib/unregister.ml:88:correctness (spec FR-105,
// FR-002, US-1; CHECK-40).
//
// unregister used to delete any row whose stored `sources` list was empty, including a
// row that existed before the registration (imported from curated.json): its curated
// fields, usage and curated.json entry were destroyed.
//
// Scenario (temp dir; fake gh knows example-user/lib; temp HOME; no network):
// curated.json holds github:example-user/lib with what, interest, notes and a consumer;
// init; `register https://github.com/example-user/lib`; `unregister` the same URL.
// Asserts: the row is reported kept, `get` still returns it with its notes, and the
// rewritten curated.json still holds the notes and the consumer.
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

const NAME = "unregister-keeps-curated";
const KEY = "github:example-user/lib";
const URL = "https://github.com/example-user/lib";
const NOTES = "precious notes ZQX-CURATED-5c0e";

const GH = `#!/bin/sh
p="$4"
repo='{"full_name":"example-user/lib","owner":{"login":"example-user"},"name":"lib","html_url":"https://github.com/example-user/lib","private":false,"fork":false,"archived":false,"language":"OCaml","description":"A lib","default_branch":"main","pushed_at":"2026-10-01T00:00:00Z","updated_at":"2026-10-01T00:00:00Z"}'
case "$p" in
  repos/example-user/lib) echo "$repo" ;;
  repos/example-user/lib/readme) printf '# lib\\n\\nA lib.\\n' ;;
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
  write(
    path.join(data, "curated.json"),
    JSON.stringify({
      [KEY]: { what: "A lib", interest: "high", notes: NOTES, consumers: ["app"] },
    }) + "\n"
  );
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
  li(["register", URL], [0, 2]);
  const u = parse(li(["unregister", URL]).stdout, "unregister");
  const got = run(bin, ["get", KEY], { env });
  const curated = fs.readFileSync(path.join(data, "curated.json"), "utf8");

  assert(u.row === "kept", `unregister reported row "${u.row}", expected "kept"`);
  assert(got.status === 0, `the curated row is gone (get exited ${got.status})`);
  assert(got.stdout.includes(NOTES), "the row lost its curated notes");
  assert(curated.includes(NOTES), "curated.json lost the notes");
  assert(curated.includes('"app"'), "curated.json lost the consumer");
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
