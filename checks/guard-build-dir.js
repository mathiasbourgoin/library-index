#!/usr/bin/env node
// Ratchet check for review finding tool/test/test_guard.ml:110:correctness (spec FR-100,
// AC-31; CHECK-42).
//
// The publishability guard (CHECK-31) located the code tree from a "/_build/" marker in
// its executable path. Under `dune runtest --build-dir <dir outside _build>` it fell back
// to the build copy, so a relative LIBRARY_INDEX_DATA did not resolve and the guard
// printed "skipped: no data dir" and passed (it failed open); an unresolvable
// LIBRARY_INDEX_DATA was treated the same way.
//
// Scenario (temp dir; no network): a copy of this dune project (without _build/_opam/.git)
// in <tmp>/tool, a data dir <tmp>/data whose denylist.txt holds one synthetic name, and a
// build dir <tmp>/bd outside the copy. From <tmp>/tool, with LIBRARY_INDEX_DATA=../data,
// `dune build --build-dir <tmp>/bd @test/runtest-test_guard --force`:
//   (a) on the clean copy the guard runs and reports "guard: ok";
//   (b) with the name injected into the copy's README.md it fails and names the file;
//   (c) with LIBRARY_INDEX_DATA pointing at a missing directory it fails.
//
// Exit: 0 = fixed, 1 = bug present (assertion fired), >= 2 = setup error.
// Builds in the project's opam switch ($LIBRARY_INDEX_SWITCH or the nearest `_opam`).
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

const NAME = "guard-build-dir";
// Built from parts so that this file itself does not hold the name.
const NAME_TOKEN = ["zqx", "guard", "sentinel"].join("");
const SKIP = new Set(["_build", "_opam", ".git", "node_modules"]);

function copyTree(src, dst) {
  fs.mkdirSync(dst, { recursive: true });
  for (const e of fs.readdirSync(src, { withFileTypes: true })) {
    if (SKIP.has(e.name)) continue;
    const s = path.join(src, e.name);
    const d = path.join(dst, e.name);
    if (e.isDirectory()) copyTree(s, d);
    else if (e.isFile()) fs.copyFileSync(s, d);
  }
}

function guard(copy, bd, data) {
  const env = Object.assign({}, process.env, { LIBRARY_INDEX_DATA: data });
  delete env.DUNE_SOURCEROOT;
  delete env.INSIDE_DUNE;
  const r = dune(copy, ["build", "--root", ".", "--build-dir", bd, "@test/runtest-test_guard", "--force"], {
    cwd: copy,
    env,
  });
  if (r.error) setup(`cannot run dune: ${r.error.message}`);
  return { status: r.status, out: `${r.stdout}${r.stderr}` };
}

function body(tmp) {
  const root = projectRoot();
  if (!fs.existsSync(path.join(root, "test", "test_guard.ml"))) setup("no test/test_guard.ml");
  const copy = path.join(tmp, "tool");
  const data = path.join(tmp, "data");
  const bd = path.join(tmp, "bd");
  copyTree(root, copy);
  write(path.join(data, "denylist.txt"), `# synthetic\n${NAME_TOKEN}\n`);

  const clean = guard(copy, bd, "../data");
  if (clean.status !== 0 && !/guard:/.test(clean.out))
    setup(`the guard test did not build or run: ${clean.out.slice(-2000)}`);
  assert(!/skipped: no data dir/.test(clean.out), "the guard skipped under a custom --build-dir (fails open)");
  assert(clean.status === 0 && /guard: ok/.test(clean.out), `the guard did not pass on the clean copy: ${clean.out.slice(-500)}`);

  fs.appendFileSync(path.join(copy, "README.md"), `\nInjected ${NAME_TOKEN} name.\n`);
  const dirty = guard(copy, bd, "../data");
  assert(dirty.status !== 0, "the guard passed although the copy holds a denylisted name");
  assert(/README\.md/.test(dirty.out), "the guard did not name the offending file");

  fs.writeFileSync(path.join(copy, "README.md"), fs.readFileSync(path.join(root, "README.md")));
  const missing = guard(copy, bd, path.join(tmp, "no-such-data-dir"));
  assert(missing.status !== 0, "a set but missing LIBRARY_INDEX_DATA was not an error");
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
