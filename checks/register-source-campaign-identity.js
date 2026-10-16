#!/usr/bin/env node
// Ratchet check for review finding tool/lib/register.ml:397:security (spec AM-17,
// FR-071, FR-085, FR-104; CHECK-38).
//
// `register-source local-root <dir>` used to list the directory with an identity holding
// only the new source, so the campaign prefixes declared on the other [[source]] entries
// were lost: a checkout of a campaign remote became a plain `local:` row whose README,
// synopsis and documents were stored, rendered and served.
//
// Scenario (temp dir; fake gh/glab answer 404 to everything, so the campaign remote is
// unreadable; temp HOME; no network): sources.toml declares the GitLab group
// acme-campaigns with campaign_prefix = true; a git checkout dev/secret-campaign (one
// commit by "Ada Example", origin git@gitlab.com:acme-campaigns/secret-campaign.git,
// README.md with a sentinel); init, then `register-source local-root <dev>`, then render.
// Asserts: the row is flagged campaign with no README/synopsis, and the sentinel appears
// nowhere in `get`, `docs`, INDEX.md or the notes page.
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

const NAME = "register-source-campaign-identity";
const SENTINEL = "ZQX-CAMPAIGN-SENTINEL-4b19d2";
const KEY = "local:secret-campaign";

function checkout(dir, env) {
  write(path.join(dir, "README.md"), `# Secret campaign\n\nThis checkout holds ${SENTINEL} exploit notes.\n`);
  const git = (...args) => must("git", ["-C", dir, ...args], { env });
  git("init", "-q");
  git("add", "README.md");
  git("-c", "user.name=Ada Example", "-c", "user.email=ada@example.org",
    "-c", "commit.gpgsign=false", "commit", "-q", "-m", "init");
  git("remote", "add", "origin", "git@gitlab.com:acme-campaigns/secret-campaign.git");
}

function body(tmp) {
  const bin = binary();
  const home = path.join(tmp, "home");
  const dev = path.join(tmp, "dev");
  const fake = path.join(tmp, "fakebin");
  const data = path.join(tmp, "data");
  const notes = path.join(tmp, "notes");
  for (const d of [home, dev, data, notes, fake]) fs.mkdirSync(d, { recursive: true });
  const notFound = '#!/bin/sh\necho "gh: Not Found (HTTP 404)" >&2\nexit 1\n';
  for (const n of ["gh", "glab"]) write(path.join(fake, n), notFound, 0o755);
  const env = {
    PATH: `${fake}:${process.env.PATH}`,
    HOME: home,
    GIT_CONFIG_NOSYSTEM: "1",
    LANG: "C.UTF-8",
    LIBRARY_INDEX_DATA: data,
  };
  checkout(path.join(dev, "secret-campaign"), env);
  write(path.join(data, "curated.json"), "{}\n");
  write(path.join(data, "whitehat_tools.json"), '{"tools": []}\n');
  write(
    path.join(data, "sources.toml"),
    '[owner]\nauthor_regex = "example"\n\n' +
      '[[source]]\nkind = "gitlab-group"\nname = "acme-campaigns"\ncampaign_prefix = true\n'
  );
  const li = (args, ok) => must(bin, args, { env }, ok);
  li(["init"]);
  li(["register-source", "local-root", dev], [0, 2]);
  const got = li(["get", KEY]).stdout;
  const docs = run(bin, ["docs", KEY, "exploit"], { env });
  const out = path.join(tmp, "INDEX.md");
  li(["render", "--out", out, "--notes-dir", notes, "--now", "2026-10-04T12:00:00Z"], [0, 2]);
  const index = fs.readFileSync(out, "utf8");
  const html = fs
    .readdirSync(notes)
    .filter((n) => n.endsWith(".html"))
    .map((n) => fs.readFileSync(path.join(notes, n), "utf8"))
    .join("\n");

  const row = parse(got, "get");
  assert(row.campaign === true, `row ${KEY} is not flagged campaign`);
  assert(row.readme == null && row.synopsis == null, `row ${KEY} keeps README/synopsis`);
  assert(!got.includes(SENTINEL), "`get` returns the campaign checkout's README text");
  assert(!`${docs.stdout}${docs.stderr}`.includes(SENTINEL), "`docs` serves the campaign README");
  assert(!index.includes(SENTINEL), "INDEX.md contains the campaign checkout's README text");
  assert(!html.includes(SENTINEL), "the notes page contains the campaign checkout's README text");
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
