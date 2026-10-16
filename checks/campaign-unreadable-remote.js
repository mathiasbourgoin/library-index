#!/usr/bin/env node
// Ratchet check for review finding lib/sources.ml:326:security (spec AM-17, FR-051,
// FR-071, US-8.2).
//
// A local checkout whose origin is a campaign remote (gitlab:acme-campaigns/...) that
// the API cannot read used to become a plain `local:` row: its on-disk README was stored
// and its README-derived synopsis was rendered into INDEX.md and the notes page.
//
// Scenario (all in a temp dir; fake gh/glab on PATH answer 404 to everything, so the
// campaign remote is unreadable; HOME and --dev-root are temp; no network):
//   a git checkout dev/secret-campaign, one commit authored "Ada Example", origin
//   git@gitlab.com:acme-campaigns/secret-campaign.git, README.md carrying a sentinel;
//   init + refresh --dev-root + render --out --notes-dir.
// Asserts: the sentinel appears nowhere in `get`, INDEX.md or the notes HTML, and the row
// is flagged campaign with the withheld description.
//
// A binary from before sources.toml (no --data-dir, e.g. the pre-fix commit 1c231af)
// hard-codes the owner's identity: the check then reads the campaign group and author
// from $LIBRARY_INDEX_LEGACY_FIXTURE (private migration/legacy-fixture.sh; exit 2 without it).
//
// Exit: 0 = fixed, 1 = bug present (assertion fired), >= 2 = setup error.
// Binary: $LIBRARY_INDEX_BIN, else built from the nearest dune project (switch from
// $LIBRARY_INDEX_SWITCH or the nearest `_opam`), so the check also runs on a pre-fix copy.
"use strict";

const fs = require("fs");
const os = require("os");
const path = require("path");
const { spawnSync } = require("child_process");

const SENTINEL = "ZQX-CAMPAIGN-SENTINEL-7f3a91";
const WITHHELD = "security-research campaign repo (details withheld)";
const KEY = "local:secret-campaign";

class SetupError extends Error {}
class AssertionFailed extends Error {}
const setup = (msg) => {
  throw new SetupError(msg);
};
const assert = (cond, msg) => {
  if (!cond) throw new AssertionFailed(msg);
};

function projectRoot() {
  // The check may run from the current tree (tool/checks/) or from a pre-fix scratch copy
  // with the older root layout: take the nearest ancestor holding dune-project + bin/main.ml.
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

function binary() {
  const env = process.env.LIBRARY_INDEX_BIN;
  if (env) {
    if (!fs.existsSync(env)) setup(`LIBRARY_INDEX_BIN not found: ${env}`);
    return path.resolve(env);
  }
  const root = projectRoot();
  const sw = opamSwitch(root);
  const [cmd, args] = sw
    ? ["opam", ["exec", `--switch=${sw}`, "--", "dune", "build", "--root", root, "./bin/main.exe"]]
    : ["dune", ["build", "--root", root, "./bin/main.exe"]];
  const r = spawnSync(cmd, args, { cwd: root, encoding: "utf8" });
  if (r.error || r.status !== 0)
    setup(`dune build failed: ${r.error ? r.error.message : r.stderr}`);
  return path.join(root, "_build/default/bin/main.exe");
}

// A binary from before sources.toml (no --data-dir) hard-codes the owner's identity: the
// scenario then needs that identity, read at run time from the JSON file named by
// $LIBRARY_INDEX_LEGACY_FIXTURE (written by the private migration/legacy-fixture.sh), so
// that no real name lives in this file. [null] for a config-driven binary.
function legacyIdentity(bin) {
  const help = run(bin, ["init", "--help=plain"], { env: { PATH: process.env.PATH, LANG: "C.UTF-8" } });
  if (`${help.stdout}`.includes("--data-dir")) return null;
  const f = process.env.LIBRARY_INDEX_LEGACY_FIXTURE;
  if (!f)
    setup(
      "this binary predates sources.toml (no --data-dir): set LIBRARY_INDEX_LEGACY_FIXTURE " +
        "to the file migration/legacy-fixture.sh writes"
    );
  let fx;
  try {
    fx = JSON.parse(fs.readFileSync(f, "utf8"));
  } catch (e) {
    setup(`unreadable LIBRARY_INDEX_LEGACY_FIXTURE: ${e.message}`);
  }
  for (const k of ["gh_user", "campaign_group", "author"])
    if (typeof fx[k] !== "string" || fx[k] === "") setup(`legacy fixture lacks ${k}`);
  return fx;
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

function fakeForge(dir) {
  fs.mkdirSync(dir, { recursive: true });
  // Every API path is unreadable: what a private subgroup without access looks like.
  const body = '#!/bin/sh\necho "glab: 404 Not Found (HTTP 404)" >&2\nexit 1\n';
  for (const name of ["gh", "glab"])
    fs.writeFileSync(path.join(dir, name), body, { mode: 0o755 });
}

function checkout(dir, env, { group, author }) {
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(
    path.join(dir, "README.md"),
    `# Secret campaign\n\nThis checkout holds ${SENTINEL} confidential-protocol exploit notes for the target.\n`
  );
  const git = (...args) => must("git", ["-C", dir, ...args], { env });
  git("init", "-q");
  git("add", "README.md");
  git(
    "-c", `user.name=${author}`, "-c", "user.email=ada@example.org",
    "-c", "commit.gpgsign=false", "commit", "-q", "-m", "init"
  );
  git("remote", "add", "origin", `git@gitlab.com:${group}/secret-campaign.git`);
}

function body(tmp) {
  const bin = binary();
  const legacy = legacyIdentity(bin);
  const who = legacy
    ? { group: legacy.campaign_group, author: legacy.author }
    : { group: "acme-campaigns", author: "Ada Example" };
  const home = path.join(tmp, "home");
  const dev = path.join(tmp, "dev");
  const fake = path.join(tmp, "fakebin");
  const data = path.join(tmp, "data");
  const notes = path.join(tmp, "notes");
  for (const d of [home, dev, data, notes]) fs.mkdirSync(d, { recursive: true });
  fakeForge(fake);
  const env = {
    PATH: `${fake}:${process.env.PATH}`,
    HOME: home,
    GIT_CONFIG_NOSYSTEM: "1",
    LANG: "C.UTF-8",
  };
  checkout(path.join(dev, "secret-campaign"), env, who);
  fs.writeFileSync(path.join(data, "curated.json"), "{}\n");
  fs.writeFileSync(path.join(data, "whitehat_tools.json"), '{"tools": []}\n');
  // The data dir's sources.toml: the owner's commit author and the campaign prefix
  // (synthetic names).
  fs.writeFileSync(
    path.join(data, "sources.toml"),
    '[owner]\nauthor_regex = "example"\n\n' +
      '[[source]]\nkind = "gitlab-group"\nname = "acme-campaigns"\ncampaign_prefix = true\n'
  );
  const db = path.join(data, "t.db");
  const li = (args, ok) => must(bin, args, { env }, ok);
  li(["init", "--db", db, "--from", data]);
  li(["refresh", "--db", db, "--dev-root", dev], [0, 2]);
  const got = li(["get", "--db", db, KEY]).stdout;
  const out = path.join(tmp, "INDEX.md");
  li(
    ["render", "--db", db, "--out", out, "--notes-dir", notes, "--now", "2026-10-04T12:00:00Z"],
    [0, 2]
  );
  const index = fs.readFileSync(out, "utf8");
  const pages = fs.readdirSync(notes).filter((n) => n.endsWith("-library-index.html"));
  if (pages.length !== 1) setup(`expected one notes page, found ${pages.length}`);
  const html = fs.readFileSync(path.join(notes, pages[0]), "utf8");

  assert(!got.includes(SENTINEL), "`get` returns the campaign checkout's README text");
  assert(!index.includes(SENTINEL), "INDEX.md contains the campaign checkout's README text");
  assert(!html.includes(SENTINEL), "the notes page contains the campaign checkout's README text");
  let row;
  try {
    row = JSON.parse(got);
  } catch (e) {
    setup(`get output is not JSON: ${e.message}`);
  }
  assert(row.campaign === true, `row ${KEY} is not flagged campaign`);
  assert(row.description === WITHHELD, `row ${KEY} description is not withheld`);
  assert(row.readme == null && row.synopsis == null, `row ${KEY} keeps README/synopsis`);
}

function main() {
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "li-check-campaign-"));
  let code = 0;
  try {
    body(tmp);
    console.log("campaign-unreadable-remote: ok");
  } catch (e) {
    if (e instanceof AssertionFailed) {
      console.error(`campaign-unreadable-remote: FAIL: ${e.message}`);
      code = 1;
    } else {
      console.error(`campaign-unreadable-remote: setup error: ${e.message}`);
      code = 2;
    }
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
  process.exit(code);
}

main();
