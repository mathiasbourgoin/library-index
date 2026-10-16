#!/usr/bin/env node
// CHECK-45 (spec "Minimal-freeze", INV-M, FR-130, AC-45): concurrent first opens of an
// older-schema database.
//
// Builds a schema-4 database with the binary (`init`, one curated row), turns it into the
// schema-3 shape the in-place migration starts from (the column v4 added is dropped and
// meta.schema_version set to 3, through node:sqlite), then starts 50 `list` processes at
// once, in three rounds from a fresh v3 copy each: (1) a write lock held by the check for
// 1.5 s while the 50 processes start, so that every one of them reads version 3 before any
// can migrate (the race of review finding tool/lib/db.ml:137); (2) the lock held for 7 s,
// longer than the busy timeout, so that every open meets contention; (3) no lock.
// Asserts: each open exits 0, or exits 1 with a "database busy ... retry" message; none
// advises `init --force` (or calls the file "not a library-index database"); after the
// round (and one more open) the database is schema 4, `PRAGMA integrity_check` is ok, the
// pre-existing row counts as not created by a registration and its curated value is
// intact.
//
// Exit: 0 = pass, 1 = assertion fired, >= 2 = setup error. Binary lookup as CHECK-43.
"use strict";

// node:sqlite prints an ExperimentalWarning on some node versions; it is not a finding.
process.removeAllListeners("warning");

const fs = require("fs");
const os = require("os");
const path = require("path");
const { spawn, spawnSync } = require("child_process");

class SetupError extends Error {}
class AssertionFailed extends Error {}
const setup = (msg) => {
  throw new SetupError(msg);
};
const assert = (cond, msg) => {
  if (!cond) throw new AssertionFailed(msg);
};

function projectRoot() {
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
  const args = ["build", "--root", root, "./bin/main.exe"];
  const [cmd, argv] = sw ? ["opam", ["exec", `--switch=${sw}`, "--", "dune", ...args]] : ["dune", args];
  const r = spawnSync(cmd, argv, { cwd: root, encoding: "utf8" });
  if (r.error || r.status !== 0) setup(`dune build failed: ${r.error ? r.error.message : r.stderr}`);
  return path.join(root, "_build/default/bin/main.exe");
}

function sqlite() {
  try {
    return require("node:sqlite");
  } catch (e) {
    return setup(`node:sqlite unavailable (${e.message}); node >= 22.5 is required`);
  }
}

function runAsync(cmd, args, env) {
  return new Promise((resolve) => {
    const child = spawn(cmd, args, { env, stdio: ["ignore", "pipe", "pipe"] });
    let out = "";
    let err = "";
    child.stdout.on("data", (d) => (out += d));
    child.stderr.on("data", (d) => (err += d));
    child.on("error", (e) => resolve({ code: 127, out, err: e.message }));
    child.on("close", (code, signal) => resolve({ code, signal, out, err }));
  });
}

const N = 50;
const KEY = "local:old-lib";

function makeV3(bin, data, env) {
  fs.mkdirSync(data, { recursive: true });
  fs.writeFileSync(path.join(data, "sources.toml"), '[owner]\nauthor_regex = "example"\n');
  fs.writeFileSync(
    path.join(data, "curated.json"),
    JSON.stringify({ [KEY]: { notes: "kept across the migration" } }) + "\n"
  );
  fs.writeFileSync(path.join(data, "whitehat_tools.json"), '{"tools": []}\n');
  const r = spawnSync(bin, ["init"], { env, encoding: "utf8" });
  if (r.status !== 0) setup(`init failed: ${r.stderr}`);
  const { DatabaseSync } = sqlite();
  const db = new DatabaseSync(path.join(data, "library-index.db"));
  try {
    db.exec("ALTER TABLE repos DROP COLUMN created_by_registration");
    db.exec("UPDATE meta SET v = '3' WHERE k = 'schema_version'");
    db.exec("PRAGMA wal_checkpoint(TRUNCATE)");
  } catch (e) {
    setup(`cannot build the schema-3 database: ${e.message}`);
  } finally {
    db.close();
  }
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function round(bin, tmp, n, holdMs) {
  const data = path.join(tmp, `data-${n}`);
  const env = { PATH: "/usr/bin:/bin", HOME: tmp, LANG: "C.UTF-8", LIBRARY_INDEX_DATA: data };
  makeV3(bin, data, env);
  let lock = null;
  if (holdMs > 0) {
    const { DatabaseSync } = sqlite();
    lock = new DatabaseSync(path.join(data, "library-index.db"));
    lock.exec("BEGIN IMMEDIATE");
  }
  const running = Promise.all(Array.from({ length: N }, () => runAsync(bin, ["list", "--limit", "5"], env)));
  if (lock) {
    await sleep(holdMs);
    lock.exec("ROLLBACK");
    lock.close();
  }
  const results = await running;
  let ok = 0;
  let busy = 0;
  for (const r of results) {
    const msg = `${r.out}\n${r.err}`;
    assert(!/init --force/.test(msg), `round ${n}: a concurrent open advised \`init --force\``);
    assert(!/not a library-index database/.test(msg), `round ${n}: a concurrent open called the database foreign`);
    if (r.code === 0) ok++;
    else if (r.code === 1 && /busy/i.test(r.err) && /retry/i.test(r.err)) busy++;
    else assert(false, `round ${n}: an open exited ${r.code} without the busy/retry message`);
  }
  const after = spawnSync(bin, ["list", "--limit", "5"], { env, encoding: "utf8" });
  assert(after.status === 0, `round ${n}: an open after the race exited ${after.status}: ${after.stderr}`);
  const { DatabaseSync } = sqlite();
  const db = new DatabaseSync(path.join(data, "library-index.db"), { readOnly: true });
  try {
    const v = db.prepare("SELECT v FROM meta WHERE k = 'schema_version'").get();
    assert(v && v.v === "4", `round ${n}: final schema version is ${v && v.v}, expected 4`);
    const ic = db.prepare("PRAGMA integrity_check").get();
    assert(ic && Object.values(ic)[0] === "ok", `round ${n}: integrity_check failed`);
    const row = db.prepare("SELECT notes, created_by_registration AS c FROM repos WHERE key = ?").get(KEY);
    assert(row && row.c === 0, `round ${n}: the pre-existing row is not marked as not created by a registration`);
    assert(row.notes === "kept across the migration", `round ${n}: the curated value changed`);
  } finally {
    db.close();
  }
  return { ok, busy };
}

async function main() {
  const NAME = "migration-concurrency";
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "li-migrate-"));
  let code = 0;
  try {
    const bin = binary();
    const rs = [await round(bin, tmp, 1, 1500), await round(bin, tmp, 2, 7000), await round(bin, tmp, 3, 0)];
    const ok = rs.reduce((a, r) => a + r.ok, 0);
    const busy = rs.reduce((a, r) => a + r.busy, 0);
    console.log(`${NAME}: ok (${N} opens x 3 rounds: ${ok} opened, ${busy} busy/retry)`);
  } catch (e) {
    if (e instanceof AssertionFailed) {
      console.error(`${NAME}: FAIL: ${e.message}`);
      code = 1;
    } else {
      console.error(`${NAME}: setup error: ${e instanceof SetupError ? e.message : e.stack}`);
      code = 2;
    }
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
  process.exit(code);
}

main();
