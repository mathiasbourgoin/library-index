#!/usr/bin/env node
// CHECK-44 (spec "Minimal-freeze", INV-K, FR-129, AC-44): curated no-loss.
//
// A deterministic operation-sequence harness over >= 200 seeded sequences of length <= 8
// (plus the round-3 import-after-dirty-export reproduction and the init --force ones as
// fixed sequences) of: init, import, import --prefer-file, init --force,
// init --force --discard-unexported, annotate (set / clear), annotate while the export
// fails (curated.json replaced by a directory, so the rename fails even as root),
// record-usage, register / unregister of a checkout (with and without curated notes),
// refresh, refresh --repo and export, on a synthetic world (temp HOME, a dev root with a
// checkout, a checkout outside it, fake gh serving one listed repository, temp data dir).
//
// The harness keeps a reference model of every curated value and usage row the sequence
// created. After each step it compares the model with `get` and `explain`, and, when the
// step exported successfully, with curated.json. It asserts:
//   - while the database holds curated data that curated.json lacks, `import` and
//     `init --force` refuse (exit 1, a message naming their override flag), and their
//     overrides `--prefer-file` / `--discard-unexported` succeed with the file's values;
//   - no message recommends `init --force` while unexported curated data exists.
// The only accepted losses are an explicit `annotate` clear and an explicit override.
//
// Exit: 0 = pass, 1 = assertion fired, >= 2 = setup error. Binary lookup as CHECK-43.
// Options (environment): LI_SEQS=N, LI_ONLY=name, LI_JOBS=N, LI_KEEP=1.
"use strict";

const fs = require("fs");
const os = require("os");
const path = require("path");
const crypto = require("crypto");
const { spawn, spawnSync } = require("child_process");

class SetupError extends Error {}
const setup = (msg) => {
  throw new SetupError(msg);
};

// ------------------------------------------------------------------ binary

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

function runAsync(cmd, args, env) {
  return new Promise((resolve) => {
    const child = spawn(cmd, args, { env, stdio: ["ignore", "pipe", "pipe"] });
    const out = [];
    const err = [];
    child.stdout.on("data", (d) => out.push(d));
    child.stderr.on("data", (d) => err.push(d));
    child.on("error", (e) => resolve({ code: 127, signal: null, out: "", err: e.message }));
    child.on("close", (code, signal) =>
      resolve({ code, signal, out: Buffer.concat(out).toString("utf8"), err: Buffer.concat(err).toString("utf8") })
    );
  });
}

function must(cmd, args, opts) {
  const r = spawnSync(cmd, args, Object.assign({ encoding: "utf8" }, opts));
  if (r.error || r.status !== 0)
    setup(`${cmd} ${args.join(" ")} failed: ${r.error ? r.error.message : r.stderr}`);
  return r;
}

function write(p, text, mode) {
  fs.mkdirSync(path.dirname(p), { recursive: true });
  fs.writeFileSync(p, text, mode ? { mode } : undefined);
}

// ------------------------------------------------------------------ the world

const ROWS = {
  alpha: { key: "github:example-user/alpha-lib" },
  gamma: { key: "local:gamma-local", checkout: "dev/gamma-local" },
  rho: { key: "local:rho", checkout: "elsewhere/rho" },
};
const IDS = Object.keys(ROWS);
const FIELDS = ["notes", "interest", "rules", "what"];
const PUSHED = "2026-09-30T10:00:00Z";

const FAKE = `#!/bin/sh
prog=$(basename "$0")
h=$(printf '%s %s' "$prog" "$4" | sha1sum | cut -c1-40)
if [ -f "$LI_FORGE/$h" ]; then
  cat "$LI_FORGE/$h"
  exit 0
fi
echo "$prog: Not Found (HTTP 404)" >&2
exit 1
`;

function forgeTable() {
  const repo = {
    full_name: "example-user/alpha-lib",
    owner: { login: "example-user" },
    name: "alpha-lib",
    html_url: "https://github.com/example-user/alpha-lib",
    private: false,
    fork: false,
    archived: false,
    language: "OCaml",
    description: "A parser library",
    default_branch: "main",
    pushed_at: PUSHED,
    updated_at: PUSHED,
  };
  const base = "repos/example-user/alpha-lib";
  return {
    "gh users/example-user/repos?type=all&per_page=100": JSON.stringify([repo]),
    [`gh ${base}`]: JSON.stringify(repo),
    [`gh ${base}/readme`]: "# alpha-lib\n\nParses things.\n",
    [`gh ${base}/contents/`]: JSON.stringify([{ name: "README.md", path: "README.md", type: "file" }]),
    [`gh ${base}/commits?per_page=1`]: JSON.stringify([{ sha: "0a", commit: { committer: { date: PUSHED } } }]),
    [`gh ${base}/contributors?per_page=100`]: JSON.stringify([{ login: "example-user", contributions: 9 }]),
    [`gh ${base}/git/trees/main?recursive=1`]: JSON.stringify({ truncated: false, tree: [{ path: "README.md", type: "blob", sha: "1a", size: 28 }] }),
    [`gh ${base}/contents/README.md`]: "# alpha-lib\n\nParses things.\n",
  };
}

function buildTemplate(dir) {
  const forge = path.join(dir, "forge");
  fs.mkdirSync(forge, { recursive: true });
  for (const [k, body] of Object.entries(forgeTable()))
    fs.writeFileSync(path.join(forge, crypto.createHash("sha1").update(k).digest("hex")), body);
  const tpl = path.join(dir, "template");
  for (const n of ["gh", "glab"]) write(path.join(tpl, "fake", n), FAKE, 0o755);
  const env = { PATH: process.env.PATH, HOME: path.join(tpl, "home"), GIT_CONFIG_NOSYSTEM: "1" };
  for (const r of Object.values(ROWS).filter((x) => x.checkout)) {
    const d = path.join(tpl, "home", r.checkout);
    write(path.join(d, "README.md"), `# ${path.basename(d)}\n\nA local library.\n`);
    const git = (...args) => must("git", ["-C", d, ...args], { env });
    git("init", "-q", "-b", "main");
    git("add", "-A");
    git("-c", "user.name=Ada Example", "-c", "user.email=ada@example.org",
      "-c", "commit.gpgsign=false", "commit", "-q", "-m", "init");
  }
  write(path.join(tpl, "data", "curated.json"), "{}\n");
  write(path.join(tpl, "data", "whitehat_tools.json"), '{"tools": []}\n');
  return { forge, tpl };
}

// ------------------------------------------------------------------ operations

function opArgv(W, op) {
  const rho = path.join(W, "home", "elsewhere", "rho");
  const key = op.id ? ROWS[op.id].key : null;
  switch (op.name) {
    case "init": return ["init"];
    case "import": return ["import"];
    case "import_prefer": return ["import", "--prefer-file"];
    case "init_force": return ["init", "--force"];
    case "init_discard": return ["init", "--force", "--discard-unexported"];
    case "annotate": case "annotate_fail": return ["annotate", key, "--set", `${op.field}=${op.value}`];
    case "clear": return ["annotate", key, "--clear", op.field];
    case "record_usage": return ["record-usage", key, op.value, "--note", `n-${op.value}`];
    case "register_rho": return ["register", rho];
    case "register_rho_notes": return ["register", rho, "--notes", op.value];
    case "unregister_rho": return ["unregister", rho];
    case "refresh": return ["refresh", "--no-analysis"];
    case "refresh_repo": return ["refresh", "--repo", key];
    case "export": return ["export"];
    default: return setup(`unknown op ${op.name}`);
  }
}

const WEIGHTS = {
  import: 3, import_prefer: 2, init_force: 2, init_discard: 1, annotate: 6, annotate_fail: 5,
  clear: 2, record_usage: 3, register_rho: 2, register_rho_notes: 2, unregister_rho: 2,
  refresh: 2, refresh_repo: 1, export: 2,
};

function mulberry32(a) {
  return function () {
    a |= 0;
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function randomSequence(seed) {
  const rnd = mulberry32(seed * 104729 + 3);
  const names = Object.keys(WEIGHTS);
  const total = names.reduce((s, n) => s + WEIGHTS[n], 0);
  const pick = () => {
    let x = rnd() * total;
    for (const n of names) if ((x -= WEIGHTS[n]) < 0) return n;
    return names[names.length - 1];
  };
  // Every sequence starts from rows that exist: init, then a refresh or a registration.
  const ops = [{ name: "init" }, { name: rnd() < 0.5 ? "refresh" : "register_rho" }];
  const len = 3 + Math.floor(rnd() * 6);
  while (ops.length < len) {
    const op = { name: pick() };
    op.id = IDS[Math.floor(rnd() * IDS.length)];
    op.field = FIELDS[Math.floor(rnd() * FIELDS.length)];
    ops.push(op);
  }
  return ops;
}

const o = (name, id, field) => ({ name, id, field });
const FIXED = {
  // Round 3 HIGH tool/lib/init.ml:53: import after a failed export loses the newer values.
  "r3-import-after-dirty-export": [o("init"), o("register_rho"), o("annotate", "rho", "notes"),
    o("annotate", "rho", "interest"), o("annotate_fail", "rho", "notes"), o("annotate_fail", "rho", "rules"), o("import")],
  "dirty-import-prefer-file": [o("init"), o("register_rho"), o("annotate", "rho", "notes"),
    o("annotate_fail", "rho", "notes"), o("import_prefer")],
  // --prefer-file lets the file win on the fields it carries; a field it lacks keeps the
  // database's value (the upsert is non-destructive, C-4).
  "dirty-import-prefer-file-other-field": [o("init"), o("register_rho"), o("annotate", "rho", "notes"),
    o("annotate_fail", "rho", "rules"), o("annotate_fail", "rho", "notes"), o("import_prefer")],
  "dirty-init-force": [o("init"), o("refresh"), o("annotate", "gamma", "notes"),
    o("annotate_fail", "gamma", "rules"), o("init_force"), o("init_discard")],
  "dirty-then-retry-by-record-usage": [o("init"), o("refresh"), o("annotate_fail", "alpha", "interest"),
    o("record_usage", "alpha"), o("import"), o("init_force")],
  "unregister-keeps-curated": [o("init"), o("register_rho_notes"), o("record_usage", "rho"),
    o("unregister_rho"), o("export"), o("init_force")],
  "clear-then-import": [o("init"), o("refresh"), o("annotate", "alpha", "notes"),
    o("clear", "alpha", "notes"), o("import"), o("refresh_repo", "alpha")],
};

// ------------------------------------------------------------------ the model

// {key: {fields: {f: v}, usage: Set("consumer|note")}}.
const emptyState = () => Object.fromEntries(IDS.map((id) => [id, { fields: {}, usage: new Set() }]));
const clone = (s) => Object.fromEntries(IDS.map((id) => [id, { fields: { ...s[id].fields }, usage: new Set(s[id].usage) }]));

function fileState(W) {
  const s = emptyState();
  let j = {};
  try {
    j = JSON.parse(fs.readFileSync(path.join(W, "data", "curated.json"), "utf8"));
  } catch (_) {
    return null;
  }
  for (const id of IDS) {
    const e = j[ROWS[id].key];
    if (!e) continue;
    s[id].listed = true;
    for (const f of FIELDS) if (typeof e[f] === "string") s[id].fields[f] = e[f];
    for (const u of e.usage || []) s[id].usage.add(`${u.consumer}|${u.note || ""}`);
  }
  return s;
}

// Values the model tracks that the other state lacks or holds differently. A field the
// model cleared is untracked (`undefined`): a refresh may pre-fill it again.
function diff(model, other) {
  const out = [];
  for (const id of IDS) {
    for (const [f, v] of Object.entries(model[id].fields)) {
      if (v === undefined) continue;
      if (other[id].fields[f] !== v) out.push(`${id}.${f}`);
    }
    for (const u of model[id].usage) if (!other[id].usage.has(u)) out.push(`${id}.usage`);
  }
  return out;
}

function unexported(model, file) {
  return file === null || diff(model, file).length > 0;
}

function dbStateFrom(gets) {
  const s = emptyState();
  for (const id of IDS) {
    const g = gets[id];
    if (!g) continue;
    for (const f of FIELDS) if (typeof g[f] === "string") s[id].fields[f] = g[f];
    for (const u of g.usage || []) s[id].usage.add(`${u.consumer}|${u.note || ""}`);
  }
  return s;
}

async function observe(bin, env) {
  const gets = {};
  const explains = {};
  await Promise.all(
    IDS.map(async (id) => {
      const g = await runAsync(bin, ["get", ROWS[id].key], env);
      if (g.code === 0) gets[id] = JSON.parse(g.out);
      const e = await runAsync(bin, ["explain", ROWS[id].key], env);
      if (e.code === 0) explains[id] = e.out;
    })
  );
  return { gets, explains };
}

// The ops after which curated.json must equal the model (a successful curated write or
// export, or a refresh retrying a dirty export).
const EXPORTING = new Set(["annotate", "clear", "record_usage", "register_rho_notes", "export",
  "refresh", "refresh_repo", "unregister_rho"]);

function failExport(W) {
  const f = path.join(W, "data", "curated.json");
  fs.renameSync(f, `${f}.held`);
  fs.mkdirSync(f);
}

function restoreExport(W) {
  const f = path.join(W, "data", "curated.json");
  fs.rmdirSync(f);
  fs.renameSync(`${f}.held`, f);
}

async function runSequence(bin, shared, root, label, ops) {
  const W = path.join(root, label);
  fs.cpSync(shared.tpl, W, { recursive: true });
  write(path.join(W, "data", "sources.toml"),
    '[owner]\nauthor_regex = "example"\n\n[[source]]\nkind = "github-user"\nname = "example-user"\n\n' +
      `[[source]]\nkind = "local-root"\nname = "${path.join(W, "home", "dev")}"\n`);
  const env = {
    PATH: `${path.join(W, "fake")}:/usr/bin:/bin`,
    HOME: path.join(W, "home"),
    GIT_CONFIG_NOSYSTEM: "1",
    LANG: "C.UTF-8",
    LIBRARY_INDEX_DATA: path.join(W, "data"),
    LI_FORGE: shared.forge,
  };
  const failures = [];
  const label1 = (op) =>
    ["annotate", "annotate_fail", "clear"].includes(op.name) ? `${op.name}(${op.id}.${op.field})`
      : ["record_usage", "refresh_repo"].includes(op.name) ? `${op.name}(${op.id})` : op.name;
  const opsText = ops.map(label1).join(" -> ");
  let model = emptyState();
  for (let i = 0; i < ops.length; i++) {
    const op = Object.assign({ value: `v${i}x${label.replace(/[^a-z0-9]/gi, "")}` }, ops[i]);
    const fail = (what) => failures.push(`[${label}] ${opsText}\n    step ${i + 1} (${op.name}): ${what}`);
    const before = fileState(W);
    const dirty = before !== null && unexported(model, before);
    if (op.name === "annotate_fail") failExport(W);
    const r = await runAsync(bin, opArgv(W, op), env);
    if (op.name === "annotate_fail") restoreExport(W);
    const msg = `${r.out}\n${r.err}`;
    if (r.signal || r.code >= 125) fail(`crashed (exit ${r.code})`);
    if (i === 0 && r.code !== 0) setup(`[${label}] init failed: ${r.err}`);
    const file = fileState(W);
    // Model transition.
    const ok = r.code === 0 || r.code === 2;
    switch (op.name) {
      case "annotate":
        if (ok) model[op.id].fields[op.field] = op.value;
        break;
      case "annotate_fail":
        if (/DB updated, export failed/.test(msg)) model[op.id].fields[op.field] = op.value;
        else if (ok) fail("annotate succeeded although the export could not be written");
        break;
      case "clear":
        if (ok) model[op.id].fields[op.field] = undefined;
        break;
      case "record_usage":
        if (ok) model[op.id].usage.add(`${op.value}|n-${op.value}`);
        break;
      case "register_rho_notes":
        if (ok) model.rho.fields.notes = op.value;
        break;
      case "import":
      case "init_force":
        if (dirty) {
          if (r.code !== 1) fail(`${op.name} with unexported curated data exited ${r.code} (expected a refusal, exit 1)`);
          const flag = op.name === "import" ? "--prefer-file" : "--discard-unexported";
          if (!msg.includes(flag)) fail(`the ${op.name} refusal does not name ${flag}`);
        } else if (r.code !== 0) fail(`${op.name} without unexported data exited ${r.code}`);
        if (op.name === "init_force" && !dirty && r.code === 0) model = clone(before);
        break;
      case "import_prefer":
        if (r.code !== 0) fail(`import --prefer-file exited ${r.code}`);
        else if (before)
          for (const id of IDS) {
            Object.assign(model[id].fields, before[id].fields);
            // Import makes a listed entry's usage equal to the file's (PR #9); an
            // unlisted row keeps its usage.
            if (before[id].listed) model[id].usage = new Set(before[id].usage);
          }
        break;
      case "init_discard":
        if (r.code !== 0) fail(`init --force --discard-unexported exited ${r.code}`);
        else if (before) model = clone(before);
        break;
      default:
        break;
    }
    if (dirty && /init --force/.test(msg) && !op.name.startsWith("init"))
      fail("a message recommends `init --force` while curated data is unexported");
    if (dirty && op.name.startsWith("init") && /init --force(?! --discard-unexported)/.test(r.err) && r.code === 1)
      fail("the refusal recommends `init --force` while curated data is unexported");
    // Comparisons.
    const seen = await observe(bin, env);
    for (const d of diff(model, dbStateFrom(seen.gets))) fail(`get lost or changed ${d}`);
    for (const id of IDS)
      for (const u of model[id].usage) {
        const consumer = u.split("|")[0];
        if (!seen.explains[id] || !seen.explains[id].includes(consumer)) fail(`explain lost ${id}.usage`);
      }
    if (EXPORTING.has(op.name) && ok && file !== null)
      for (const d of diff(model, file)) fail(`curated.json lacks ${d} after a successful export`);
    if (failures.length) break;
  }
  if (!process.env.LI_KEEP) fs.rmSync(W, { recursive: true, force: true });
  return failures;
}

async function pool(items, jobs, f) {
  const results = new Array(items.length);
  let next = 0;
  const worker = async () => {
    while (next < items.length) {
      const i = next++;
      results[i] = await f(items[i]);
    }
  };
  await Promise.all(Array.from({ length: jobs }, worker));
  return results;
}

async function main() {
  const NAME = "curated-no-loss";
  const started = Date.now();
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "li-curated-"));
  let code = 0;
  try {
    // A private copy: a rebuild during the run cannot swap the binary under it.
    const bin = path.join(tmp, "library-index.exe");
    fs.copyFileSync(binary(), bin);
    fs.chmodSync(bin, 0o755);
    const shared = buildTemplate(tmp);
    const nseq = Number(process.env.LI_SEQS || 220);
    const seqs = Object.entries(FIXED).map(([label, ops]) => ({ label, ops }));
    for (let seed = 1; seed <= nseq; seed++) seqs.push({ label: `seed-${seed}`, ops: randomSequence(seed) });
    const only = process.env.LI_ONLY;
    const chosen = only ? seqs.filter((s) => s.label === only) : seqs;
    if (chosen.length === 0) setup(`no sequence named ${only}`);
    const jobs = Number(process.env.LI_JOBS || Math.max(2, Math.min(16, os.cpus().length)));
    const all = await pool(chosen, jobs, (s) => runSequence(bin, shared, tmp, s.label, s.ops));
    const failures = all.flat();
    const steps = chosen.reduce((n, s) => n + s.ops.length, 0);
    const secs = ((Date.now() - started) / 1000).toFixed(1);
    if (failures.length) {
      console.error(`${NAME}: FAIL: ${all.filter((f) => f.length).length} of ${chosen.length} sequences broke INV-K (${steps} steps, ${secs}s)`);
      for (const f of all.filter((x) => x.length).slice(0, 40))
        for (const line of f.slice(0, 3)) console.error(`  ${line}`);
      code = 1;
    } else console.log(`${NAME}: ok (${chosen.length} sequences, ${steps} steps, ${secs}s)`);
  } catch (e) {
    console.error(`${NAME}: setup error: ${e instanceof SetupError ? e.message : e.stack}`);
    code = 2;
  } finally {
    if (!process.env.LI_KEEP) fs.rmSync(tmp, { recursive: true, force: true });
  }
  process.exit(code);
}

main();
