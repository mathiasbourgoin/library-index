#!/usr/bin/env node
// CHECK-43 (spec "Minimal-freeze", INV-C, FR-128, AC-43): confidentiality closure.
//
// A deterministic operation-sequence harness. It drives the real binary (CLI and one MCP
// stdio session per step) over >= 200 seeded sequences of length <= 8, plus the round-1,
// round-2 and round-3 HIGH reproductions as fixed sequences, on a synthetic world:
//   - a temp HOME with a dev root of git checkouts: one plain local repository, one whose
//     origin is under the campaign group configured from the start, one whose origin is
//     under a group that a later `register-source --campaign-prefix` makes a campaign
//     group, and the checkout of a listed GitHub repository carrying a second remote under
//     the campaign group; plus a checkout outside the dev root (registered by path) with a
//     mirror remote under the later campaign group. Every GitLab remote is unreadable to
//     the API (404).
//   - fake gh/glab on PATH serving synthetic listings, metadata, READMEs, document trees,
//     files and manifests from a table (an "outage" flag file turns them into network
//     failures);
//   - a temp data dir (sources.toml, curated.json, whitehat_tools.json, a prototype
//     cache/raw.json whose gaps name the repositories).
// Every repository's README, description, documents, API items and cached synopsis carry
// unique sentinels. After EACH step every surface is collected: CLI get, search, list,
// resolve, docs, find, explain, copies, whats-new, sources list; the MCP tools of the same
// names plus resources/read (repository and document URIs) and completion/complete;
// render (INDEX.md + notes page); curated.json and whitehat_tools.json; the stored gaps
// (INDEX.md "Gaps" and the notes page gap list).
// Assertions after each step:
//   - leak: no sentinel of a row that is campaign under the current sources.toml (parsed
//     after the step) or under the sticky flag (campaign at an earlier step while the row
//     stayed in the database) appears on any surface, and no gap line names such a row;
//   - authentic: every sentinel of a non-campaign row that the step fetched appears on at
//     least one surface (the harness is not vacuous);
//   - no surface command crashes (exit >= 3 / signal).
// On failure it prints the sequence (ops), the step, the surface and the sentinel id, never
// repository content.
//
// Exit: 0 = pass, 1 = assertion fired, >= 2 = setup error.
// Binary: $LIBRARY_INDEX_BIN, else built from the nearest dune project (switch from
// $LIBRARY_INDEX_SWITCH or the nearest `_opam`), so the check also runs on a pre-fix copy.
// Options (environment): LI_SEQS=N limits the random sequences (default 220),
// LI_ONLY=name runs one fixed sequence or "seed-<n>", LI_JOBS=N parallel worlds,
// LI_KEEP=1 keeps the temp worlds, LI_DUMP=dir writes every surface of every step there.
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

function dune(root, args) {
  const sw = opamSwitch(root);
  const [cmd, argv] = sw
    ? ["opam", ["exec", `--switch=${sw}`, "--", "dune", ...args]]
    : ["dune", args];
  return spawnSync(cmd, argv, { cwd: root, encoding: "utf8" });
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

// ------------------------------------------------------------------ processes

function runAsync(cmd, args, opts) {
  return new Promise((resolve) => {
    const child = spawn(cmd, args, {
      env: opts.env,
      cwd: opts.cwd,
      stdio: ["pipe", "pipe", "pipe"],
    });
    const out = [];
    const err = [];
    child.stdout.on("data", (d) => out.push(d));
    child.stderr.on("data", (d) => err.push(d));
    child.on("error", (e) => resolve({ code: 127, signal: null, out: "", err: e.message }));
    child.on("close", (code, signal) =>
      resolve({
        code,
        signal,
        out: Buffer.concat(out).toString("utf8"),
        err: Buffer.concat(err).toString("utf8"),
      })
    );
    child.stdin.on("error", () => {});
    if (opts.input != null) child.stdin.end(opts.input);
    else child.stdin.end();
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

// Repositories. [campaign] says when the row is campaign under the configuration:
// "always" (a remote under the group configured from the start), "later" (a remote or
// owner under acme-later), "shadow" (owner shadow-org), "registered" (only through a
// `register --campaign`), "never".
const REPOS = [
  { id: "alpha", key: "github:example-user/alpha-lib", fn: "example-user/alpha-lib", name: "alpha-lib", rule: "registered", forge: true },
  { id: "eps", key: "github:example-user/epsilon", fn: "example-user/epsilon", name: "epsilon", rule: "always", forge: true, checkout: "dev/epsilon",
    remotes: [["origin", "git@github.com:example-user/epsilon.git"], ["audit", "git@gitlab.com:acme-campaigns/epsilon-audit.git"]] },
  { id: "beta", key: "github:shadow-org/beta-tool", fn: "shadow-org/beta-tool", name: "beta-tool", rule: "shadow", forge: true },
  { id: "delta", key: "github:shadow-org/delta-kit", fn: "shadow-org/delta-kit", name: "delta-kit", rule: "shadow", forge: true },
  { id: "kappa", key: "github:example-user/kappa-lib", fn: "example-user/kappa-lib", name: "kappa-lib", rule: "later", forge: true, checkout: "elsewhere/kappa",
    remotes: [["origin", "git@github.com:example-user/kappa-lib.git"], ["mirror", "git@gitlab.com:acme-later/kappa-mirror.git"]] },
  { id: "camp", key: "local:camp-one", name: "camp-one", rule: "always", checkout: "dev/camp-one",
    remotes: [["origin", "git@gitlab.com:acme-campaigns/camp-one.git"]] },
  { id: "later", key: "local:later-secret", name: "later-secret", rule: "later", checkout: "dev/later-secret",
    remotes: [["origin", "git@gitlab.com:acme-later/later-secret.git"]] },
  { id: "gamma", key: "local:gamma-local", name: "gamma-local", rule: "never", checkout: "dev/gamma-local", remotes: [] },
];
const BY_ID = Object.fromEntries(REPOS.map((r) => [r.id, r]));

// Sentinels: R readme, D description, O document, A API item, S cached synopsis.
const KINDS = ["R", "D", "O", "A", "S"];
const sentinel = (id, k) => `ZQ${k}${id.toUpperCase()}X7`;
// The query word placed next to each sentinel: queries never contain a sentinel, so an
// echoed query cannot be mistaken for a leak.
const qword = (id, k) => `QW${k}${id.toUpperCase()}`;
const tagged = (id, k) => `${qword(id, k)} ${sentinel(id, k)}`;
const docPath = (r) => `docs/guide-${r.id}.md`;
const mliPath = (r) => `src/${r.id}_api.mli`;

function files(r) {
  return {
    "README.md": `# ${r.name}\n\nThis repository holds ${tagged(r.id, "R")} notes about parsers.\n\n## Usage\n\nCall run.\n`,
    [docPath(r)]: `# Guide\n\nThe guide explains ${tagged(r.id, "O")} configuration.\n`,
    [mliPath(r)]: `(** Runs the ${tagged(r.id, "A")} pipeline. *)\nval run : unit -> unit\n`,
    "dune-project": `(lang dune 3.0)\n(name ${r.id}_pkg)\n`,
  };
}

const PUSHED = "2026-09-30T10:00:00Z";

function ghRepo(r) {
  const [owner, name] = r.fn.split("/");
  return {
    full_name: r.fn,
    owner: { login: owner },
    name,
    html_url: `https://github.com/${r.fn}`,
    private: false,
    fork: false,
    archived: false,
    language: "OCaml",
    description: `Tool for ${tagged(r.id, "D")} work`,
    default_branch: "main",
    pushed_at: PUSHED,
    updated_at: PUSHED,
  };
}

function forgeTable() {
  const t = {};
  const gh = (p, body) => {
    t[`gh ${p}`] = typeof body === "string" ? body : JSON.stringify(body);
  };
  const listed = (ids) => ids.map((id) => ghRepo(BY_ID[id]));
  gh("users/example-user/repos?type=all&per_page=100", listed(["alpha", "eps"]));
  gh("orgs/shadow-org", { login: "shadow-org" });
  gh("orgs/shadow-org/repos?type=all&per_page=100", listed(["delta"]));
  for (const r of REPOS.filter((x) => x.forge)) {
    const base = `repos/${r.fn}`;
    const fs_ = files(r);
    gh(base, ghRepo(r));
    gh(`${base}/readme`, fs_["README.md"]);
    gh(`${base}/contents/`, [
      { name: "README.md", path: "README.md", type: "file" },
      { name: "dune-project", path: "dune-project", type: "file" },
      { name: "docs", path: "docs", type: "dir" },
      { name: "src", path: "src", type: "dir" },
    ]);
    gh(`${base}/commits?per_page=1`, [
      { sha: "0123456789abcdef", commit: { committer: { date: PUSHED }, author: { date: PUSHED } } },
    ]);
    gh(`${base}/contributors?per_page=100`, [{ login: "example-user", contributions: 40 }]);
    gh(`${base}/git/trees/main?recursive=1`, {
      truncated: false,
      tree: Object.keys(fs_).map((p, i) => ({
        path: p,
        type: "blob",
        sha: crypto.createHash("sha1").update(fs_[p]).digest("hex"),
        size: fs_[p].length + i * 0,
      })),
    });
    for (const p of Object.keys(fs_)) {
      const enc = p.split("/").map(encodeURIComponent).join("/");
      gh(`${base}/contents/${enc}`, fs_[p]);
    }
  }
  return t;
}

const FAKE = `#!/bin/sh
# Fake gh/glab: canned answers keyed by "<prog> <api path>" (argv 4); 404 otherwise.
prog=$(basename "$0")
if [ -f "$LI_WORLD/outage" ]; then
  echo "error connecting to api.example.invalid" >&2
  exit 1
fi
h=$(printf '%s %s' "$prog" "$4" | sha1sum | cut -c1-40)
if [ -f "$LI_FORGE/$h" ]; then
  cat "$LI_FORGE/$h"
  exit 0
fi
echo "$prog: Not Found (HTTP 404)" >&2
exit 1
`;

function gitCheckout(dir, r, env) {
  for (const [p, text] of Object.entries(files(r))) write(path.join(dir, p), text);
  const git = (...args) => must("git", ["-C", dir, ...args], { env });
  git("init", "-q", "-b", "main");
  git("add", "-A");
  git("-c", "user.name=Ada Example", "-c", "user.email=ada@example.org",
    "-c", "commit.gpgsign=false", "commit", "-q", "-m", "init");
  for (const [n, url] of r.remotes) git("remote", "add", n, url);
}

function buildTemplate(dir) {
  const forge = path.join(dir, "forge");
  fs.mkdirSync(forge, { recursive: true });
  for (const [k, body] of Object.entries(forgeTable())) {
    const h = crypto.createHash("sha1").update(k).digest("hex");
    fs.writeFileSync(path.join(forge, h), body);
  }
  const tpl = path.join(dir, "template");
  for (const n of ["gh", "glab"]) write(path.join(tpl, "fake", n), FAKE, 0o755);
  const env = { PATH: process.env.PATH, HOME: path.join(tpl, "home"), GIT_CONFIG_NOSYSTEM: "1" };
  fs.mkdirSync(env.HOME, { recursive: true });
  for (const r of REPOS.filter((x) => x.checkout))
    gitCheckout(path.join(tpl, "home", r.checkout), r, env);
  write(path.join(tpl, "data", "curated.json"), "{}\n");
  write(path.join(tpl, "data", "whitehat_tools.json"), '{"tools": []}\n');
  fs.mkdirSync(path.join(tpl, "notes"), { recursive: true });
  fs.mkdirSync(path.join(tpl, "out"), { recursive: true });
  return { forge, tpl };
}

function initialSources() {
  return (
    '[owner]\ndisplay_name = "Ada Example"\nauthor_regex = "example"\n\n' +
    '[[source]]\nkind = "github-user"\nname = "example-user"\n\n' +
    '[[source]]\nkind = "gitlab-group"\nname = "acme-campaigns"\ncampaign_prefix = true\n'
  );
}

function rawCache(W) {
  const local = (r) => ({
    path: path.join(W, "home", r.checkout),
    worktree: false,
    remote_url: r.remotes.length ? r.remotes[0][1] : null,
    remote_key: r.remotes.length ? `gitlab:${r.remotes[0][1].split(":")[1].replace(/\.git$/, "")}` : null,
    all_remote_keys: r.remotes.map(([, u]) => `gitlab:${u.split(":")[1].replace(/\.git$/, "")}`),
    last_commit: PUSHED,
    my_commits: 1,
    total_commits: 1,
  });
  const row = (r) => ({
    key: r.key,
    host: "local",
    owner: "(local)",
    name: r.name,
    full_name: r.name,
    description: "",
    synopsis: `Cached synopsis ${tagged(r.id, "S")} text`,
    url: `file://${path.join(W, "home", r.checkout)}`,
    private: true,
    local_paths: [path.join(W, "home", r.checkout)],
    local_my_commits: 1,
    local_total_commits: 1,
    pushed_at: PUSHED,
    sources: ["local-unreachable-remote"],
  });
  const ids = ["camp", "later", "gamma"];
  return JSON.stringify({
    repos: Object.fromEntries(ids.map((id) => [BY_ID[id].key, row(BY_ID[id])])),
    local: ids.map((id) => local(BY_ID[id])),
    gaps: [
      "local:camp-one: readme failed (not-found); previous data kept",
      "gitlab:acme-campaigns/camp-one: metadata failed (not-found); previous data kept",
      "local:later-secret: readme failed (not-found); previous data kept",
      "github:example-user/alpha-lib: contributors failed (network); previous data kept",
      "kappa-lib: last-commit failed (network); previous data kept",
      "delta-kit: readme failed (network); previous data kept",
    ],
  });
}

// ------------------------------------------------------------------ operations

const KEYS = REPOS.map((r) => r.key);

function opArgv(W, op) {
  const url = (id) => `https://github.com/${BY_ID[id].fn}`;
  const dev = path.join(W, "home", "dev");
  switch (op.name) {
    case "init": return ["init"];
    case "init_force": return ["init", "--force"];
    case "import": return ["import"];
    case "import_raw": return ["import-raw"];
    case "refresh": case "refresh_outage": return ["refresh", "--no-analysis"];
    case "refresh_repo": return ["refresh", "--repo", op.key];
    case "reg_beta": return ["register", url("beta"), "--ownership", "external"];
    case "reg_alpha_camp": return ["register", url("alpha"), "--campaign"];
    case "reg_kappa": return ["register", path.join(W, "home", "elsewhere", "kappa")];
    case "rs_local": return ["register-source", "local-root", dev];
    case "rs_later": return ["register-source", "gitlab-group", "acme-later", "--campaign-prefix"];
    case "rs_later_plain": return ["register-source", "gitlab-group", "acme-later"];
    case "rs_later_conf": return ["register-source", "gitlab-group", "acme-later", "--confidential", "generic"];
    case "rs_shadow": return ["register-source", "github-org", "shadow-org", "--campaign-prefix"];
    case "rs_shadow_plain": return ["register-source", "github-org", "shadow-org"];
    case "unreg_beta": return ["unregister", url("beta")];
    case "unreg_alpha": return ["unregister", url("alpha")];
    case "unreg_kappa": return ["unregister", path.join(W, "home", "elsewhere", "kappa")];
    case "unrs_later": return ["unregister-source", "gitlab-group", "acme-later"];
    case "unrs_shadow": return ["unregister-source", "github-org", "shadow-org"];
    case "unrs_local": return ["unregister-source", "local-root", dev];
    case "unrs_campaigns": return ["unregister-source", "gitlab-group", "acme-campaigns"];
    case "annotate": return ["annotate", op.key, "--set", `notes=harness note ${op.step}`];
    case "record_usage": return ["record-usage", op.key, `consumer-${op.step}`, "--note", "harness"];
    case "analyze": return ["analyze", op.key];
    case "render": return renderArgv(W);
    case "export": return ["export"];
    default: return setup(`unknown op ${op.name}`);
  }
}

function renderArgv(W) {
  return ["render", "--out", path.join(W, "out", "INDEX.md"), "--notes-dir", path.join(W, "notes"),
    "--now", "2026-10-04T12:00:00Z"];
}

const WEIGHTS = {
  init_force: 1, import: 1, import_raw: 2, refresh: 3, refresh_outage: 2, refresh_repo: 2,
  reg_beta: 3, reg_alpha_camp: 2, reg_kappa: 3, rs_local: 4, rs_later: 4, rs_later_plain: 2, rs_later_conf: 2,
  rs_shadow: 3, rs_shadow_plain: 3, unreg_beta: 1, unreg_alpha: 1, unreg_kappa: 1,
  unrs_later: 1, unrs_shadow: 1, unrs_local: 1, unrs_campaigns: 1, annotate: 2,
  record_usage: 1, analyze: 2, render: 1, export: 1,
};
const KEYED = new Set(["refresh_repo", "annotate", "record_usage", "analyze"]);

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
  const rnd = mulberry32(seed * 7919 + 17);
  const names = Object.keys(WEIGHTS);
  const total = names.reduce((s, n) => s + WEIGHTS[n], 0);
  const pick = () => {
    let x = rnd() * total;
    for (const n of names) {
      x -= WEIGHTS[n];
      if (x < 0) return n;
    }
    return names[names.length - 1];
  };
  const len = 3 + Math.floor(rnd() * 6); // 3..8, init included
  const ops = [{ name: "init" }];
  while (ops.length < len) {
    const name = pick();
    const op = { name };
    if (KEYED.has(name)) op.key = KEYS[Math.floor(rnd() * KEYS.length)];
    ops.push(op);
  }
  return ops;
}

const o = (name, key) => (key ? { name, key } : { name });
const FIXED = {
  // Round 3 HIGH tool/lib/source_reg.ml:294: a campaign prefix registered after the row
  // was stored (the listing of the new source fails) must reclassify it at once.
  "r3-register-source-prefix-after-rows": [o("init"), o("rs_local"), o("rs_later"), o("render")],
  "r3-prefix-after-registered-path": [o("init"), o("reg_kappa"), o("rs_later")],
  // INV-C: a confidential level added after rows exist acts as a campaign prefix at once.
  "r3-confidential-after-rows": [o("init"), o("rs_local"), o("reg_kappa"), o("rs_later_conf"), o("render")],
  "r3-prefix-after-registered-url": [o("init"), o("reg_beta"), o("rs_shadow")],
  "r3-prefix-after-listed": [o("init"), o("rs_shadow_plain"), o("rs_shadow")],
  "r3-prefix-after-import-raw-gaps": [o("init"), o("import_raw"), o("rs_local"), o("rs_later"), o("render")],
  // Round 2 HIGH tool/lib/register.ml:397 (identity of one source) and :389 (flags kept).
  "r2-register-source-identity": [o("init"), o("rs_local")],
  "r2-register-source-keeps-flags": [o("init"), o("rs_local"), o("rs_later"), o("rs_later_plain"), o("refresh")],
  // Round 1 HIGH lib/sources.ml:326 (unreadable campaign remote) through a full refresh.
  "r1-unreadable-campaign-remote": [o("init"), o("rs_local"), o("refresh"), o("render")],
  // Sticky flag: removing the prefix never declassifies a stored row.
  "sticky-after-unregister-source": [o("init"), o("rs_local"), o("rs_later"), o("unrs_later"), o("refresh"), o("refresh_repo", "local:later-secret")],
  "campaign-registration-after-refresh": [o("init"), o("refresh"), o("reg_alpha_camp"), o("render")],
  "outage-gaps-name-campaign-rows": [o("init"), o("reg_beta"), o("rs_shadow"), o("refresh_outage"), o("render")],
  "analyze-after-prefix": [o("init"), o("rs_local"), o("rs_later_plain"), o("analyze", "local:later-secret")],
  "authentic-everything": [o("init"), o("rs_local"), o("rs_shadow_plain"), o("reg_beta"), o("reg_kappa"), o("refresh"), o("render")],
};

// ------------------------------------------------------------------ the model

// sources.toml as written by the tool: the [[source]] and [[repo]] tables.
function parseSources(text) {
  const tables = [];
  let cur = null;
  for (const raw of text.split("\n")) {
    const line = raw.trim();
    if (line.startsWith("[[")) {
      cur = { table: line.replace(/[[\]\s]/g, ""), kv: {} };
      tables.push(cur);
    } else if (line.startsWith("[")) {
      cur = null;
    } else if (cur && line.includes("=") && !line.startsWith("#")) {
      const i = line.indexOf("=");
      cur.kv[line.slice(0, i).trim()] = line.slice(i + 1).trim().replace(/^"|"$/g, "");
    }
  }
  return tables;
}

function configState(W) {
  const t = parseSources(fs.readFileSync(path.join(W, "data", "sources.toml"), "utf8"));
  const prefixes = new Set();
  let localRoot = false;
  const campaignRegs = new Set();
  const registered = new Set();
  for (const e of t) {
    if (e.table === "source") {
      const conf = e.kv.confidential && e.kv.confidential !== "none";
      if (e.kv.kind === "local-root") localRoot = true;
      else if (e.kv.campaign_prefix === "true" || conf) prefixes.add(e.kv.name.toLowerCase());
      if (e.kv.kind === "github-org") registered.add(`org:${e.kv.name}`);
    } else if (e.table === "repo") {
      const target = e.kv.target || "";
      for (const r of REPOS) {
        const hit = (r.fn && target.toLowerCase().includes(r.fn.toLowerCase())) ||
          (r.checkout && target.endsWith(r.checkout));
        if (hit) {
          registered.add(r.id);
          if (e.kv.campaign === "true" || e.kv.campaign_prefix === "true" ||
            e.kv.confidential === "generic") campaignRegs.add(r.id);
        }
      }
    }
  }
  return { prefixes, localRoot, campaignRegs, registered };
}

function configCampaign(cfg, r) {
  const has = (p) => cfg.prefixes.has(p);
  switch (r.id) {
    case "alpha": return cfg.campaignRegs.has("alpha");
    case "eps": return has("acme-campaigns") && cfg.localRoot;
    case "camp": return has("acme-campaigns");
    case "later": case "kappa": return has("acme-later") || cfg.campaignRegs.has(r.id);
    case "beta": case "delta": return has("shadow-org") || cfg.campaignRegs.has(r.id);
    default: return cfg.campaignRegs.has(r.id);
  }
}

// The rows a step fetched (their sentinels must show unless campaign).
function produced(op, cfg, ok) {
  if (!ok) return [];
  const local = cfg.localRoot ? ["gamma", "later", "camp", "eps"] : [];
  switch (op.name) {
    case "refresh": {
      const reg = ["beta", "kappa", "alpha"].filter((id) => cfg.registered.has(id));
      const org = cfg.registered.has("org:shadow-org") ? ["delta"] : [];
      return [...new Set(["alpha", "eps", ...reg, ...org, ...local])];
    }
    case "reg_beta": return ["beta"];
    case "reg_kappa": return ["kappa"];
    case "rs_local": return ["gamma", "later", "camp", "eps"];
    case "rs_shadow_plain": case "rs_shadow": return ["delta"];
    default: return [];
  }
}

// Which sentinels the authentic check expects for a fetched row (README and description
// of forge rows, README of local rows, documents of the owned or registered ones).
function expectedVisible(r) {
  const docs = ["alpha", "beta", "kappa", "gamma"].includes(r.id) ? ["O"] : [];
  return (r.forge ? ["R", "D"] : ["R"]).concat(docs);
}

// ------------------------------------------------------------------ surfaces

function mcpRequests(present) {
  const reqs = [
    { jsonrpc: "2.0", id: 1, method: "initialize", params: { protocolVersion: "2025-11-25", capabilities: {}, clientInfo: { name: "closure", version: "0" } } },
    { jsonrpc: "2.0", method: "notifications/initialized" },
  ];
  let id = 10;
  const call = (name, args) => reqs.push({ jsonrpc: "2.0", id: id++, method: "tools/call", params: { name, arguments: args } });
  const other = (method, params) => reqs.push({ jsonrpc: "2.0", id: id++, method, params });
  for (const r of REPOS) {
    call("get", { repo: r.key });
    call("explain", { repo: r.key });
    call("copies", { name: r.name });
    call("resolve-library-id", { libraryName: r.name, query: "parsers" });
    for (const k of ["R", "O", "A"]) call("query-docs", { libraryId: r.key, query: qword(r.id, k), limit: 10 });
    call("query-docs", { libraryId: r.key, query: "guide configuration", limit: 10 });
    for (const k of KINDS) call("search", { query: qword(r.id, k), limit: 50 });
    call("find-capability", { query: qword(r.id, "A"), limit: 20 });
  }
  call("search", { query: "parsers", limit: 50 });
  call("search", { query: "guide", limit: 50, kind: "doc" });
  call("list", { limit: 500 });
  call("find-capability", { query: "pipeline", limit: 50 });
  call("whats-new", { since: "2000-01-01", limit: 100 });
  call("list-sources", {});
  for (const r of present) {
    const host = r.key.split(":")[0];
    const [owner, name] = r.fn ? r.fn.split("/") : ["_", r.name];
    const base = host === "local" ? `local/_/${name}` : `${host}/${owner}/${name}`;
    other("resources/read", { uri: `libindex://repo/${base}` });
    for (const p of Object.keys(files(r)))
      other("resources/read", { uri: `libindex://doc/${base}/${encodeURIComponent(p)}` });
    other("completion/complete", {
      ref: { type: "ref/resource", uri: "libindex://doc/{host}/{owner}/{name}/{path}" },
      argument: { name: "path", value: "" },
      context: { arguments: { host, owner: host === "local" ? "_" : owner, name } },
    });
  }
  for (const v of ["host", "owner", "name"])
    other("completion/complete", {
      ref: { type: "ref/resource", uri: "libindex://repo/{host}/{owner}/{name}" },
      argument: { name: v, value: "" },
      context: { arguments: {} },
    });
  return reqs.map((r) => JSON.stringify(r)).join("\n") + "\n";
}

async function cliSurfaces(bin, env) {
  const jobs = [];
  const add = (surface, args) => jobs.push({ surface, args });
  for (const r of REPOS) {
    add(`cli get ${r.id}`, ["get", r.key]);
    add(`cli explain ${r.id}`, ["explain", r.key]);
    add(`cli copies ${r.id}`, ["copies", r.name]);
    add(`cli resolve ${r.id}`, ["resolve", r.name, "parsers"]);
    for (const k of ["R", "O", "A"]) add(`cli docs ${r.id}/${k}`, ["docs", r.key, qword(r.id, k), "--limit", "10"]);
    for (const k of KINDS) add(`cli search ${r.id}/${k}`, ["search", qword(r.id, k), "--limit", "50"]);
    add(`cli find ${r.id}`, ["find", qword(r.id, "A"), "--limit", "20"]);
  }
  add("cli search generic", ["search", "parsers", "--limit", "50"]);
  add("cli list", ["list", "--limit", "500"]);
  add("cli whats-new", ["whats-new", "2000-01-01"]);
  add("cli sources list", ["sources", "list"]);
  const res = await Promise.all(jobs.map((j) => runAsync(bin, j.args, { env })));
  return jobs.map((j, i) => ({ surface: j.surface, res: res[i] }));
}

function readIf(p) {
  try {
    return fs.readFileSync(p, "utf8");
  } catch (_) {
    return "";
  }
}

function gapLines(index, html) {
  const lines = [];
  const m = index.split("**Gaps:**\n")[1];
  if (m) for (const l of m.split("\n")) {
    if (!l.startsWith("- ")) break;
    lines.push(l);
  }
  const g = html.match(/<li>[^<]*<\/li>/g) || [];
  return { index: lines.join("\n"), html: g.join("\n") };
}

async function surfaces(bin, W, env) {
  const out = [];
  const cli = await cliSurfaces(bin, env);
  const present = new Set();
  for (const c of cli) {
    out.push({ surface: c.surface, text: c.res.out + c.res.err, res: c.res });
    const m = c.surface.match(/^cli get (\w+)$/);
    if (m && c.res.code === 0) present.add(m[1]);
  }
  const mcp = await runAsync(bin, ["mcp"], { env, input: mcpRequests(REPOS.filter((r) => present.has(r.id))) });
  out.push({ surface: "mcp session", text: mcp.out + mcp.err, res: mcp });
  for (const n of fs.readdirSync(path.join(W, "notes"))) fs.unlinkSync(path.join(W, "notes", n));
  const rend = await runAsync(bin, renderArgv(W), { env });
  out.push({ surface: "cli render", text: rend.out + rend.err, res: { code: rend.code === 2 ? 0 : rend.code, signal: rend.signal } });
  const index = readIf(path.join(W, "out", "INDEX.md"));
  const html = fs.readdirSync(path.join(W, "notes")).map((n) => readIf(path.join(W, "notes", n))).join("\n");
  out.push({ surface: "INDEX.md", text: index });
  out.push({ surface: "notes page", text: html });
  out.push({ surface: "curated.json", text: readIf(path.join(W, "data", "curated.json")) });
  out.push({ surface: "whitehat_tools.json", text: readIf(path.join(W, "data", "whitehat_tools.json")) });
  const gaps = gapLines(index, html);
  return { list: out, present, gaps };
}

// ------------------------------------------------------------------ one sequence

async function runSequence(bin, shared, root, label, ops) {
  const W = path.join(root, label);
  fs.cpSync(shared.tpl, W, { recursive: true });
  write(path.join(W, "data", "sources.toml"), initialSources());
  write(path.join(W, "data", "cache", "raw.json"), rawCache(W));
  const env = {
    PATH: `${path.join(W, "fake")}:/usr/bin:/bin`,
    HOME: path.join(W, "home"),
    GIT_CONFIG_NOSYSTEM: "1",
    LANG: "C.UTF-8",
    LIBRARY_INDEX_DATA: path.join(W, "data"),
    LI_WORLD: W,
    LI_FORGE: shared.forge,
  };
  let sticky = new Set();
  let presentPrev = new Set();
  const failures = [];
  const opsText = ops.map((op) => (op.key ? `${op.name}(${op.key})` : op.name)).join(" -> ");
  for (let i = 0; i < ops.length; i++) {
    const op = Object.assign({ step: i }, ops[i]);
    if (op.name === "refresh_outage") write(path.join(W, "outage"), "1");
    const r = await runAsync(bin, opArgv(W, op), { env });
    if (op.name === "refresh_outage") fs.unlinkSync(path.join(W, "outage"));
    const fail = (surface, what) =>
      failures.push(`[${label}] ${opsText}\n    after step ${i + 1} (${op.name}): ${surface}: ${what}`);
    if (r.signal || r.code >= 124) fail(`op ${op.name}`, `crashed or unusable (exit ${r.code}, signal ${r.signal})`);
    if (i === 0 && r.code !== 0) setup(`[${label}] init failed: ${r.err}`);
    const cfg = configState(W);
    const s = await surfaces(bin, W, env);
    if (process.env.LI_DUMP) {
      const d = path.join(process.env.LI_DUMP, label, `step-${i + 1}-${op.name}`);
      fs.mkdirSync(d, { recursive: true });
      s.list.forEach((c, j) => fs.writeFileSync(path.join(d, `${j}-${c.surface.replace(/[^a-z0-9]+/gi, "_")}.txt`), c.text));
    }
    const recreated = (op.name === "init" || op.name === "init_force") && r.code === 0;
    const campaign = new Set();
    for (const rp of REPOS) {
      const wasSticky = !recreated && presentPrev.has(rp.id) && sticky.has(rp.id);
      if (s.present.has(rp.id) && (configCampaign(cfg, rp) || wasSticky)) campaign.add(rp.id);
    }
    for (const c of s.list)
      if (c.res && (c.res.signal || c.res.code > 2)) fail(c.surface, `crashed (exit ${c.res.code})`);
    for (const id of campaign) {
      for (const k of KINDS) {
        const sen = sentinel(id, k).toLowerCase();
        for (const c of s.list)
          if (c.text.toLowerCase().includes(sen)) fail(c.surface, `campaign sentinel ${id}.${k} visible`);
      }
      const name = BY_ID[id].name.toLowerCase();
      if (s.gaps.index.toLowerCase().includes(name)) fail("INDEX.md gaps", `names campaign row ${id}`);
      if (s.gaps.html.toLowerCase().includes(name)) fail("notes page gaps", `names campaign row ${id}`);
    }
    // The MCP session must answer every request (a dead session would make every MCP
    // surface vacuously clean): JSON-RPC errors other than "resource not found" fail.
    const mcp = s.list.find((c) => c.surface === "mcp session");
    let answered = 0;
    for (const line of mcp.text.split("\n")) {
      if (!line.startsWith("{")) continue;
      let j;
      try {
        j = JSON.parse(line);
      } catch (_) {
        continue;
      }
      if (j.id !== undefined) answered++;
      if (j.error && j.error.code !== -32002) fail("mcp session", `JSON-RPC error ${j.error.code} on request ${j.id}`);
    }
    if (answered < 10) fail("mcp session", `only ${answered} responses (session dead or vacuous)`);
    const ok = r.code === 0 || r.code === 2;
    for (const id of produced(op, cfg, ok)) {
      if (campaign.has(id) || !s.present.has(id)) continue;
      for (const k of expectedVisible(BY_ID[id])) {
        const sen = sentinel(id, k).toLowerCase();
        if (!s.list.some((c) => c.text.toLowerCase().includes(sen)))
          fail("all surfaces", `non-campaign sentinel ${id}.${k} visible nowhere (authentic path)`);
      }
      if (!mcp.text.toLowerCase().includes(sentinel(id, "R").toLowerCase()))
        fail("mcp session", `non-campaign sentinel ${id}.R absent from the MCP surfaces (authentic path)`);
    }
    sticky = campaign;
    presentPrev = s.present;
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
  const NAME = "confidentiality-closure";
  const started = Date.now();
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "li-closure-"));
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
      const failed = all.filter((f) => f.length).length;
      console.error(`${NAME}: FAIL: ${failed} of ${chosen.length} sequences broke INV-C (${steps} steps, ${secs}s)`);
      // One block per failing sequence: its first three findings.
      for (const f of all.filter((x) => x.length).slice(0, 40))
        for (const line of f.slice(0, 3)) console.error(`  ${line}`);
      code = 1;
    } else {
      console.log(`${NAME}: ok (${chosen.length} sequences, ${steps} steps, ${secs}s)`);
    }
  } catch (e) {
    console.error(`${NAME}: setup error: ${e instanceof SetupError ? e.message : e.stack}`);
    code = 2;
  } finally {
    if (!process.env.LI_KEEP) fs.rmSync(tmp, { recursive: true, force: true });
  }
  process.exit(code);
}

main();
