#!/usr/bin/env node
// Ratchet check for review finding lib/fetch.ml:363:spec (spec AM-18, FR-034, AC-18).
//
// When a repository was pushed again and its README fetch then failed, the synopsis was
// recomputed from "no README" and stored as NULL, although the gap said "previous data
// kept".
//
// Scenario (all in a temp dir; fake gh/glab on PATH; HOME and --dev-root are temp; no
// network): refresh 1 lists github:example-user/demo (pushed T1) and serves its README,
// so README, synopsis and readme_sha256 are stored; refresh 2 lists it with a newer
// pushed_at T2 and the README call fails (network). Asserts: refresh 2 exits 2 with a
// classified README gap, and the stored README, synopsis and readme_sha256 are unchanged.
//
// A binary from before sources.toml (no --data-dir, e.g. the pre-fix commit 1c231af)
// hard-codes the owner's identity: the check then reads the listed GitHub user from
// $LIBRARY_INDEX_LEGACY_FIXTURE (private migration/legacy-fixture.sh; exit 2 without it).
//
// Exit: 0 = fixed, 1 = bug present (assertion fired), >= 2 = setup error.
// Binary: $LIBRARY_INDEX_BIN, else built from the nearest dune project (switch from
// $LIBRARY_INDEX_SWITCH or the nearest `_opam`), so the check also runs on a pre-fix copy.
"use strict";

const fs = require("fs");
const os = require("os");
const path = require("path");
const { spawnSync } = require("child_process");

let KEY = "github:example-user/demo";

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

function must(cmd, args, opts, ok) {
  const r = spawnSync(cmd, args, Object.assign({ encoding: "utf8" }, opts));
  if (r.error) setup(`cannot run ${cmd}: ${r.error.message}`);
  if (!(ok || [0]).includes(r.status))
    setup(`${cmd} ${args[0]} exited ${r.status}: ${r.stderr}`);
  return r;
}

// A binary from before sources.toml (no --data-dir) hard-codes the owner's identity: the
// scenario then needs that identity, read at run time from the JSON file named by
// $LIBRARY_INDEX_LEGACY_FIXTURE (written by the private migration/legacy-fixture.sh), so
// that no real name lives in this file. [null] for a config-driven binary.
function legacyIdentity(bin) {
  const help = spawnSync(bin, ["init", "--help=plain"], {
    encoding: "utf8",
    env: { PATH: process.env.PATH, LANG: "C.UTF-8" },
  });
  if (help.error) setup(`cannot run ${bin}: ${help.error.message}`);
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
  if (typeof fx.gh_user !== "string" || fx.gh_user === "") setup("legacy fixture lacks gh_user");
  return fx;
}

// Fake gh: `gh api -X GET <path> ...`; the phase (1 or 2) comes from $FAKE_PHASE. The
// listed GitHub user is example-user, replaced by the legacy identity when there is one.
const GH = `#!/bin/sh
p="$4"
repo='{"full_name":"example-user/demo","owner":{"login":"example-user"},"name":"demo","html_url":"https://github.com/example-user/demo","private":false,"fork":false,"archived":false,"language":"OCaml","description":"A demo","default_branch":"main","pushed_at":"PUSHED","updated_at":"PUSHED"}'
if [ "$FAKE_PHASE" = 2 ]; then pushed=2026-10-02T00:00:00Z; else pushed=2026-10-01T00:00:00Z; fi
case "$p" in
  users/example-user/repos*) echo "[$repo]" | sed "s/PUSHED/$pushed/g" ;;
  user/repos*|users/*/repos*|user/memberships/orgs) echo '[]' ;;
  search/*) echo '{"total_count":0,"items":[]}' ;;
  repos/example-user/demo/readme)
    if [ "$FAKE_PHASE" = 2 ]; then echo 'dial tcp: connection refused' >&2; exit 1; fi
    printf '# Demo\\n\\nDemo is a small library that indexes repositories for agents.\\n' ;;
  repos/example-user/demo/contents/|repos/example-user/demo/commits*) echo '[]' ;;
  *) echo 'gh: Not Found (HTTP 404)' >&2; exit 1 ;;
esac
`;

const GLAB = "#!/bin/sh\necho '[]'\n";

function body(tmp) {
  const bin = binary();
  const legacy = legacyIdentity(bin);
  const user = legacy ? legacy.gh_user : "example-user";
  KEY = `github:${user}/demo`.toLowerCase();
  const home = path.join(tmp, "home");
  const dev = path.join(tmp, "dev");
  const fake = path.join(tmp, "fakebin");
  const data = path.join(tmp, "data");
  for (const d of [home, dev, fake, data]) fs.mkdirSync(d, { recursive: true });
  fs.writeFileSync(path.join(fake, "gh"), GH.replace(/example-user/g, user), { mode: 0o755 });
  fs.writeFileSync(path.join(fake, "glab"), GLAB, { mode: 0o755 });
  fs.writeFileSync(path.join(data, "curated.json"), "{}\n");
  fs.writeFileSync(path.join(data, "whitehat_tools.json"), '{"tools": []}\n');
  // The data dir's sources.toml declares the listed GitHub user (synthetic name).
  fs.writeFileSync(
    path.join(data, "sources.toml"),
    '[[source]]\nkind = "github-user"\nname = "example-user"\n'
  );
  const db = path.join(data, "t.db");
  const env = (phase) => ({
    PATH: `${fake}:${process.env.PATH}`,
    HOME: home,
    LANG: "C.UTF-8",
    FAKE_PHASE: phase,
  });
  const li = (phase, args, ok) => must(bin, args, { env: env(phase) }, ok);
  const get = () => {
    const out = li("1", ["get", "--db", db, KEY]).stdout;
    try {
      return JSON.parse(out);
    } catch (e) {
      return setup(`get output is not JSON: ${e.message}`);
    }
  };

  li("1", ["init", "--db", db, "--from", data]);
  li("1", ["refresh", "--db", db, "--dev-root", dev], [0, 2]);
  const before = get();
  if (!before.readme || !before.synopsis || !before.readme_sha256)
    setup("refresh 1 did not store README, synopsis and readme_sha256");

  const r2 = li("2", ["refresh", "--db", db, "--dev-root", dev], [0, 2]);
  const after = get();
  assert(r2.status === 2, `refresh 2 exited ${r2.status}, expected 2 (partial)`);
  assert(
    r2.stdout.includes(`${KEY}: readme failed (network); previous data kept`),
    "refresh 2 did not record the classified README gap"
  );
  assert(after.pushed_at === "2026-10-02T00:00:00Z", "refresh 2 did not store the new push");
  assert(after.synopsis === before.synopsis, `synopsis changed: ${JSON.stringify(after.synopsis)}`);
  assert(after.readme === before.readme, "README changed after a failed fetch");
  assert(after.readme_sha256 === before.readme_sha256, "readme_sha256 changed after a failed fetch");
}

function main() {
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "li-check-readme-"));
  let code = 0;
  try {
    body(tmp);
    console.log("readme-failure-keeps-synopsis: ok");
  } catch (e) {
    if (e instanceof AssertionFailed) {
      console.error(`readme-failure-keeps-synopsis: FAIL: ${e.message}`);
      code = 1;
    } else {
      console.error(`readme-failure-keeps-synopsis: setup error: ${e.message}`);
      code = 2;
    }
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
  process.exit(code);
}

main();
