#!/usr/bin/env node
// CHECK-46 (spec "Minimal-freeze", INV-D, FR-131, AC-46): one campaign decision for every
// write path.
//
// A structural scan of the OCaml sources under lib/ and bin/ (comments and string literals
// removed). The campaign predicates below may be composed only in the decision module
// lib/campaign.ml; a predicate may also appear in the one module that defines it (its home
// module, listed with it). Any other module that uses one fails, named with file:line.
// The decision module must exist and expose `decide`, and every write path that stores a
// campaign flag (WRITE_PATHS) must reference it.
//
// Exit: 0 = pass, 1 = assertion fired, >= 2 = setup error. Needs no build.
"use strict";

const fs = require("fs");
const path = require("path");

const DECISION = "lib/campaign.ml";

// [pattern, home modules that define the predicate (besides the decision module)].
const PREDICATES = [
  // The composed rules (formerly in Classify): owner prefix, remote-key owner, row rule.
  [/\bClassify\.(is_campaign|campaign_flag|row_is_campaign|key_owner)\b/, []],
  // The checkouts the rule ties to a row, and a forced checkout.
  [/\bLocal_keys\.(for_row|forced_row)\b/, []],
  // A registration flagged campaign.
  [/\bIdentity\.is_registered_campaign\b/, []],
  // Marking outside the decision module (the old Registry.mark_campaign).
  [/\bRegistry\.mark_campaign\b/, []],
  // The checkout facts the rule reads: defined by Local_scan, indexed by Local_keys.
  [/\bremotes_unreadable\b/, ["lib/local_scan.ml", "lib/local_keys.ml"]],
  [/\bconfidential_roots\b/, ["lib/local_keys.ml", "lib/identity.ml"]],
  [/\.campaign_prefixes\b/, ["lib/identity.ml"]],
  [/\bcheckout_remote_keys\b/, ["lib/local_scan.ml"]],
  // Bare definitions of the old composed predicates outside the decision module.
  [/^\s*let\s+(campaign_of|checkout_campaign|campaign_now|with_campaign_flags|campaign_flag|row_is_campaign|mark_campaign)\b/m, []],
];

// Every write path that stores a row's campaign flag must take it from the decision
// module (a module that stops referencing it has grown its own rule, or lost the rule).
const WRITE_PATHS = [
  "lib/repo_refresh.ml", "lib/register.ml", "lib/source_reg.ml", "lib/import_raw.ml",
  "lib/analysis.ml", "lib/curated_io.ml", "lib/registry.ml",
];

function root() {
  let d = path.resolve(__dirname, "..");
  for (let i = 0; i < 4; i++) {
    if (fs.existsSync(path.join(d, "dune-project")) && fs.existsSync(path.join(d, "lib"))) return d;
    d = path.dirname(d);
  }
  return null;
}

// OCaml source with comments and string literals blanked (newlines kept, so line numbers
// stay right). Handles nested comments, "..." with escapes, {|...|} / {id|...|id} and
// character literals such as '"'.
function strip(src) {
  let out = "";
  let i = 0;
  let depth = 0;
  const n = src.length;
  const blank = (s) => s.replace(/[^\n]/g, " ");
  while (i < n) {
    const c = src[i];
    if (src.startsWith("(*", i)) {
      depth++;
      out += "  ";
      i += 2;
      continue;
    }
    if (depth > 0) {
      if (src.startsWith("*)", i)) {
        depth--;
        out += "  ";
        i += 2;
      } else {
        out += c === "\n" ? "\n" : " ";
        i++;
      }
      continue;
    }
    if (c === '"') {
      let j = i + 1;
      while (j < n && src[j] !== '"') j += src[j] === "\\" ? 2 : 1;
      out += blank(src.slice(i, j + 1));
      i = j + 1;
      continue;
    }
    const q = /^\{([a-z_]*)\|/.exec(src.slice(i, i + 20));
    if (q) {
      const close = `|${q[1]}}`;
      const j = src.indexOf(close, i + q[0].length);
      const end = j < 0 ? n : j + close.length;
      out += blank(src.slice(i, end));
      i = end;
      continue;
    }
    if (c === "'" && /^'(\\.|[^\\'])'/.test(src.slice(i, i + 4))) {
      const m = /^'(\\.|[^\\'])'/.exec(src.slice(i, i + 4));
      out += blank(m[0]);
      i += m[0].length;
      continue;
    }
    out += c;
    i++;
  }
  return out;
}

function main() {
  const NAME = "single-campaign-decision";
  const r = root();
  if (!r) {
    console.error(`${NAME}: setup error: no dune project with lib/ above the check`);
    process.exit(2);
  }
  const files = ["lib", "bin"].flatMap((d) =>
    fs.existsSync(path.join(r, d))
      ? fs.readdirSync(path.join(r, d)).filter((f) => f.endsWith(".ml")).map((f) => `${d}/${f}`)
      : []
  );
  const findings = [];
  const decision = path.join(r, DECISION);
  if (!fs.existsSync(decision)) findings.push(`${DECISION}: the decision module does not exist`);
  else if (!/^\s*let\s+decide\b/m.test(strip(fs.readFileSync(decision, "utf8"))))
    findings.push(`${DECISION}: no \`decide\` function`);
  for (const f of WRITE_PATHS) {
    const p = path.join(r, f);
    if (fs.existsSync(p) && !/\bCampaign\./.test(strip(fs.readFileSync(p, "utf8"))))
      findings.push(`${f}: a write path that does not take its campaign flag from ${DECISION}`);
  }
  for (const f of files.sort()) {
    if (f === DECISION) continue;
    const lines = strip(fs.readFileSync(path.join(r, f), "utf8")).split("\n");
    for (const [re, homes] of PREDICATES) {
      if (homes.includes(f)) continue;
      const line = new RegExp(re.source, re.flags.replace("m", ""));
      lines.forEach((l, i) => {
        const m = line.exec(l);
        if (m) findings.push(`${f}:${i + 1}: ${m[0].trim()} composed outside ${DECISION}`);
      });
    }
  }
  if (findings.length) {
    console.error(`${NAME}: FAIL: ${findings.length} use(s) of the campaign predicates outside the decision module`);
    for (const x of findings) console.error(`  ${x}`);
    process.exit(1);
  }
  console.log(`${NAME}: ok (${files.length} files scanned)`);
  process.exit(0);
}

main();
