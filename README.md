# library-index

An index of the repositories one person owns, maintains, contributes to or may reuse: GitHub, GitLab, local
checkouts, a collaborator's repositories, and internal security-research ("whitehat") tools. It is stored in one
SQLite database with FTS5/BM25 full-text search over names, descriptions, READMEs, package synopses and curated
notes, plus the documents of the owner's Tier-1 libraries (READMEs, `docs/**/*.md`, or their public interface such
as `.mli` items), chunked by heading or by item.

Agents query it **before building infrastructure**, through an MCP server (stdio, built on mcp-kit) or the
`library-index` CLI. They record what they adopt with `record_usage` / `annotate`.

The code holds no name: who is indexed and which sources are listed come from the data directory's
`sources.toml` (see [examples/sources.example.toml](examples/sources.example.toml)).

## Install

The repository root is the opam package `library_index`: the library `library_index` (`lib/`) and the executable
`library-index` (`bin/`).

```bash
opam pin add library_index git+https://<this repository>.git#<commit>
```

`mcp-kit` 0.1.0 is not in the opam repository: pin `mcp-kit` from its upstream repository first
(`opam pin add -n mcp-kit <its git URL>#<commit>`; the code uses Server, Tool with make_with_progress,
Resource_template, Prompt, Completion and Mcp_kit_stdio).

## Build

```bash
opam switch create . ocaml-base-compiler.5.3.0 --no-install -y   # once
eval $(opam env --switch=. --set-switch)
opam pin add -n mcp-kit <its git URL>#<commit>                    # see Install
opam install . --deps-only --with-test
dune build && dune runtest
dune build @fmt                                                    # ocamlformat 0.28.1
```

## Data directory

The code holds no data. Everything that belongs to the user lives outside this repository, in one data
directory: `--data-dir DIR`, else `$LIBRARY_INDEX_DATA`, else the directory of an explicit database (`--db` /
`$LIBRARY_INDEX_DB`), else `$XDG_DATA_HOME/library-index` (`~/.local/share/library-index`).

| What | File in the data directory |
|---|---|
| Configuration: owner, sources, classification rules, rendering texts | `sources.toml` |
| Database (metadata, READMEs, documents, FTS, usage) | `library-index.db` (`--db` / `$LIBRARY_INDEX_DB` override it), regenerable |
| Curated fields + usage history | `curated.json`, exported on every curated write |
| Whitehat internal tools (generic descriptions only) | `whitehat_tools.json` |
| Text view | `INDEX.md` (or `render.index_md`) |
| Sensitive names for the publishability guard | `denylist.txt` (optional) |

The dated HTML page goes to `render.notes_dir` (none when unset). Keep the data directory in a private repository
if you want it versioned; rebuild a database with `library-index init` (curated data, offline), then
`library-index refresh` (network, GET only).

### Import semantics

`curated.json` is the source of truth and the database is regenerable. `library-index import` (`--from DIR`,
default the data directory) reads `curated.json` and `whitehat_tools.json` in one transaction:

- Every listed entry's curated fields are replaced by the file's and marked `curated` (`provenance` in `get`;
  see "No model, ever" under [Registering libraries](#registering-libraries)).
- Every listed entry's `usage` list is applied exactly: the library's usage rows become the file's records,
  compared as a multiset of `(consumer, note, at)` after redaction. A record edited or removed in the file is
  replaced or deleted in the database; an entry without `usage` ends with none. The output is
  `{"curated", "whitehat", "usage_added", "usage_removed"}`. Usage rows have no provenance of their own (only
  curated fields are `auto` or `curated`): they all come from `record_usage` or `curated.json`.
- Libraries absent from `curated.json` keep their row, curated fields and usage; fetched data is never deleted.
- While the last export failed (`export_dirty`, shown by `get`), `import` is refused and writes nothing: the
  database may then hold curated data the file lacks. Run `library-index export` first, or pass
  `--prefer-file` to let the files win: a listed entry's usage becomes the file's, but a curated field the
  file does not carry keeps the database's value (without the override, such a field is cleared).

### `sources.toml`

- `[owner]`: `display_name`, `author_regex` (counts local commits, `git rev-list --regexp-ignore-case --author`),
  `gitlab_me_names` and `gitlab_email_fragments` (GitLab contributor matching).
- `[render]`: `title`, `notes_dir`, `index_md`, `how_to` (bullet lines), `inherited_examples`,
  `contributes_column`, `whitehat_where`, `fixed_gaps`.
- `[[source]]`: `kind` = `github-user | github-org | github-affiliation | github-collab | github-memberships |
  github-search | gitlab-user | gitlab-group | local-root`, `name`, and the flags `own`, `probe`,
  `include_subgroups`, `confidential` (`generic | exclude-from-page | none`) and `campaign_prefix`.
- `[[repo]]`: single repositories, written by `register` (see [Registration](#registering-libraries)):
  `target` (URL or local path), `key`, `ownership` (`own | participant | external`), `confidential`,
  `campaign`, `deep`.
- `[classify]`: `mirror_name_regex`, `mirror_exempt_owners`, `major_contributor_owners` and
  `major_contributor_min_prs`, `org_member_owners`, `main_author_owners` and `main_author_min`, `derived_domains`,
  `campaign_owner_prefixes`, `campaign_exempt_names`, `domains` (the closed domain vocabulary).

The file is parsed strictly: an unknown key or kind, or a value of the wrong type, is an error naming it.

## CLI

Every command takes `--data-dir DIR` and `--db PATH`. Query commands print JSON.

```bash
library-index search "mcp server" --language OCaml --tier 1 --limit 5   # ranked, with snippets
library-index get json-kit             # key, owner/name or bare name; ambiguous names list candidates
library-index list --domain "OCaml libraries" --status active
library-index resolve json-kit "parse"  # library ids (as the MCP tool resolve-library-id)
library-index docs github:example-org/json-kit "install" --kind doc --limit 3   # excerpts (query-docs)
library-index find "parse a trace file" --limit 5   # which library and item already do it (find-capability)
library-index copies json-kit          # every copy (mirrors, forks, local checkouts) and the maintained one
library-index explain json-kit         # everything for a reuse decision
library-index whats-new 2026-09-01     # repositories pushed, committed or with changed documents since then
library-index analyze ~/dev/my-lib     # optional arch-index analysis of a local checkout (--allow-build)
library-index record-usage github:example-org/json-kit my-project --note "JSON helpers"
library-index annotate json-kit --set status_override=active --set notes="..."   # --clear FIELD, --json OBJ
library-index refresh                  # full incremental refresh (GET-only gh api / glab api + local git)
library-index refresh --repo json-kit  # one repository
library-index refresh --no-analysis    # full refresh without the arch-index hook
library-index render                   # INDEX.md + <notes_dir>/<date>-library-index.html
library-index register https://github.com/example-org/json-kit --ownership external   # register + fetch now
library-index register ~/dev/my-lib --domain "Other" --notes "..."   # a local checkout
library-index register-source github-org example-org                 # a source: listed and fetched now
library-index unregister github:example-org/json-kit                  # see "Registering libraries"
library-index unregister-source github-org example-org
library-index sources list
library-index init | import | export | import-raw | mcp
library-index import --prefer-file     # import although the DB holds unexported curated values
library-index init --force --discard-unexported   # recreate although they would be lost
```

Unexported curated data: when an export fails (read-only data directory, full disk), the DB keeps the
new curated values and records `export_dirty` (shown by `get` and `list`); the next curated write or
`library-index export` retries the export. Until one succeeds, `import` refuses (it would overwrite the
newer DB values with the older files) unless `--prefer-file`, and `init --force` refuses (it would delete
them) unless `--discard-unexported`. A field a `curated.json` entry does not carry always keeps the DB's
value: `import` never clears a curated field.

Search filters: `--host --owner --language --tier (1|2|fork|collab|inherited|whitehat)
--status (active|dormant|stale|archived|unknown) --domain --private --maintained-only --has-docs
--ownership (own|participant|external)` (`--private`, `--maintained-only` and `--has-docs` take `true|false`); `find`
takes them too. `search`, `resolve`, `docs` and `find` also take `--detail brief|standard|full` and
`--budget-chars N` (cut the JSON result to N characters, `truncated: true`); `search` and `docs` take
`--kind readme|doc|api`. Without these options the output is the core one.
Curated fields that `annotate` can set: `what, interest, domain, consumers, status_override, maintained_side, notes,
rules` (`rules`: the library's usage conventions, shown first by `query-docs`).

### Refresh semantics

- The sources are the `[[source]]` entries of `sources.toml`, listed in a fixed order: GitHub user, affiliation,
  organisations, collaborators, memberships, commit and PR search; GitLab user (owned, contributed, MRs) and
  groups; the local root.
- Metadata comes **only** from the APIs. Nothing is cloned.
- A README (and its synopsis) is fetched again **only** when `pushed_at` / `last_activity_at` changes, when none is
  stored, or with `--full`. Other details (contributors, last commit, fork parent, languages) are also refetched
  after 30 days.
- Campaign repositories never have their README or synopsis fetched or read from disk, and their description is
  withheld. A row is campaign when its owner starts with a configured campaign prefix, **or** when one of its local
  checkouts has a remote under such an owner. The flag is stored per row (`campaign` in `get`) and is sticky.
  One module, `lib/campaign.ml`, takes this decision for every write path. A registration that adds a campaign
  prefix or a confidential level reclassifies the rows already stored at once, also those its source does not
  list: their README, synopsis, documents, API items and pre-filled `what` are dropped, and every stored gap
  line naming them reads "a campaign repository".
- All stored text is made valid UTF-8 and goes through secret redaction first. A failing call is recorded as a
  classified gap (`auth | not-found | rate-limit | network | other`), the previous data is kept, and the command
  exits 2.

### Documents

- Tier-1 and explicitly registered (any ownership), non-campaign libraries get documents; other rows keep their
  README metadata only.
- Selection: the README, then `docs/**/*.md`, then the other root `*.md` (not LICENSE, CHANGELOG, CODE_OF_CONDUCT,
  CONTRIBUTING). Only when there is no markdown besides the README: the public API files chosen by the
  per-language extractors (below).
- Source: the local checkout (`git ls-files`, regular files only) when there is one, else the forge through
  GET-only `gh api` / `glab api` calls. A file whose blob id did not change is not fetched again.
- Chunks: markdown is split at h1–h3 headings; an API file gives one chunk per public item, titled
  `kind qualified_path` (`val Json.parse`, `fn crate::io::read`), with its signature and doc; each chunk is at
  most 2000 characters.
- Caps: 256 KiB per file, 200 files and 4 MiB per repository, 60 forge file fetches per repository and run.
- Whatever the selection above, every file the extractors select is also scanned into **API items** (one row per
  public item: kind, qualified path, signature, doc, file, line; full-text indexed), re-extracted only when its
  content changes. `find-capability` ranks them with the documentation sections.
- The root manifests are read too: their package names become the repository's `aliases` (indexed like its name),
  so `resolve-library-id` finds a library by its package name (an opam package named differently from its
  repository, an npm scope, a Cargo crate, …).

### API extractors

Lightweight source scanning, no build and no toolchain. A language is detected by its manifest (`dune-project` /
`*.opam`, `package.json` / `tsconfig.json`, `Cargo.toml`, `go.mod`, `pyproject.toml` / `setup.py`) or the
repository's primary language. `vendor/`, `node_modules/`, `_build/`, `_opam/`, `target/`, `dist/`, `build/`,
test directories, `examples/` and generated files are skipped. An unknown language gets docs and README only.

| Language | Files | Items |
|---|---|---|
| OCaml | `*.mli`; a module's `.ml` when it has no `.mli` | `val`, `type`, `module`, `exception`, `external`, … with `(** *)` docs |
| TypeScript / JavaScript | `*.d.ts`, plus the `package.json` `types` / `exports` / `main` entries | exported declarations with JSDoc/TSDoc |
| Rust | the `src/` files of each crate (not `main.rs`, `bin/`) | `pub` items (and `pub fn` of `impl` blocks) with `///` / `//!` docs |
| Go | non-test files outside `internal/`, `cmd/` | exported identifiers with their comments, the package doc |
| Python | modules (a `.pyi` stub replaces its module) | `__all__`, else public `def` / `class`, with docstrings |
| C / C++ | headers at the root or under an `include/` directory | prototypes, inline functions, `struct` / `union` / `enum` / `class`, `typedef`, `#define` (not include guards), with `/** */` / `///` docs; C++ `namespace`s qualify names |
| Quint | `*.qnt` | `module` and its `def` / `val` / `action` / `temporal` / `type` / `const` / `var` / `run`, with the comment above |
| Rocq / Coq | `*.v` (detected by `_CoqProject`, a `dune-project` using `coq`/`rocq`, or the primary language) | `Definition`, `Fixpoint`, `Inductive`, `Theorem`, `Lemma`, `Record`, `Class`, `Instance`, `Module`, … (not `Local`), with `(** *)` docs |

### Optional arch-index analysis

`analyze-repo` (MCP) / `library-index analyze REPO` runs the external
arch-index LSP indexer (`arch_index_cli`, else its `arch-index` wrapper) on a repository's
local checkout and imports the exported functions and call edges of its comment_db output as API items of source
`arch-index`. library-index has no LSP or symbol extraction of its own.

- Preconditions, recorded with their cause when missing (`skipped`): a local checkout (never cloned), arch-index on
  `PATH`, a language arch-index supports (OCaml, TypeScript, Rust, Go, Python) and its language server on `PATH`
  (`ocamllsp`, `typescript-language-server`, `rust-analyzer`, `gopls`, `pylsp`).
- OCaml's CMT enrichment needs a built project. Nothing is built unless `allow_build` / `--allow-build`, which runs
  the project's own `dune build`; without it the analysis runs with `--no-enrich` and is `partial`.
- arch-index runs with `EPURE_ARCH_INDEX_TIMEOUT_S=300` under a 420 s overall timeout, into a temporary file under
  the data directory that is removed afterwards.
- Each run records `analysis(status ok|partial|skipped|failed, cause, head_sha, at, tool_version)`, shown by
  `explain`. An empty call graph is `partial` (`empty call graph (LSP prepareCallHierarchy)`); a failed run, no
  function or an unsupported output is `failed` and keeps the previous items. Nothing here fails a refresh or a
  tool call.
- `register-repo` with `deep: true` runs it after the fetch (no build). A full CLI refresh runs it for the Tier-1
  repositories of your own whose local checkout's HEAD changed since their last analysis (`--no-analysis` turns
  this off).

## MCP server

Run `library-index mcp` (stdio). It offers these tools:

| Tool | Arguments | Result |
|---|---|---|
| `search` | `query, limit?, filters?, detail?, budget_chars?, kind?` | ranked repositories with `score`, `snippet`, `uri` and, when one of its documents matches, an `excerpt` |
| `get` | `repo` | full entry including README, rules, usage, effective status, `uri` and its document list |
| `list` | `host? owner? language? tier? status? domain? private? maintained_only? has_docs? ownership? limit? offset?` | `{total, repos}` |
| `resolve-library-id` | `libraryName, query?, filters?, detail?, budget_chars?` | `{candidates: [{id, name, full_name, what, maintained_copy, score, doc_count, uri}], hint?}` |
| `query-docs` | `libraryId, query, limit?, kind?, filters?, detail?, budget_chars?` | `{library, rules?, excerpts: [{title, path, kind, source, snippet, score, uri}], hint?}` |
| `record_usage` | `repo, consumer, note?` | appends usage, adds the consumer, re-exports `curated.json` |
| `annotate` | `repo, fields` | sets or clears curated fields (`null` clears), re-exports |
| `refresh` | `repo, full?` | refreshes one repository and its documents (45 s deadline); a partial refresh is `isError` naming the classified gaps. The full refresh is CLI-only |
| `register-repo` | `target, ownership?, confidential?, campaign?, domain?, notes?, rules?, deep?` | registers (or updates) a repository and fetches it at once; the entry plus `registered: created\|updated` and the `fetch` report |
| `register-source` | `kind, name?, host?, own?, probe?, include_subgroups?, confidential?, campaign_prefix?` | registers (or updates) a source, lists it and fetches its repositories; `{source, repos_added, repos_updated, gaps}` |
| `unregister-repo` | `target` | removes the registration; `{key, row: deleted\|kept\|absent}` |
| `unregister-source` | `kind, name?` | removes the source; `{source, repos_removed, repos_kept}` |
| `list-sources` | — | the configured sources and registered repositories |
| `find-capability` | `query, limit?, filters?, detail?, budget_chars?` | `{results: [{library: {id, name, what, uri}, item: {kind, qualified_path, signature, doc_excerpt, file, line, source}, score, uri}]}`: which library and which item already provide a capability |
| `copies` | `name` | `{groups: [{maintained, members: [{key, host, archived, last_commit, pushed_at, local_paths, maintained}]}]}` |
| `explain` | `repo` | what, interest, effective status, rules, consumers, usage history, domain, tier, ownership, provenance, copies summary, document and API item counts, last commit, last analysis |
| `whats-new` | `since, limit?` | repositories changed on or after `since` (RFC 3339 or `YYYY-MM-DD`), newest first, with what changed |
| `analyze-repo` | `repo, allow_build?` | the optional arch-index analysis (see above); `{status, cause, head_sha, items, calls, …}` |

Every tool declares an `outputSchema` and returns `structuredContent`. Every hit carries a `libindex://` URI, served
by two resource templates (`libindex://repo/{host}/{owner}/{name}` and
`libindex://doc/{host}/{owner}/{name}/{path}`), each completing its variables (`completion/complete`: hosts,
owners, names and document paths from the database, at most 100). Whitehat entries and campaign documents are not
exposed.

Two prompts: `before-building(capability)` walks an agent through the reuse-first check (`find-capability`,
`search` / `resolve-library-id`, `explain`, `query-docs`) and asks for its decision; `record-usage(repo, consumer,
note?)` asks it to call `record_usage`.

`resolve-library-id` and `query-docs` have the shape of the context7 tools but are answered entirely from the local
database: library-index never calls any service other than `gh` / `glab`. When a library is not in the index,
`resolve-library-id` returns no candidate and a hint pointing at context7; the agent decides.

README, document and description text is untrusted repository content. The server says so in its instructions.

### Registering libraries

When you start using a new library or project (yours, or a third-party one the external documentation service does
not cover well), register it first, then query it with `query-docs`:

- `register-repo` / `library-index register <url|path>` adds or updates one `[[repo]]` entry of `sources.toml`
  (atomically, under the refresh lock and a `.sources.toml.lock` in the data directory; every other line of the
  file, and the comments inside the entry, are kept) and fetches the repository at once: metadata, README and
  documents, through GET-only API calls or the local checkout (never a clone). A GitHub/GitLab URL that does not
  exist registers nothing; a URL carrying credentials (`https://user:token@…`) is stored as its plain canonical URL,
  and nothing written to `sources.toml` ever holds a credential. Calling it again with other flags updates the
  entry; absent flags keep their values (`--no-campaign`, `--no-deep` clear them on the CLI). `domain`, `notes`
  and `rules` become curated fields. `deep` runs the arch-index analysis after the fetch (never building the
  project).
- `register-source` / `library-index register-source <kind> <name>` adds or updates a `[[source]]` entry (absent
  flags keep their stored values), runs that source's listing only and fetches its repositories incrementally,
  classifying them with the whole configuration (campaign prefixes of every source, local checkouts of every
  local root). A `local-root` that is not an existing directory is refused. Through MCP it stops after 120 s with a
  partial result; `library-index refresh` continues.
- `unregister-repo` / `unregister-source` remove the entry. A repository is deleted (with its documents; no
  tombstone; `curated.json` is rewritten) only when the removed entry is all that keeps it — a registration that
  created the row, or a source no other source or registration shares — and nobody curated it (no curated field,
  no usage). Every other repository keeps its entry and curated data; only the registration and its overrides, or
  the source's labels, go.
- With a progress token (MCP), the registration tools report progress.

Add `.sources.toml.lock`, `.refresh.lock`, `.export.lock` and `*.db*` to the data directory's `.gitignore` when
it is a git repository.

**Ownership.** Every entry is `own` (role owner or main author), `participant` (any other role) or `external` (no
participation evidence), unless its registration says otherwise. External libraries are searchable and queryable
like the others, but never counted or shown as the owner's: they are left out of the rendered sections 1–2 and the
Tier-1 counts and listed in section 8, "External libraries registered for use". Every query tool takes the
`ownership` filter. `confidential = "generic"` (or `campaign`) makes a registered repository a campaign row;
`"exclude-from-page"` keeps it out of the rendered views.

**No model, ever.** Nothing in library-index calls a language model. `what` and `interest` are pre-filled
deterministically when they are empty: `what` from the description, else the package synopsis, else the README's
first paragraph; `interest` from a fixed template (`Own OCaml code (Libraries)`). Such values are marked `auto`
(see `provenance` in `get`): a refresh replaces them, the `curated.json` export leaves them out, and a value set
with `annotate` (or imported) is `curated` and never overwritten.

### Registering the MCP server

```bash
scripts/register-mcp.sh --install --dry-run              # show what would happen
scripts/register-mcp.sh --install --data-dir DIR         # install ~/.local/bin/library-index, then register
```

This runs `claude mcp add --transport stdio --env LIBRARY_INDEX_DATA=<data dir> --scope user library-index -- <bin>
mcp` and appends an `[mcp_servers.library-index]` table to `~/.codex/config.toml` (`--bin` and `--data-dir` are made
absolute and TOML-escaped). The script is idempotent: a registration that already exists is reported and left
unchanged.

## Confidentiality

- Never store or print tokens. gh and glab authenticate themselves, and subprocess stderr and env are never echoed.
- Whitehat tools are described generically: campaign names are allowed, not targets' internals, findings or exploit
  details.
- Nothing secret goes into the rendered views.
- `test/test_guard.ml` scans this tree (except the root `LICENSE`, whose copyright notice names its holder) for the
  names in the data directory's `denylist.txt` and `sources.toml`; it reports "skipped: no data dir" when there is
  none.

## Tests and checks

`dune runtest` runs the alcotest suites; every fixture is synthetic (`test/fixtures`, configuration
`test/fixtures/sources.test.toml`). Single groups run as `dune exec test/<suite>.exe -- test <suite> <group>`.
Scripts: `scripts/mcp-smoke.sh` (stdio smoke test) and `scripts/check-register-dryrun.sh`. `checks/*.js` are
self-contained node ratchet checks (exit 0 fixed, 1 bug present, ≥ 2 setup error; temp HOME, fake gh/glab, no
network). Four of them are invariant harnesses (spec "Minimal-freeze"): `confidentiality-closure.js`
(seeded operation sequences, every CLI/MCP/render/export surface after every step; about 4 minutes),
`curated-no-loss.js` (seeded sequences against a reference model of the curated state), `migration-concurrency.js`
(50 concurrent first opens of a schema-3 DB; node ≥ 22.5) and `single-campaign-decision.js` (structural). The
seeded ones take `LI_SEQS=N`, `LI_ONLY=<sequence>`, `LI_JOBS=N`, `LI_KEEP=1` (and `LI_DUMP=<dir>` for the first).
The checks build `bin/main.exe` from the nearest dune project: set `LIBRARY_INDEX_SWITCH=<dir>` when the opam
switch is not in an ancestor `_opam`, or `LIBRARY_INDEX_BIN=<exe>`.

## License

ISC, see [LICENSE](LICENSE).
