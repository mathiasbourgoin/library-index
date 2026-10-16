(lang dune 3.17)

(name library_index)

(generate_opam_files true)

(authors "library-index contributors")

(maintainers "library-index maintainers")

(license ISC)

(package
 (name library_index)
 (synopsis "SQLite/FTS5 index of one person's repositories with a CLI and an MCP server")
 (description
  "library-index keeps one SQLite database (FTS5) of repositories, curated notes and usage, exposes it through a CLI and an mcp-kit stdio MCP server, and renders INDEX.md and the notes HTML page from it.")
 (depends
  (ocaml
   (>= 5.3.0))
  (yojson
   (>= 3.0))
  (sqlite3
   (>= 5.4))
  (cmdliner
   (>= 2.0))
  (re
   (>= 1.11.0))
  (otoml
   (>= 1.0.5))
  (ptime
   (>= 1.1.0))
  (mcp-kit
   (= 0.1.0))
  (alcotest :with-test)))
