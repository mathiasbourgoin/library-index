module S = Mcp_kit.Schema

let str d = S.string ~description:d ()
let int d = S.integer ~description:d ()
let bool d = S.bool ~description:d ()
let obj props = S.object_ ~properties:props ()
let raw = S.of_string_exn

(* ------------------------------------------------------------ inputs *)

let filters_doc =
  "Optional filters (all must hold): host (github|gitlab|local|whitehat), owner, \
   language, tier (1|2|fork|collab|inherited|whitehat), status \
   (active|dormant|stale|archived|unknown), domain (one of the index domains), private \
   (boolean), maintained_only (boolean: only the maintained copy of a library, not \
   archived), has_docs (boolean: with or without indexed documents), ownership \
   (own|participant|external: whose code it is; external = third-party libraries \
   registered for use)."

let filters = S.object_ ~description:filters_doc ()
let detail = str "Optional: brief | standard (default) | full"

let budget =
  int "Optional: cut the result to this many characters (100..1000000); sets truncated"

let kind = str "Optional: readme | doc | api (document kind)"

let search_in =
  obj
    [
      ("query", str "Free text; words are matched as quoted terms (AND, then OR fallback)");
      ("limit", int "Maximum hits, 1..50 (default 10)");
      ("filters", filters);
      ("detail", detail);
      ("budget_chars", budget);
      ("kind", str "Optional: readme | doc | api (kind of the excerpt shown per hit)");
    ]

let get_in = obj [ ("repo", str "Key (github:owner/name), owner/name, or bare name") ]

let list_in =
  obj
    [
      ("host", str "github | gitlab | local | whitehat");
      ("owner", str "Owner / namespace (case-insensitive)");
      ("language", str "Primary language (case-insensitive)");
      ("tier", raw {|{"description":"1 | 2 | fork | collab | inherited | whitehat"}|});
      ("status", str "active | dormant | stale | archived | unknown (effective status)");
      ("domain", str "One of the index domains, e.g. OCaml libraries");
      ("private", bool "Only private (true) or public (false) repos");
      ("maintained_only", bool "Only the maintained copy of each library, not archived");
      ("has_docs", bool "Only entries with (true) or without (false) documents");
      ("ownership", str "own | participant | external (whose code it is)");
      ("limit", int "Maximum rows, 1..1000 (default 100)");
      ("offset", int "Rows to skip (default 0)");
    ]

let resolve_in =
  obj
    [
      ("libraryName", str "Library name to look up, e.g. json-kit");
      ("query", str "Optional: what you want to do with it (improves ranking)");
      ("filters", filters);
      ("detail", detail);
      ("budget_chars", budget);
    ]

let query_docs_in =
  obj
    [
      ( "libraryId",
        str "Library id from resolve-library-id (an index key), or /owner/name" );
      ("query", str "What you need to know, e.g. 'install' or 'parse a trace file'");
      ("limit", int "Maximum excerpts, 1..20 (default 5)");
      ("kind", kind);
      ("filters", filters);
      ("detail", str "Optional: brief | standard (default) | full (full chunk text)");
      ("budget_chars", budget);
    ]

let annotate_in =
  obj
    [
      ("repo", str "Key, owner/name or bare name");
      ( "fields",
        S.object_
          ~description:
            "Curated fields to set; null clears. Allowed: what, interest, domain (one of \
             the index domains), consumers (array of strings, replaces the list), \
             status_override (active|dormant|stale|archived|unknown), maintained_side, \
             notes, rules (usage conventions shown first by query-docs). Max 4000 \
             characters each."
          () );
    ]

let usage_in =
  obj
    [
      ("repo", str "Key, owner/name or bare name of the library adopted");
      ("consumer", str "The project that now uses it");
      ("note", str "Optional: what it is used for");
    ]

let refresh_in =
  obj
    [
      ("repo", str "Key, owner/name or bare name of the entry to refresh");
      ("full", bool "Refetch the README, detail and documents even if nothing was pushed");
    ]

let register_repo_in =
  obj
    [
      ( "target",
        str
          "GitHub/GitLab URL (https://github.com/o/r, git@gitlab.com:g/p.git) or a local \
           checkout path" );
      ("ownership", str "Optional: own | participant | external (default: inferred)");
      ("confidential", str "Optional: generic | exclude-from-page | none (default none)");
      ("campaign", bool "Optional: a campaign repository (details withheld, no documents)");
      ("domain", str "Optional curated domain (one of the index domains)");
      ("notes", str "Optional curated notes");
      ("rules", str "Optional curated usage conventions, shown first by query-docs");
      ( "deep",
        bool
          "Optional: run the arch-index analysis after the fetch (no build; needs a \
           local checkout, arch-index and the language server on PATH)" );
    ]

let register_source_in =
  obj
    [
      ( "kind",
        str
          "github-user | github-org | github-affiliation | github-collab | \
           github-memberships | github-search | gitlab-user | gitlab-group | local-root"
      );
      ("name", str "User, organisation, group or directory (not for nameless kinds)");
      ("host", str "Optional host name");
      ("own", bool "Optional: one of the owner's own organisations");
      ("probe", bool "Optional: may be absent (a 404 is a gap, not an error)");
      ("include_subgroups", bool "Optional, GitLab groups (default true)");
      ("confidential", str "Optional: generic | exclude-from-page | none");
      ("campaign_prefix", bool "Optional: the name is a campaign owner prefix");
    ]

let unregister_repo_in =
  obj [ ("target", str "The registered URL, key (github:o/r) or checkout path") ]

let unregister_source_in =
  obj [ ("kind", str "The source kind"); ("name", str "The source name") ]

let list_sources_in = obj []

let find_capability_in =
  obj
    [
      ( "query",
        str
          "What you need, in words, e.g. 'parse a trace file' or 'token bucket rate \
           limit'" );
      ("limit", int "Maximum results, 1..50 (default 10)");
      ("filters", filters);
      ("detail", str "Optional: brief | standard (default) | full (whole doc)");
      ("budget_chars", budget);
    ]

let ownership_in = str "Optional: own | participant | external (whose code it is)"

let copies_in =
  obj
    [
      ("name", str "Library name (or owner/name, key, package alias)");
      ("ownership", ownership_in);
    ]

let explain_in = obj [ ("repo", str "Key (github:owner/name), owner/name, or bare name") ]

let whats_new_in =
  obj
    [
      ("since", str "RFC 3339 time or YYYY-MM-DD date (inclusive)");
      ("limit", int "Maximum repositories, 1..100 (default 20)");
      ("ownership", ownership_in);
    ]

let analyze_repo_in =
  obj
    [
      ("repo", str "Key, owner/name or bare name of a repository with a local checkout");
      ( "allow_build",
        bool
          "Optional: let an OCaml project be built (its own dune build) for the CMT \
           enricher (default false: never build)" );
    ]

(* ------------------------------------------------------------ outputs *)

let search_out =
  raw
    {|{"type":"object","required":["query","mode","count","hits"],
 "properties":{
  "query":{"type":"string"},
  "mode":{"type":"string","enum":["and","or","none"]},
  "count":{"type":"integer"},
  "truncated":{"type":"boolean"},
  "hits":{"type":"array","items":{"type":"object","required":["key","uri"],
   "properties":{
    "key":{"type":"string"},
    "full_name":{"type":["string","null"]},
    "what":{"type":["string","null"]},
    "score":{"type":"number"},
    "uri":{"type":["string","null"]},
    "excerpt":{"type":"object","required":["title","path","kind","snippet","uri"],
     "properties":{
      "title":{"type":"string"},"path":{"type":"string"},
      "kind":{"type":"string","enum":["readme","doc","api"]},
      "snippet":{"type":"string"},"uri":{"type":["string","null"]}}}}}}}}|}

let get_out =
  raw
    {|{"type":"object","required":["key","status","usage","uri","documents"],
 "properties":{
  "key":{"type":"string"},
  "status":{"type":"string"},
  "rules":{"type":["string","null"]},
  "usage":{"type":"array"},
  "uri":{"type":["string","null"]},
  "documents":{"type":"array","items":{"type":"object","required":["path","kind","uri"],
   "properties":{"path":{"type":"string"},"kind":{"type":"string"},
    "uri":{"type":["string","null"]}}}}}}|}

let list_out =
  raw
    {|{"type":"object","required":["total","limit","offset","count","repos"],
 "properties":{
  "total":{"type":"integer"},"limit":{"type":"integer"},"offset":{"type":"integer"},
  "count":{"type":"integer"},"export_dirty":{"type":"boolean"},
  "repos":{"type":"array","items":{"type":"object","required":["key","uri"],
   "properties":{"key":{"type":"string"},"uri":{"type":["string","null"]}}}}}}|}

let resolve_out =
  raw
    {|{"type":"object","required":["candidates"],
 "properties":{
  "hint":{"type":"string"},
  "truncated":{"type":"boolean"},
  "candidates":{"type":"array","items":{"type":"object",
   "required":["id","name","maintained_copy","uri"],
   "properties":{
    "id":{"type":"string"},"name":{"type":"string"},
    "full_name":{"type":"string"},"what":{"type":["string","null"]},
    "maintained_copy":{"type":["string","null"]},
    "score":{"type":"number"},"doc_count":{"type":"integer"},
    "uri":{"type":["string","null"]}}}}}}|}

let query_docs_out =
  raw
    {|{"type":"object","required":["library","excerpts"],
 "properties":{
  "library":{"type":"object","required":["id","name","uri"],
   "properties":{"id":{"type":"string"},"name":{"type":"string"},
    "uri":{"type":["string","null"]}}},
  "rules":{"type":"string"},
  "hint":{"type":"string"},
  "truncated":{"type":"boolean"},
  "excerpts":{"type":"array","items":{"type":"object","required":["title","path","uri"],
   "properties":{
    "title":{"type":"string"},"path":{"type":"string"},
    "kind":{"type":"string","enum":["readme","doc","api"]},
    "source":{"type":"string","enum":["local","api"]},
    "snippet":{"type":"string"},"text":{"type":"string"},
    "score":{"type":"number"},"uri":{"type":["string","null"]}}}}}}|}

let annotate_out =
  raw
    {|{"type":"object","required":["key","updated","exported"],
 "properties":{"key":{"type":"string"},
  "updated":{"type":"array","items":{"type":"string"}},
  "exported":{"type":"boolean"}}}|}

let usage_out =
  raw
    {|{"type":"object","required":["key","consumer","consumers","usage_count","exported"],
 "properties":{"key":{"type":"string"},"consumer":{"type":"string"},
  "consumers":{"type":"array","items":{"type":"string"}},
  "usage_count":{"type":"integer"},"exported":{"type":"boolean"}}}|}

let refresh_out =
  raw
    {|{"type":"object","required":["key","complete","gaps"],
 "properties":{"key":{"type":"string"},"complete":{"type":"boolean"},
  "gaps":{"type":"array","items":{"type":"string"}},
  "docs":{"type":"object"}}}|}

let register_repo_out =
  raw
    {|{"type":"object","required":["key","registered","fetch","status","uri","documents"],
 "properties":{
  "key":{"type":"string"},
  "registered":{"type":"string","enum":["created","updated"]},
  "ownership":{"type":["string","null"]},
  "provenance":{"type":"object"},
  "fetch":{"type":"object","required":["key","complete","gaps"],
   "properties":{"complete":{"type":"boolean"},
    "gaps":{"type":"array","items":{"type":"string"}}}},
  "deep":{"type":"object"},
  "status":{"type":"string"},
  "uri":{"type":["string","null"]},
  "documents":{"type":"array"}}}|}

let register_source_out =
  raw
    {|{"type":"object","required":["source","repos_added","repos_updated","gaps"],
 "properties":{
  "source":{"type":"object","required":["kind","name"],
   "properties":{"kind":{"type":"string"},"name":{"type":"string"}}},
  "registered":{"type":"string","enum":["created","updated"]},
  "complete":{"type":"boolean"},
  "listed":{"type":"integer"},
  "repos_added":{"type":"integer"},
  "repos_updated":{"type":"integer"},
  "docs":{"type":"object"},
  "gaps":{"type":"array","items":{"type":"string"}}}}|}

let unregister_repo_out =
  raw
    {|{"type":"object","required":["key","row","exported"],
 "properties":{"key":{"type":"string"},
  "row":{"type":"string","enum":["deleted","kept","absent"]},
  "exported":{"type":"boolean"}}}|}

let unregister_source_out =
  raw
    {|{"type":"object","required":["source","repos_removed","repos_kept","exported"],
 "properties":{"source":{"type":"object"},
  "repos_removed":{"type":"integer"},"repos_kept":{"type":"integer"},
  "exported":{"type":"boolean"}}}|}

let list_sources_out =
  raw
    {|{"type":"object","required":["sources","repos"],
 "properties":{
  "sources":{"type":"array","items":{"type":"object","required":["kind","name"],
   "properties":{"kind":{"type":"string"},"name":{"type":"string"}}}},
  "repos":{"type":"array","items":{"type":"object","required":["target","key","indexed"],
   "properties":{"target":{"type":"string"},"key":{"type":["string","null"]},
    "indexed":{"type":"boolean"}}}}}}|}

let library_json =
  {|{"type":"object","required":["id","name","uri"],
   "properties":{"id":{"type":"string"},"name":{"type":"string"},
    "what":{"type":["string","null"]},"uri":{"type":["string","null"]}}}|}

let find_capability_out =
  raw
    (Printf.sprintf
       {|{"type":"object","required":["query","mode","count","results"],
 "properties":{
  "query":{"type":"string"},
  "mode":{"type":"string","enum":["and","or","none"]},
  "count":{"type":"integer"},
  "truncated":{"type":"boolean"},
  "results":{"type":"array","items":{"type":"object",
   "required":["library","item","uri"],
   "properties":{
    "library":%s,
    "item":{"type":"object","required":["kind","qualified_path","file","source"],
     "properties":{
      "kind":{"type":"string"},"qualified_path":{"type":"string"},
      "signature":{"type":["string","null"]},"doc_excerpt":{"type":"string"},
      "file":{"type":"string"},"line":{"type":["integer","null"]},
      "source":{"type":"string","enum":["scan","arch-index","doc"]}}},
    "score":{"type":"number"},
    "uri":{"type":["string","null"]}}}}}}|}
       library_json)

let copies_out =
  raw
    {|{"type":"object","required":["name","count","groups"],
 "properties":{
  "name":{"type":"string"},"count":{"type":"integer"},"hint":{"type":"string"},
  "groups":{"type":"array","items":{"type":"object","required":["maintained","members"],
   "properties":{
    "maintained":{"type":["string","null"]},
    "members":{"type":"array","items":{"type":"object",
     "required":["key","host","archived","last_commit","pushed_at","local_paths","maintained"],
     "properties":{
      "key":{"type":"string"},"host":{"type":"string"},"archived":{"type":"boolean"},
      "last_commit":{"type":["string","null"]},"pushed_at":{"type":["string","null"]},
      "local_paths":{"type":"array"},"maintained":{"type":"boolean"},
      "uri":{"type":["string","null"]}}}}}}}}}|}

let analysis_json =
  {|{"type":"object","required":["status","at"],
   "properties":{"status":{"type":"string","enum":["ok","partial","skipped","failed"]},
    "cause":{"type":["string","null"]},"head_sha":{"type":["string","null"]},
    "at":{"type":"string"},"tool_version":{"type":["string","null"]}}}|}

let explain_out =
  raw
    (Printf.sprintf
       {|{"type":"object",
 "required":["key","name","uri","status","usage","provenance","copies","doc_count",
  "api_item_count","analysis"],
 "properties":{
  "key":{"type":"string"},"name":{"type":"string"},"uri":{"type":["string","null"]},
  "what":{"type":["string","null"]},"interest":{"type":["string","null"]},
  "status":{"type":"string"},"rules":{"type":["string","null"]},
  "usage":{"type":"array"},"provenance":{"type":"object"},
  "domain":{"type":["string","null"]},"tier":{"type":["string","null"]},
  "ownership":{"type":["string","null"]},
  "copies":{"type":"object","required":["count","maintained","keys"],
   "properties":{"count":{"type":"integer"},"maintained":{"type":["string","null"]},
    "keys":{"type":"array","items":{"type":"string"}}}},
  "doc_count":{"type":"integer"},
  "api_item_count":{"type":"object","required":["scan","arch-index","total"]},
  "last_commit":{"type":["string","null"]},
  "analysis":{"anyOf":[{"type":"null"},%s]}}}|}
       analysis_json)

let whats_new_out =
  raw
    {|{"type":"object","required":["since","total","count","repos"],
 "properties":{
  "since":{"type":"string"},"total":{"type":"integer"},"count":{"type":"integer"},
  "repos":{"type":"array","items":{"type":"object",
   "required":["key","name","uri","latest","changes"],
   "properties":{
    "key":{"type":"string"},"name":{"type":"string"},"what":{"type":["string","null"]},
    "uri":{"type":["string","null"]},"latest":{"type":"string"},
    "changes":{"type":"object","properties":{
     "metadata":{"type":"object"},
     "docs":{"type":"object","required":["count","paths"]}}}}}}}}|}

let analyze_repo_out =
  raw
    {|{"type":"object","required":["key","status","at","items","calls"],
 "properties":{
  "key":{"type":"string"},
  "status":{"type":"string","enum":["ok","partial","skipped","failed"]},
  "cause":{"type":["string","null"]},"head_sha":{"type":["string","null"]},
  "at":{"type":"string"},"tool_version":{"type":["string","null"]},
  "language":{"type":["string","null"]},
  "items":{"type":"integer"},"calls":{"type":"integer"}}}|}
