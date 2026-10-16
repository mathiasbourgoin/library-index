(* Per-language API extractors (CHECK-36, AC-36, FR-120) and package aliases (S12
   deviation 5): one synthetic fixture per core language, skipped directories, an unknown
   language giving docs only, items from several languages found through the chunk index,
   and a library resolved by its package name. *)

open Library_index
open Test_support.Support
module F = Test_support.Doc_fixture
module Fake_forge = Test_support.Fake_forge

let strs = Alcotest.(list string)

let titles ~path text =
  List.map (fun (c : Chunk.t) -> c.title) (Extract.chunks ~path text)

let contains s sub = Re.execp (Re.compile (Re.str sub)) s

let doc_of ~path text name =
  match
    List.find_opt (fun (i : Api_item.item) -> i.name = name) (Extract.items ~path text)
  with
  | Some i -> i.doc
  | None -> "<missing " ^ name ^ ">"

let ctx_of ?language ?(manifests = []) paths = { Api_item.paths; manifests; language }

(* ------------------------------------------------------------ fixtures *)

let ocaml_ml =
  "(** Parse a widget description. *)\n\
   let parse_widget s = s\n\n\
   type widget = { name : string }\n\n\
   let _hidden = 1\n\
   let () = ()\n\n\
   module Registry = struct end\n"

let ts_index =
  "/**\n\
  \ * Parse a widget description.\n\
  \ */\n\
   export function parseWidget(s: string): Widget {\n\
  \  return {} as Widget;\n\
   }\n\n\
   export interface Widget { name: string }\n\
   const internal = 1;\n\
   export default class Renderer {}\n"

let ts_dts =
  "/** A gauge value. */\n\
   declare function gauge(v: number): string;\n\
   export type Level = number;\n"

let rust_lib =
  "//! Widgets for terminals.\n\
   pub mod parse;\n\n\
   /// Parse a widget description.\n\
   #[inline]\n\
   pub fn parse_widget(s: &str) -> Widget {\n\
  \    Widget\n\
   }\n\n\
   pub(crate) fn hidden() {}\n\n\
   pub struct Widget;\n\n\
   impl Widget {\n\
  \    /// Render it.\n\
  \    pub fn render(&self) -> String {\n\
  \        String::new()\n\
  \    }\n\
   }\n"

let go_doc = "// Package widget parses widget descriptions.\npackage widget\n"

let go_src =
  "package widget\n\n\
   // ParseWidget parses a widget description.\n\
   func ParseWidget(s string) Widget { return Widget{} }\n\n\
   // Widget is a widget.\n\
   type Widget struct{}\n\n\
   // Render draws it.\n\
   func (w Widget) Render() string { return \"\" }\n\n\
   func hidden() {}\n\n\
   const (\n\
   \t// MaxWidgets is the limit.\n\
   \tMaxWidgets = 10\n\
   \tminWidgets = 1\n\
   )\n\n\
   // Version is the version.\n\
   var Version = \"1\"\n"

let py_init =
  "__all__ = [\"parse_widget\", \"VERSION\"]\n\n\
   VERSION = \"1\"\n\n\
   def parse_widget(s):\n\
  \    \"\"\"Parse a widget description.\"\"\"\n\
  \    return s\n\n\
   def helper():\n\
  \    pass\n"

let py_render =
  "def render(w,\n\
  \           width=80):\n\
  \    \"\"\"Render a widget.\n\n\
  \    Returns text.\n\
  \    \"\"\"\n\
  \    return \"\"\n\n\
   def _private():\n\
  \    pass\n\n\
   class Canvas:\n\
  \    \"\"\"A canvas.\"\"\"\n"

(* ------------------------------------------------------------ extractors *)

let ocaml () =
  Alcotest.check strs ".ml without .mli: top-level items"
    [ "let Widget.parse_widget"; "type Widget.widget"; "module Widget.Registry" ]
    (titles ~path:"src/widget.ml" ocaml_ml);
  Alcotest.(check string)
    "doc above" "Parse a widget description."
    (doc_of ~path:"src/widget.ml" ocaml_ml "parse_widget");
  Alcotest.check strs ".mli items"
    [
      "module Itf";
      "val Itf.parse_file";
      "type Itf.t";
      "module Itf.Sub";
      "exception Itf.Bad";
      "external Itf.fast";
      "val Itf.( >>= )";
      "type Itf.box";
    ]
    (titles ~path:"src/itf.mli" F.itf_mli);
  Alcotest.check strs "selection: .mli, plus .ml without .mli, skipped dirs never"
    [ "lib/a.mli"; "lib/b.ml" ]
    (Extract_ocaml.extractor.select
       (ctx_of
          [
            "dune-project";
            "lib/a.mli";
            "lib/a.ml";
            "lib/b.ml";
            "_build/x.ml";
            "test/t.ml";
            "vendor/v.mli";
          ]))

let typescript () =
  Alcotest.check strs "exported declarations"
    [ "function parseWidget"; "interface Widget"; "class Renderer" ]
    (titles ~path:"src/index.ts" ts_index);
  Alcotest.(check string)
    "JSDoc" "Parse a widget description."
    (doc_of ~path:"src/index.ts" ts_index "parseWidget");
  Alcotest.check strs ".d.ts ambient declarations"
    [ "function types.gauge"; "type types.Level" ]
    (titles ~path:"types.d.ts" ts_dts);
  let pkg = {|{"name":"widget-ts","types":"dist/index.d.ts","main":"dist/index.js"}|} in
  Alcotest.check strs "package.json entry (dist → src) and .d.ts, node_modules skipped"
    [ "src/index.ts"; "types.d.ts" ]
    (Extract_ts.extractor.select
       (ctx_of
          ~manifests:[ ("package.json", pkg) ]
          [
            "package.json";
            "src/index.ts";
            "src/util.ts";
            "types.d.ts";
            "node_modules/x/index.d.ts";
            "dist/y.d.ts";
          ]))

let rust () =
  Alcotest.check strs "pub items, impl methods, module doc"
    [
      "mod crate";
      "mod crate::parse";
      "fn crate::parse_widget";
      "struct crate::Widget";
      "fn crate::Widget::render";
    ]
    (titles ~path:"src/lib.rs" rust_lib);
  Alcotest.(check string)
    "/// doc across an attribute" "Parse a widget description."
    (doc_of ~path:"src/lib.rs" rust_lib "parse_widget");
  Alcotest.check strs "module path" [ "fn crate::parse::tokens" ]
    (titles ~path:"src/parse.rs" "/// Tokenise.\npub fn tokens() {}\n");
  Alcotest.check strs "crate files only: no main.rs, bin/, target/"
    [ "src/lib.rs"; "src/parse.rs" ]
    (Extract_rust.extractor.select
       (ctx_of
          [
            "Cargo.toml";
            "src/lib.rs";
            "src/parse.rs";
            "src/main.rs";
            "src/bin/cli.rs";
            "target/debug/x.rs";
          ]))

let go () =
  Alcotest.check strs "exported identifiers"
    [
      "func widget.ParseWidget";
      "type widget.Widget";
      "func widget.Widget.Render";
      "const widget.MaxWidgets";
      "var widget.Version";
    ]
    (titles ~path:"widget.go" go_src);
  Alcotest.(check string)
    "comment above" "ParseWidget parses a widget description."
    (doc_of ~path:"widget.go" go_src "ParseWidget");
  Alcotest.check strs "package doc" [ "package widget" ] (titles ~path:"doc.go" go_doc);
  Alcotest.check strs "generated file: nothing" []
    (titles ~path:"gen.go" ("// Code generated by a tool. DO NOT EDIT.\n" ^ go_src));
  Alcotest.check strs "no tests, internal/, cmd/, vendor/, generated names"
    [ "doc.go"; "widget.go" ]
    (Extract_go.extractor.select
       (ctx_of
          [
            "go.mod";
            "doc.go";
            "widget.go";
            "widget_test.go";
            "internal/x.go";
            "cmd/tool/main.go";
            "vendor/dep/dep.go";
            "zz_generated.deepcopy.go";
          ]))

let python () =
  Alcotest.check strs "__all__ restricts the items"
    [ "const widget.VERSION"; "def widget.parse_widget" ]
    (titles ~path:"src/widget/__init__.py" py_init);
  Alcotest.check strs "public defs and classes, multi-line header"
    [ "def widget.render.render"; "class widget.render.Canvas" ]
    (titles ~path:"src/widget/render.py" py_render);
  Alcotest.(check string)
    "docstring" "Render a widget.\n\nReturns text."
    (doc_of ~path:"src/widget/render.py" py_render "render");
  Alcotest.check strs "stub replaces its module; tests, docs, private modules skipped"
    [ "src/widget/__init__.py"; "src/widget/render.pyi" ]
    (Extract_python.extractor.select
       (ctx_of
          [
            "pyproject.toml";
            "src/widget/__init__.py";
            "src/widget/render.py";
            "src/widget/render.pyi";
            "src/widget/_impl.py";
            "tests/test_x.py";
            "docs/conf.py";
            "setup.py";
          ]))

let detection () =
  let names ctx =
    List.map (fun (e : Api_item.extractor) -> e.language) (Extract.detected ctx)
  in
  Alcotest.check strs "manifests"
    [ "ocaml"; "typescript"; "rust"; "go"; "python" ]
    (names
       (ctx_of
          [ "dune-project"; "package.json"; "Cargo.toml"; "go.mod"; "pyproject.toml" ]));
  Alcotest.check strs "primary language" [ "go" ]
    (names (ctx_of ~language:"Go" [ "x.go" ]));
  Alcotest.check strs "unknown language: nothing" []
    (names (ctx_of ~language:"COBOL" [ "main.cbl" ]));
  Alcotest.(check (list (pair string string)))
    "unknown language: README only"
    [ ("README.md", "readme") ]
    (List.map
       (fun (p, k) -> (p, Doc_select.kind_name k))
       (Doc_select.select ~language:"COBOL"
          [ "README.md"; "main.cbl"; "copybooks/lib.cpy"; "notes.txt" ]));
  let long =
    String.concat "\n" (List.init 400 (fun i -> Printf.sprintf "   line %d of the doc" i))
  in
  let cs = Extract.chunks ~path:"a.mli" ("(** " ^ long ^ " *)\nval x : int\n") in
  Alcotest.(check bool)
    "excerpt capped at 2000" true
    (List.for_all (fun (c : Chunk.t) -> Utf8.length c.text <= 2000) cs)

(* ------------------------------------------------------------ extras (CHECK-37) *)

let c_header =
  "#ifndef WIDGET_H\n\
   #define WIDGET_H\n\n\
   #include <stddef.h>\n\n\
   /** Maximum widgets per canvas. */\n\
   #define WIDGET_MAX 64\n\n\
   #ifdef __cplusplus\n\
   extern \"C\" {\n\
   #endif\n\n\
   /**\n\
  \ * Parse a widget description.\n\
  \ * Returns NULL on error.\n\
  \ */\n\
   widget_t *widget_parse(const char *s,\n\
  \                       size_t len);\n\n\
   /// A widget.\n\
   typedef struct widget {\n\
  \  int width; /* not an item */\n\
  \  int (*draw)(struct widget *w);\n\
   } widget_t;\n\n\
   /// Draw callback.\n\
   typedef int (*widget_draw_fn)(widget_t *w);\n\n\
   enum widget_kind { WIDGET_BOX, WIDGET_TEXT };\n\n\
   struct canvas;\n\n\
   extern int widget_count;\n\n\
   /* Plain comment: not a doc. */\n\
   static inline int widget_area(const widget_t *w) {\n\
  \  if (w) { return w->width; }\n\
  \  return 0;\n\
   }\n\n\
   DECLARE_WIDGET_HOOK(resize);\n\n\
   #ifdef __cplusplus\n\
   }\n\
   #endif\n\
   #endif\n"

let cpp_header =
  "#pragma once\n\
   namespace acme {\n\
   namespace gauge {\n\
   /// A gauge.\n\
   class Gauge : public Base {\n\
   public:\n\
  \  void set(double v);\n\
   };\n\
   /// Render a gauge to text.\n\
   std::string render(const Gauge &g);\n\
   }  // namespace gauge\n\
   }  // namespace acme\n"

let quint_spec =
  "// A token bucket.\n\
   module bucket {\n\
  \  /// The capacity.\n\
  \  const CAPACITY: int\n\n\
  \  var tokens: int\n\n\
  \  /* Take one token\n\
  \     when available. */\n\
  \  action take = all {\n\
  \    tokens > 0,\n\
  \    tokens' = tokens - 1,\n\
  \  }\n\n\
  \  // Never negative.\n\
  \  val nonNegative = tokens >= 0\n\n\
  \  pure def clamp(x: int): int = if (x > CAPACITY) CAPACITY else x\n\n\
  \  temporal alwaysOk = always(nonNegative)\n\n\
  \  type Level = int\n\n\
  \  def helper(x) = {\n\
  \    val inner = x\n\
  \    inner\n\
  \  }\n\
   }\n"

let rocq_theory =
  "(** Widgets, formally. *)\n\
   Require Import List.\n\n\
   (** A widget has a width. *)\n\
   Record widget := { width : nat }.\n\n\
   (** Parse a widget description. *)\n\
   Definition parse_widget (s : list nat) : option widget :=\n\
  \  None.\n\n\
   (* An ordinary comment: not a doc. *)\n\
   Fixpoint area (n : nat) : nat :=\n\
  \  match n with 0 => 0 | S m => area m end.\n\n\
   Local Definition hidden := 0.\n\n\
   Section Props.\n\
   Variable w : widget.\n\
   (** Parsing never fails on empty input. *)\n\
   Lemma parse_nil : parse_widget nil = None.\n\
   Proof. reflexivity. Qed.\n\
   End Props.\n\n\
   Module Sub.\n\
   (** Inside a module. *)\n\
   #[global] Instance inst : Inhabited nat := { default := 0 }.\n\
   End Sub.\n\n\
   Module Alias := Sub.\n\
   Theorem after : True.\n\
   Proof. exact I. Qed.\n"

let extras () =
  Alcotest.check strs "C header items (guards, includes, members and macro calls skipped)"
    [
      "macro WIDGET_MAX";
      "function widget_parse";
      "typedef widget_t";
      "typedef widget_draw_fn";
      "enum widget_kind";
      "struct canvas";
      "var widget_count";
      "function widget_area";
    ]
    (titles ~path:"include/widget.h" c_header);
  Alcotest.(check string)
    "/** */ doc" "Parse a widget description.\nReturns NULL on error."
    (doc_of ~path:"include/widget.h" c_header "widget_parse");
  Alcotest.(check string)
    "/// doc" "A widget."
    (doc_of ~path:"include/widget.h" c_header "widget_t");
  Alcotest.(check string)
    "macro doc" "Maximum widgets per canvas."
    (doc_of ~path:"include/widget.h" c_header "WIDGET_MAX");
  Alcotest.(check string)
    "a plain comment is not a doc" ""
    (doc_of ~path:"include/widget.h" c_header "widget_area");
  (match
     List.find_opt
       (fun (i : Api_item.item) -> i.name = "widget_parse")
       (Extract.items ~path:"include/widget.h" c_header)
   with
  | Some i ->
      Alcotest.(check string)
        "multi-line prototype signature"
        "widget_t *widget_parse(const char *s, size_t len)" i.signature;
      Alcotest.(check int) "line" 17 i.line
  | None -> Alcotest.fail "widget_parse missing");
  Alcotest.check strs "C++ namespaces qualify items"
    [ "class acme::gauge::Gauge"; "function acme::gauge::render" ]
    (titles ~path:"include/acme/gauge.hpp" cpp_header);
  Alcotest.check strs "C selection: include/** and root headers only"
    [ "include/acme/gauge.hpp"; "include/widget.h"; "widget.h" ]
    (Extract_c.extractor.select
       (ctx_of
          [
            "include/widget.h";
            "include/acme/gauge.hpp";
            "widget.h";
            "src/internal.h";
            "src/widget.c";
            "vendor/include/dep.h";
            "test/include/t.h";
          ]));
  Alcotest.check strs "Quint items"
    [
      "module bucket";
      "const bucket.CAPACITY";
      "var bucket.tokens";
      "action bucket.take";
      "val bucket.nonNegative";
      "def bucket.clamp";
      "temporal bucket.alwaysOk";
      "type bucket.Level";
      "def bucket.helper";
    ]
    (titles ~path:"spec/bucket.qnt" quint_spec);
  Alcotest.(check string)
    "Quint block comment" "Take one token\nwhen available."
    (doc_of ~path:"spec/bucket.qnt" quint_spec "take");
  Alcotest.(check string)
    "Quint line comment" "Never negative."
    (doc_of ~path:"spec/bucket.qnt" quint_spec "nonNegative");
  Alcotest.(check string)
    "a trailing comment is not the next item's doc" ""
    (doc_of ~path:"t.qnt" "module m {\n  const A: int // about A\n  var b: int\n}\n" "b");
  Alcotest.(check string)
    "Quint module doc" "A token bucket."
    (doc_of ~path:"spec/bucket.qnt" quint_spec "bucket");
  Alcotest.check strs "Rocq items (Local skipped, Section transparent, modules nest)"
    [
      "record Widget.widget";
      "definition Widget.parse_widget";
      "fixpoint Widget.area";
      "lemma Widget.parse_nil";
      "module Widget.Sub";
      "instance Widget.Sub.inst";
      "module Widget.Alias";
      "theorem Widget.after";
    ]
    (titles ~path:"theories/Widget.v" rocq_theory);
  Alcotest.(check string)
    "Rocq doc" "Parse a widget description."
    (doc_of ~path:"theories/Widget.v" rocq_theory "parse_widget");
  Alcotest.(check string)
    "Rocq plain comment is not a doc" ""
    (doc_of ~path:"theories/Widget.v" rocq_theory "area");
  (match
     List.find_opt
       (fun (i : Api_item.item) -> i.name = "parse_widget")
       (Extract.items ~path:"theories/Widget.v" rocq_theory)
   with
  | Some i ->
      Alcotest.(check string)
        "Rocq signature up to :=" "Definition parse_widget (s : list nat) : option widget"
        i.signature
  | None -> Alcotest.fail "parse_widget missing");
  let names ctx =
    List.map (fun (e : Api_item.extractor) -> e.language) (Extract.detected ctx)
  in
  Alcotest.check strs "detection of the extras" [ "c"; "quint"; "rocq" ]
    (names (ctx_of [ "include/w.h"; "spec/b.qnt"; "_CoqProject"; "theories/W.v" ]));
  Alcotest.check strs "a .v file alone is not Rocq (Verilog)" []
    (names (ctx_of [ "rtl/top.v" ]));
  Alcotest.check strs "Rocq through dune-project" [ "ocaml"; "rocq" ]
    (names
       (ctx_of
          ~manifests:[ ("dune-project", "(lang dune 3.8)\n(using coq 0.8)\n") ]
          [ "dune-project"; "theories/W.v" ]))

(* ------------------------------------------------------------ through the store *)

let checkout dev name files =
  let dir = Filename.concat dev name in
  F.git_checkout dir files;
  dir

let cross_language () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let readme = ("README.md", "# W\n\nWidgets.\n") in
  let repos =
    [
      ( "widget-ml",
        [ readme; ("dune-project", "(lang dune 3.0)\n"); ("src/widget.ml", ocaml_ml) ] );
      ( "widget-rs",
        [
          readme;
          ("Cargo.toml", "[package]\nname = \"widget-rs\"\n");
          ("src/lib.rs", rust_lib);
        ] );
      ( "widget-go",
        [
          readme;
          ("go.mod", "module example.com/acme/widget-go/v2\n");
          ("doc.go", go_doc);
          ("widget.go", go_src);
        ] );
      ( "widget-py",
        [
          readme;
          ("pyproject.toml", "[project]\nname = \"widget-py\"\n");
          ("src/widget/__init__.py", py_init);
        ] );
      ( "widget-ts",
        [
          readme;
          ("package.json", {|{"name":"widget-ts","main":"src/index.ts"}|});
          ("src/index.ts", ts_index);
        ] );
      ("widget-cbl", [ readme; ("main.cbl", "IDENTIFICATION DIVISION.\n") ]);
    ]
  in
  let keys =
    List.map
      (fun (name, files) ->
        F.insert_repo ctx ~owner:"example-user" ~local_dir:(checkout dev name files) name)
      repos
  in
  let s = F.ingest ctx (F.mixed_env (Fake_forge.create ())) in
  Alcotest.(check (list string)) "no gap" [] s.gaps;
  let api_paths key =
    List.filter_map (fun (p, k, _) -> if k = "api" then Some p else None) (F.docs ctx key)
  in
  Alcotest.(check (list (list string)))
    "api documents per repository (unknown language: none)"
    [
      [ "src/widget.ml" ];
      [ "src/lib.rs" ];
      [ "doc.go"; "widget.go" ];
      [ "src/widget/__init__.py" ];
      [ "src/index.ts" ];
      [];
    ]
    (List.map api_paths keys);
  (* A capability phrase finds items in several languages through the chunk index. *)
  let hits =
    Db.query ctx.db
      {|SELECT DISTINCT r.name FROM doc_chunks_fts f JOIN doc_chunks c ON c.id = f.rowid
JOIN documents d ON d.id = c.doc_id JOIN repos r ON r.id = d.repo_id
WHERE doc_chunks_fts MATCH ? AND d.kind = 'api' ORDER BY r.name|}
      [ Db.text "\"widget\" \"description\"" ]
      (fun r -> Option.value (Db.to_string_opt r.(0)) ~default:"")
  in
  Alcotest.check strs "items from five languages"
    [ "widget-go"; "widget-ml"; "widget-py"; "widget-rs"; "widget-ts" ]
    hits;
  let d =
    ok_or_fail
      (Ops.query_docs ctx
         (`Assoc
            [
              ("libraryId", `String (List.nth keys 1));
              ("query", `String "parse widget");
              ("kind", `String "api");
            ]))
  in
  Alcotest.(check bool)
    "query-docs kind=api" true
    (contains (Yojson.Safe.to_string d) "fn crate::parse_widget")

(* Items of the three extra languages through the store: api documents, scanned
   api_items and find-capability. *)
let extras_store () =
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let readme = ("README.md", "# W\n\nWidgets.\n") in
  let mk name files =
    F.insert_repo ctx ~owner:"example-user" ~local_dir:(checkout dev name files) name
  in
  let kc = mk "widget-c" [ readme; ("include/widget.h", c_header) ] in
  let kq = mk "widget-qnt" [ readme; ("spec/bucket.qnt", quint_spec) ] in
  let kr =
    mk "widget-v"
      [ readme; ("_CoqProject", "-R theories W\n"); ("theories/Widget.v", rocq_theory) ]
  in
  let s = F.ingest ctx (F.mixed_env (Fake_forge.create ())) in
  Alcotest.(check (list string)) "no gap" [] s.gaps;
  let api_paths key =
    List.filter_map (fun (p, k, _) -> if k = "api" then Some p else None) (F.docs ctx key)
  in
  Alcotest.(check (list (list string)))
    "api documents"
    [ [ "include/widget.h" ]; [ "spec/bucket.qnt" ]; [ "theories/Widget.v" ] ]
    (List.map api_paths [ kc; kq; kr ]);
  let r =
    ok_or_fail (Ops.find_capability ctx (`Assoc [ ("query", `String "parse widget") ]))
  in
  let libs =
    List.sort_uniq compare
      (List.map
         (fun h -> to_str (member "id" (member "library" h)))
         (match member "results" r with `List l -> l | _ -> []))
  in
  Alcotest.(check bool)
    "find-capability reaches C and Rocq" true
    (List.mem kc libs && List.mem kr libs);
  let r = ok_or_fail (Ops.find_capability ctx (`Assoc [ ("query", `String "token") ])) in
  Alcotest.(check bool)
    "find-capability reaches Quint" true
    (contains (Yojson.Safe.to_string r) "bucket.take")

(* ------------------------------------------------------------ aliases *)

let aliases () =
  Alcotest.check strs "every manifest kind"
    [
      "@acme/gauges";
      "gauge-dune";
      "gauge-go";
      "gauge-kit";
      "gauge-py";
      "gauge-rs";
      "gauges";
    ]
    (Manifest.aliases
       ~paths:[ "gauge-kit.opam"; "sub/other.opam"; "dune-project" ]
       ~manifests:
         [
           ("dune-project", "(lang dune 3.0)\n(name gauge-dune)\n");
           ("package.json", {|{"name":"@acme/gauges"}|});
           ( "Cargo.toml",
             "[workspace]\n\
              [package]\n\
              version = \"1\"\n\
              name = \"gauge-rs\"\n\
              [dependencies]\n\
              name = \"x\"\n" );
           ( "pyproject.toml",
             "[build-system]\nrequires = []\n[project]\nname = \"gauge-py\"\n" );
           ("go.mod", "module example.com/acme/gauge-go/v3\n\ngo 1.22\n");
         ]);
  (* End to end: a repository named differently from its package is resolved by the
     package name, and found by search. *)
  let ctx = empty_ctx () in
  let dev = temp_dir () in
  let dir =
    checkout dev "ocaml-gauge"
      [
        ("README.md", "# ocaml-gauge\n\nGauges.\n");
        ("gauge-kit.opam", "opam-version: \"2.0\"\n");
        ("src/gauge.mli", "val draw : float -> string\n");
      ]
  in
  let key = F.insert_repo ctx ~owner:"example-user" ~local_dir:dir "ocaml-gauge" in
  let before =
    ok_or_fail
      (Ops.resolve_library_id ctx (`Assoc [ ("libraryName", `String "gauge-kit") ]))
  in
  Alcotest.(check bool)
    "not found before ingestion" true
    (member "candidates" before = `List []);
  ignore (F.ingest ctx (F.mixed_env (Fake_forge.create ())));
  let r =
    ok_or_fail
      (Ops.resolve_library_id ctx (`Assoc [ ("libraryName", `String "gauge-kit") ]))
  in
  (match member "candidates" r with
  | `List (c :: _) ->
      Alcotest.(check string) "resolved by its package name" key (to_str (member "id" c))
  | _ -> Alcotest.fail "no candidate");
  let s = ok_or_fail (Ops.search ctx (`Assoc [ ("query", `String "gauge-kit") ])) in
  Alcotest.(check bool)
    "searchable alias" true
    (contains (Yojson.Safe.to_string (member "hits" s)) key)

(* An API manifest that cannot be read keeps the stored aliases (and is a gap). *)
let aliases_kept_on_failure () =
  let ctx = empty_ctx () in
  let key = F.insert_repo ctx ~owner:"example-user" "gauge-js" in
  Db.run ctx.db "UPDATE repos SET aliases = '[\"old-gauge\"]' WHERE key = ?"
    [ Db.text key ];
  let fn = "example-user/gauge-js" in
  let forge = Fake_forge.create () in
  Fake_forge.json forge (F.tree_path fn)
    (F.gh_tree [ ("README.md", "r"); ("package.json", "p") ]);
  Fake_forge.ok forge (F.file_path fn "README.md") "# gauge\n\nx\n";
  Fake_forge.fail forge (F.file_path fn "package.json") `Network;
  let s = F.ingest ~full:true ctx (Fake_forge.env forge) in
  Alcotest.(check bool) "gap recorded" true (s.gaps <> []);
  Alcotest.(check string)
    "stored aliases kept" {|["old-gauge"]|}
    (Option.value ~default:""
       (Option.join
          (Db.query_one ctx.db "SELECT aliases FROM repos WHERE key = ?"
             [ Db.text key ]
             (fun r -> Db.to_string_opt r.(0)))));
  Fake_forge.ok forge (F.file_path fn "package.json") {|{"name":"gauge-js"}|};
  ignore (F.ingest ~full:true ctx (Fake_forge.env forge));
  Alcotest.(check string)
    "replaced once readable" {|["gauge-js"]|}
    (Option.value ~default:""
       (Option.join
          (Db.query_one ctx.db "SELECT aliases FROM repos WHERE key = ?"
             [ Db.text key ]
             (fun r -> Db.to_string_opt r.(0)))))

let () =
  run "extract"
    [
      ( "extractors",
        [
          Alcotest.test_case "OCaml" `Quick ocaml;
          Alcotest.test_case "TypeScript" `Quick typescript;
          Alcotest.test_case "Rust" `Quick rust;
          Alcotest.test_case "Go" `Quick go;
          Alcotest.test_case "Python" `Quick python;
          Alcotest.test_case "detection" `Quick detection;
          Alcotest.test_case "cross-language" `Quick cross_language;
        ] );
      ( "extras",
        [
          Alcotest.test_case "C, Quint and Rocq" `Quick extras;
          Alcotest.test_case "through the store" `Quick extras_store;
        ] );
      ( "aliases",
        [
          Alcotest.test_case "package names" `Quick aliases;
          Alcotest.test_case "kept on a failed manifest" `Quick aliases_kept_on_failure;
        ] );
    ]
