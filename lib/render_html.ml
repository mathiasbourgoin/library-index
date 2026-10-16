(* Port of refresh.py@b70f23d render_html (:665-756). *)

open Render_data

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#x27;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let e = escape
let opt s = Option.value s ~default:""
let lang (r : repo) = Option.value (first_set [ r.language ]) ~default:"—"

let table cols rows tid =
  let head =
    Printf.sprintf {|<table id="%s"><thead><tr>|} tid
    ^ String.concat "" (List.map (fun c -> "<th>" ^ e c ^ "</th>") cols)
    ^ "</tr></thead><tbody>"
  in
  let row cells =
    "<tr>" ^ String.concat "" (List.map (fun c -> "<td>" ^ c ^ "</td>") cells) ^ "</tr>"
  in
  String.concat "\n" ((head :: List.map row rows) @ [ "</tbody></table>" ])

let link (r : repo) =
  Printf.sprintf {|<a href="%s">%s</a>|}
    (e (Option.value r.url ~default:"None"))
    (e r.full_name)
  ^ (if r.private_ then " 🔒" else "")
  ^ if r.archived then " <em>(archived)</em>" else ""

let loc ~home (r : repo) =
  match r.local_paths with p :: _ -> "<code>" ^ e (tilde ~home p) ^ "</code>" | [] -> ""

let section1 ~home data =
  let part =
    List.filter
      (fun r ->
        r.tier = "1" && r.role <> None && r.ownership <> "external" && not (is_campaign r))
      data.repos
    |> stable_sort_by (fun r -> (role_rank r.role, -date_number (last_date r))) compare
  in
  let sec = List.filter (fun r -> r.tier = "1" && is_campaign r) data.repos in
  let inh = List.filter (fun r -> r.tier = "inherited") data.repos in
  let names l =
    e (String.concat ", " (List.sort compare (List.map (fun r -> r.full_name) l)))
  in
  [
    "<h2>1. Repos I participate in</h2>";
    table
      [ "Repo"; "Role"; "Evidence"; "Lang"; "Last commit"; "Status"; "Local" ]
      (List.map
         (fun r ->
           [
             link r;
             e (opt r.role);
             e r.evidence;
             e (lang r);
             d10 (last_date r);
             e r.status;
             loc ~home r;
           ])
         part)
      "t-part";
  ]
  @ (if sec = [] then []
     else
       [
         Printf.sprintf
           "<p><strong>Security-research workspaces (%d, private audit \
            mirrors):</strong> %s</p>"
           (List.length sec) (names sec);
       ])
  @
  if inh = [] then []
  else
    [
      Printf.sprintf
        "<p><strong>Inherited history (%d forks/mirrors carrying his upstream \
         commits):</strong> %s</p>"
        (List.length inh) (names inh);
    ]

let domain_index ~domains r =
  let d = Option.value (first_set [ r.domain ]) ~default:"Other" in
  let rec find i = function
    | [] -> 99
    | x :: tl -> if x = d then i else find (i + 1) tl
  in
  find 0 domains

let section2 ~home data =
  let t1 =
    List.filter
      (fun r -> r.tier = "1" && r.ownership <> "external" && not (is_campaign r))
      data.repos
    |> stable_sort_by
         (fun r ->
           ( domain_index ~domains:data.settings.identity.domains r,
             String.lowercase_ascii r.full_name ))
         compare
  in
  [
    "<h2>2. Tier 1 — authored / maintained</h2>";
    table
      [
        "Repo";
        "Domain";
        "What it is";
        "Lang";
        "Last commit";
        "Status";
        "Interest / reuse";
        "Local";
      ]
      (List.map
         (fun r ->
           [
             link r;
             e (Option.value (first_set [ r.domain ]) ~default:"Other");
             e (what r);
             e (lang r);
             d10 (last_date r);
             e r.status;
             e (opt r.interest);
             loc ~home r;
           ])
         t1)
      "t-tier1";
  ]

let section3_4 ~home data =
  let forks =
    List.filter (fun r -> r.tier = "fork") data.repos
    |> stable_sort_by (fun r -> String.lowercase_ascii r.full_name) compare
  in
  [
    "<h2>3. Forks</h2>";
    table
      [ "Fork"; "Parent / maintained side"; "Lang"; "Fork last push" ]
      (List.map
         (fun r -> [ link r; e (opt r.fork_note); e (lang r); d10 r.pushed_at ])
         forks)
      "t-forks";
    "<h2>4. Local-only / other local checkouts</h2>";
    table
      [ "Path"; "Remote"; "Last commit"; "His commits / total" ]
      (List.map
         (fun (l : local) ->
           [
             "<code>" ^ e (tilde ~home l.path) ^ "</code>";
             e (Option.value (first_set [ l.remote_key ]) ~default:"local only");
             d10 l.last_commit;
             Printf.sprintf "%d / %d" l.my_commits l.total_commits;
           ])
         data.section4)
      "t-local";
  ]

let section5 data =
  let col =
    List.filter (fun r -> r.tier = "collab") data.repos
    |> stable_sort_by (fun r -> (r.fork, String.lowercase_ascii r.full_name)) compare
  in
  [
    Printf.sprintf "<h2>5. %s — tools and libraries</h2>"
      (e
         (match data.settings.identity.gh_collabs with
         | [] -> "Collaborators"
         | l -> String.concat ", " l));
    table
      [
        "Repo";
        "What it is";
        "Lang";
        "Last commit";
        "Fork";
        "Interest / reuse";
        data.settings.contributes_column;
      ]
      (List.map
         (fun r ->
           [
             link r;
             e (what r);
             e (lang r);
             d10 (last_date r);
             e (Option.value (first_set [ r.fork_note ]) ~default:"no");
             e (opt r.interest);
             e (if r.role <> None then r.evidence else "");
           ])
         col)
      "t-collab";
  ]

let section6_7 data =
  let t2 =
    List.filter in_section7 data.repos
    |> stable_sort_by
         (fun r -> (String.lowercase_ascii r.owner, String.lowercase_ascii r.name))
         compare
  in
  [
    "<h2>6. whitehat — internal tools (by campaign)</h2><p class=meta>Generic \
     descriptions only; finding-specific tools withheld.</p>";
    table
      [ "Campaign repo"; "Tool path"; "What it does"; "Lang"; "Last commit"; "Reuse" ]
      (List.map
         (fun t ->
           [
             e t.campaign;
             "<code>" ^ e t.tool_path ^ "</code>";
             e t.tool_what;
             e t.tool_language;
             e t.tool_last_commit;
             e t.reuse;
           ])
         data.tools)
      "t-whitehat";
    "<h2>7. Appendix — Tier 2 org repos</h2>";
    table
      [ "Org"; "Repo"; "Lang"; "Last push"; "Description" ]
      (List.map
         (fun r ->
           [
             e r.owner;
             (link r ^ if r.fork then " (fork)" else "");
             e (lang r);
             d10 r.pushed_at;
             e (Utf8.take r.description 160);
           ])
         t2)
      "t-tier2";
  ]

(* Section 8: third-party libraries registered for use (never counted as his). *)
let section8 data =
  match external_registered data with
  | [] -> []
  | rows ->
      [
        "<h2>8. External libraries registered for use</h2>";
        table
          [ "Key"; "What it is"; "Lang"; "Last commit" ]
          (List.map
             (fun r ->
               [
                 "<code>" ^ e r.key ^ "</code>"; e (what r); e (lang r); d10 (last_date r);
               ])
             rows)
          "t-external";
      ]

let style =
  {|:root{--bg:#fbfbf8;--fg:#1d1d1b;--mut:#6b6b66;--line:#deded8;--acc:#2b5fb3;--hd:#f0f0ea}
@media (prefers-color-scheme:dark){:root{--bg:#151614;--fg:#e6e6e0;--mut:#9a9a92;--line:#33342f;--acc:#7fa8ec;--hd:#1f201d}}
body{background:var(--bg);color:var(--fg);font:14px/1.45 system-ui,sans-serif;margin:0 auto;max-width:1400px;padding:16px}
a{color:var(--acc)} h1{font-size:1.5em} h2{margin-top:2em;border-bottom:1px solid var(--line)}
.meta{color:var(--mut)} code{font-size:.9em}
table{border-collapse:collapse;width:100%;margin:.5em 0} th,td{border-bottom:1px solid var(--line);padding:4px 6px;text-align:left;vertical-align:top}
th{background:var(--hd);cursor:pointer;position:sticky;top:0;white-space:nowrap}
#q{width:100%;max-width:420px;padding:6px;background:var(--bg);color:var(--fg);border:1px solid var(--line);border-radius:4px}
.wrap{overflow-x:auto}
|}

let script =
  {|const q=document.getElementById('q');
q.addEventListener('input',()=>{const v=q.value.toLowerCase();document.querySelectorAll('tbody tr').forEach(tr=>{tr.style.display=tr.textContent.toLowerCase().includes(v)?'':'none'})});
document.querySelectorAll('th').forEach((th,i)=>th.addEventListener('click',()=>{const tb=th.closest('table').tBodies[0];const idx=[...th.parentNode.children].indexOf(th);const asc=th.dataset.asc!=='1';th.dataset.asc=asc?'1':'0';
[...tb.rows].sort((a,b)=>{const x=a.cells[idx].textContent,y=b.cells[idx].textContent;return asc?x.localeCompare(y,undefined,{numeric:true}):y.localeCompare(x,undefined,{numeric:true})}).forEach(r=>tb.appendChild(r))}));
|}

let render ~home data =
  let sections =
    section1 ~home data @ section2 ~home data @ section3_4 ~home data @ section5 data
    @ section6_7 data @ section8 data
  in
  let gaps = String.concat "" (List.map (fun g -> "<li>" ^ e g ^ "</li>") data.gaps) in
  let cnt = String.concat " · " (List.map (fun (k, v) -> e k ^ ": " ^ v) data.counts) in
  String.concat ""
    [
      {|<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>Library index</title>
<style>
|};
      style;
      {|</style></head><body>
<h1>|};
      e data.settings.title;
      {|</h1>
<p class="meta">Generated |};
      data.generated;
      {| by <code>library-index render</code> from <code>|};
      e data.settings.db_display;
      {|</code> (query it with <code>library-index search</code> / <code>get</code> / <code>list</code> or the <code>library-index</code> MCP server; text view: <code>|};
      e data.settings.index_display;
      {|</code>). |};
      cnt;
      {|</p>
<details><summary>Gaps</summary><ul>|};
      gaps;
      {|</ul></details>
<p><input id="q" placeholder="Filter all tables (e.g. ocaml, mcp, quint)…" autofocus> <span class="meta">Click a header to sort.</span></p>
<div class="wrap">|};
      String.concat "" sections;
      {|</div>
<script>
|};
      script;
      {|</script></body></html>
|};
    ]
