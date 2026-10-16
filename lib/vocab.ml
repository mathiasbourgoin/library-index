let default_domains =
  [
    "OCaml libraries";
    "Formal methods & verification";
    "Agent tooling & AI engineering";
    "Tools & CLIs";
    "Services & apps";
    "Blockchain";
    "Security research";
    "Teaching, papers & personal";
    "Other";
  ]

let tiers = [ "1"; "2"; "fork"; "collab"; "inherited"; "whitehat" ]
let statuses = [ "active"; "dormant"; "stale"; "archived"; "unknown" ]
let hosts = [ "github"; "gitlab"; "local"; "whitehat" ]
let curated_hosts = [ "github"; "gitlab"; "local" ]

let canonical allowed v =
  let lv = String.lowercase_ascii v in
  List.find_opt (fun a -> String.equal (String.lowercase_ascii a) lv) allowed

let invalid_value ~what v allowed =
  Printf.sprintf "invalid %s '%s'; allowed values: %s" what v (String.concat ", " allowed)

let ownerships = [ "own"; "participant"; "external" ]
