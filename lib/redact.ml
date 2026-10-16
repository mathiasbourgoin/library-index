let marker = "[redacted]"

let token_patterns =
  List.map Re.Perl.compile_pat
    [
      "gh[pousr]_[A-Za-z0-9]{36,}";
      "github_pat_[A-Za-z0-9_]{22,}";
      "glpat-[A-Za-z0-9_-]{20,}";
      "glptt-[A-Za-z0-9_-]{20,}";
      "xox[abpr]-[A-Za-z0-9-]{10,}";
      "sk-[A-Za-z0-9]{32,}";
      "sk-(?:ant|proj|svcacct|admin)-[A-Za-z0-9_-]{20,}";
      "gl(?:rt|dt|cbt)-[A-Za-z0-9_-]{20,}";
      "eyJ[A-Za-z0-9_-]{8,}\\.eyJ[A-Za-z0-9_-]{8,}\\.[A-Za-z0-9_-]*";
      "npm_[A-Za-z0-9]{36}";
      "AKIA[0-9A-Z]{16}";
      (* Base58check secret keys of the ed25519, secp256k1, P-256 and BLS families: unencrypted (edsk, spsk, p2sk, BLsk: 54 or 98
         characters) and encrypted (edesk, spesk, p2esk, BLesk: 88). Public keys and
         key hashes (edpk, tz1, ...) are not secrets and do not match. *)
      "(?:edsk|spsk|p2sk|BLsk)[1-9A-HJ-NP-Za-km-z]{50,}";
      "(?:edesk|spesk|p2esk|BLesk)[1-9A-HJ-NP-Za-km-z]{80,}";
    ]

(* PEM ([RSA PRIVATE KEY], [OPENSSH PRIVATE KEY], ...) and PGP armour
   ([PGP PRIVATE KEY BLOCK]) labels. *)
let pem_label =
  Re.(
    seq
      [
        rep (alt [ rg 'A' 'Z'; char ' ' ]);
        str "PRIVATE KEY";
        opt (str " BLOCK");
        str "-----";
      ])

(* A terminated block, matched non-greedily so two blocks stay two matches. *)
let pem_block =
  Re.(
    compile
      (seq
         [
           str "-----BEGIN "; pem_label; non_greedy (rep any); str "-----END "; pem_label;
         ]))

(* An unterminated header: everything from it to the end of the text is withheld. *)
let pem_open = Re.(compile (seq [ str "-----BEGIN "; pem_label; rep any; eos ]))

let userinfo =
  Re.(
    compile
      (seq
         [
           group
             (seq
                [
                  alt [ rg 'a' 'z'; rg 'A' 'Z' ];
                  rep (alt [ rg 'a' 'z'; rg 'A' 'Z'; digit; set "+.-" ]);
                  str "://";
                ]);
           rep1 (compl [ set "/?#@ \t\r\n\"'<>" ]);
           char '@';
         ]))

(* [Authorization: Bearer <token>]: the scheme word is kept, the credential withheld. *)
let bearer =
  Re.(
    compile
      (seq
         [
           group (seq [ no_case (str "bearer"); rep1 (set " \t") ]);
           repn (alt [ alnum; set "._~+/=-" ]) 20 None;
         ]))

let text s =
  let replace re s = Re.replace_string ~all:true re ~by:marker s in
  let s = Utf8.sanitize s in
  let s = replace pem_block s in
  let s = replace pem_open s in
  let s = List.fold_left (fun acc re -> replace re acc) s token_patterns in
  let s = Re.replace ~all:true bearer ~f:(fun g -> Re.Group.get g 1 ^ marker) s in
  Re.replace ~all:true userinfo ~f:(fun g -> Re.Group.get g 1) s

let text_opt = Option.map text
let contains_secret s = not (String.equal (text s) s)

let rec json (v : Yojson.Safe.t) : Yojson.Safe.t =
  match v with
  | `String s -> `String (text s)
  | `List l -> `List (List.map json l)
  | `Assoc m -> `Assoc (List.map (fun (k, x) -> (k, json x)) m)
  | other -> other
