open Api_item

let header_exts = [ ".h"; ".hh"; ".hpp"; ".hxx"; ".h++" ]

let is_header p =
  List.exists (Filename.check_suffix (String.lowercase_ascii p)) header_exts

let public_header p =
  is_header p
  && (not (skipped p))
  &&
  let segs = String.split_on_char '/' p in
  List.length segs = 1
  || List.mem "include" (List.filteri (fun i _ -> i < List.length segs - 1) segs)

let select (ctx : context) = List.filter public_header ctx.paths |> List.sort_uniq compare

let detect (ctx : context) =
  language_is ctx [ "C"; "C++"; "Objective-C"; "Cuda" ]
  || List.exists public_header ctx.paths

let re s = Re.compile (Re.Perl.re s)
let ident_re = re {|[A-Za-z_][A-Za-z0-9_]*(?:::[A-Za-z_~][A-Za-z0-9_]*)*|}
let define_re = re {|^#[ \t]*define[ \t]+([A-Za-z_][A-Za-z0-9_]*)|}
let ifndef_re = re {|^#[ \t]*ifndef[ \t]+([A-Za-z_][A-Za-z0-9_]*)|}
let template_re = re {|^template[ \t]*<[^{;]*?>[ \t]*|}
let fnptr_re = re {|\([ \t]*\*[ \t]*([A-Za-z_][A-Za-z0-9_]*)[ \t]*\)|}
let tag_re = re {|^(struct|union|enum[ \t]+class|enum[ \t]+struct|enum|class)\b|}
let namespace_re = re {|^(?:inline[ \t]+)?namespace(?:[ \t]+([A-Za-z_][A-Za-z0-9_:]*))?$|}
let extern_c_re = re {|^extern[ \t]*"[^"]*"$|}

let not_names =
  [
    "__attribute__";
    "__attribute";
    "__declspec";
    "alignas";
    "_Alignas";
    "decltype";
    "sizeof";
    "noexcept";
    "throw";
    "operator";
    "if";
    "while";
    "for";
    "switch";
    "return";
  ]

let guard name =
  List.exists
    (fun s -> String.ends_with ~suffix:s name)
    [ "_H"; "_H_"; "_H__"; "_HPP"; "_HPP_"; "_HH"; "_INCLUDED"; "_INCLUDED_" ]

let idents s = List.map (fun g -> Re.Group.get g 0) (Re.all ident_re s)
let last l = match List.rev l with x :: _ -> Some x | [] -> None
let sig_of s = Utf8.take (Src_scan.squash s) 300

(* The function name: the identifier just before the first '(' that is not an attribute,
   with a non-blank prefix (a return type), so that a bare macro call is not a prototype. *)
let function_name s =
  let rec go pos =
    match Re.exec_opt ~pos (re {|([A-Za-z_~][A-Za-z0-9_:~]*)[ \t]*\(|}) s with
    | None -> None
    | Some g ->
        let name = Re.Group.get g 1 in
        let start = Re.Group.start g 1 in
        if List.mem name not_names then go (Re.Group.stop g 0)
        else if String.trim (String.sub s 0 start) = "" then None
        else Some name
  in
  go 0

type kind =
  | Item of string * string
  (* kind, name *)
  | Skip

(* [s] up to its first single ':' (a base class or an enum's underlying type); a '::'
   scope separator is kept. *)
let cut_base s =
  let n = String.length s in
  let rec go i =
    if i >= n then s
    else if s.[i] = ':' && i + 1 < n && s.[i + 1] = ':' then go (i + 2)
    else if s.[i] = ':' then String.sub s 0 i
    else go (i + 1)
  in
  go 0

let classify ~has_body text =
  let t = String.trim (Re.replace_string template_re ~by:"" (String.trim text)) in
  let head =
    (* The part before a body, for tag names and the function test. *)
    match String.index_opt t '{' with
    | Some i -> String.sub t 0 i
    | None -> t
  in
  let starts p = String.starts_with ~prefix:p t in
  if starts "typedef" then
    match Re.exec_opt fnptr_re t with
    | Some g -> Item ("typedef", Re.Group.get g 1)
    | None -> (
        match last (idents t) with
        | Some n when n <> "typedef" -> Item ("typedef", n)
        | _ -> Skip)
  else if String.contains head '(' && not (String.contains head '=') then
    match function_name head with Some n -> Item ("function", n) | None -> Skip
  else
    match Re.exec_opt tag_re t with
    | Some g -> (
        let kw = Re.Group.get g 1 in
        let kind = if String.starts_with ~prefix:"enum" kw then "enum" else kw in
        let rest =
          String.sub head (Re.Group.stop g 0) (String.length head - Re.Group.stop g 0)
        in
        (* Base classes and enum underlying types follow a ':'. *)
        let rest = cut_base rest in
        match idents rest with
        | [] -> Skip
        | names when has_body -> (
            match last names with Some n -> Item (kind, n) | None -> Skip)
        | [ n ] -> Item (kind, n)
        | _ -> Skip)
    | None when starts "extern" && not (String.contains t '(') -> (
        match last (idents t) with
        | Some n when n <> "extern" -> Item ("var", n)
        | _ -> Skip)
    | None -> Skip

type frame = Transparent of string option | Opaque

(* The walker's state over one header. *)
type st = {
  sc : Src_scan.t;
  mutable items : item list;  (** Reversed. *)
  mutable stack : frame list;
  stmt : Buffer.t;
  mutable stmt_line : int;
  mutable body_head : string option;
  mutable last_ifndef : string option;
}

let opaque st = List.exists (fun f -> f = Opaque) st.stack

let add st kind name line signature =
  let ns =
    List.filter_map
      (function Transparent (Some n) -> Some n | _ -> None)
      (List.rev st.stack)
  in
  st.items <-
    {
      kind;
      name;
      qualified_path = String.concat "::" (ns @ [ name ]);
      signature = sig_of signature;
      doc = Src_scan.doc_above ~accept:Src_scan.is_doc_c st.sc line;
      line = line + 1;
    }
    :: st.items

let reset st =
  Buffer.clear st.stmt;
  st.body_head <- None

let emit st ~has_body =
  let text = Buffer.contents st.stmt in
  (match classify ~has_body text with
  | Item (kind, name) ->
      let signature =
        match (kind, st.body_head) with "function", Some h -> h | _ -> text
      in
      add st kind name st.stmt_line signature
  | Skip -> ());
  reset st

(* A preprocessor line starting at [start], with its continuations: records a macro
   (not an include guard) and returns the last physical line it spans. *)
let preprocessor st start =
  let code = st.sc.code in
  let n = Array.length code in
  let logical = Buffer.create 80 in
  let rec gather j =
    let s = String.trim code.(j) in
    if String.ends_with ~suffix:"\\" s && j + 1 < n then (
      Buffer.add_string logical (String.sub s 0 (String.length s - 1));
      Buffer.add_char logical ' ';
      gather (j + 1))
    else (
      Buffer.add_string logical s;
      j)
  in
  let stop = gather start in
  let line = Buffer.contents logical in
  (match Re.exec_opt ifndef_re line with
  | Some g -> st.last_ifndef <- Some (Re.Group.get g 1)
  | None -> (
      match Re.exec_opt define_re line with
      | Some g ->
          let name = Re.Group.get g 1 in
          if not (guard name || st.last_ifndef = Some name) then (
            st.stmt_line <- start;
            add st "macro" name start line);
          st.last_ifndef <- None
      | None -> st.last_ifndef <- None));
  stop

(* Inside a body: only braces matter; closing the outermost one ends a function
   definition. *)
let opaque_char st c =
  match c with
  | '{' -> st.stack <- Opaque :: st.stack
  | '}' -> (
      (match st.stack with _ :: rest -> st.stack <- rest | [] -> ());
      if not (opaque st) then
        match st.body_head with
        | Some h
          when String.contains h '('
               && (not (String.contains h '='))
               && not (String.starts_with ~prefix:"typedef" (String.trim h)) ->
            emit st ~has_body:true
        | _ -> ())
  | _ -> ()

let open_brace st i =
  let h = String.trim (Buffer.contents st.stmt) in
  if Re.execp extern_c_re h then (
    st.stack <- Transparent None :: st.stack;
    reset st)
  else
    match Re.exec_opt namespace_re h with
    | Some g ->
        st.stack <- Transparent (Re.Group.get_opt g 1) :: st.stack;
        reset st
    | None ->
        if Buffer.length st.stmt = 0 then st.stmt_line <- i;
        st.body_head <- Some h;
        Buffer.add_string st.stmt " { … } ";
        st.stack <- Opaque :: st.stack

let code_char st i c =
  match c with
  | '{' -> open_brace st i
  | '}' ->
      (match st.stack with Transparent _ :: rest -> st.stack <- rest | _ -> ());
      reset st
  | ';' -> if Buffer.length st.stmt > 0 then emit st ~has_body:(st.body_head <> None)
  | c when Buffer.length st.stmt = 0 && (c = ' ' || c = '\t') -> ()
  | c ->
      if Buffer.length st.stmt = 0 then st.stmt_line <- i;
      Buffer.add_char st.stmt c

let extract ~path:_ text =
  let sc = Src_scan.scan Src_scan.C_like text in
  let st =
    {
      sc;
      items = [];
      stack = [];
      stmt = Buffer.create 256;
      stmt_line = 0;
      body_head = None;
      last_ifndef = None;
    }
  in
  let n = Array.length sc.code in
  let rec walk i =
    if i < n then
      let l = sc.code.(i) in
      let t = String.trim l in
      if (not (opaque st)) && Buffer.length st.stmt = 0 && t <> "" && t.[0] = '#' then
        walk (preprocessor st i + 1)
      else (
        String.iter (fun c -> if opaque st then opaque_char st c else code_char st i c) l;
        if (not (opaque st)) && Buffer.length st.stmt > 0 then Buffer.add_char st.stmt ' ';
        walk (i + 1))
  in
  walk 0;
  List.rev st.items

let extractor = { language = "c"; detect; select; handles = is_header; extract }
