type t = {
  by_key : (string, string list) Hashtbl.t;
  by_path : (string, string list) Hashtbl.t;
  forced_keys : (string, unit) Hashtbl.t;
  forced_paths : (string, unit) Hashtbl.t;
  forced_names : (string, unit) Hashtbl.t;
  confidential_roots : string list;  (** Expanded. *)
  mutable unknown : string list;  (** Checkouts whose remotes are unreadable. *)
}

let create ~confidential_roots =
  {
    by_key = Hashtbl.create 64;
    by_path = Hashtbl.create 64;
    forced_keys = Hashtbl.create 8;
    forced_paths = Hashtbl.create 8;
    forced_names = Hashtbl.create 8;
    confidential_roots;
    unknown = [];
  }

let empty = create ~confidential_roots:[]
let keys (c : Local_scan.checkout) = Option.to_list c.remote_key @ c.all_remote_keys
let lower = String.lowercase_ascii

let add tbl k v =
  Hashtbl.replace tbl k (v @ Option.value (Hashtbl.find_opt tbl k) ~default:[])

let under roots path =
  List.exists
    (fun r -> path = r || String.starts_with ~prefix:(Filename.concat r "") path)
    roots

(* A checkout whose remotes are unknown, or that lies under a confidential local root,
   makes every repository it holds a campaign repository (fail closed). *)
let forced t (c : Local_scan.checkout) =
  c.remotes_unreadable || under t.confidential_roots c.path

let index t (c : Local_scan.checkout) =
  let ks = keys c in
  List.iter (fun k -> add t.by_key k ks) ks;
  Hashtbl.replace t.by_path c.path ks;
  if c.remotes_unreadable then t.unknown <- c.path :: t.unknown;
  if forced t c then (
    List.iter (fun k -> Hashtbl.replace t.forced_keys k ()) ks;
    Hashtbl.replace t.forced_paths c.path ();
    Hashtbl.replace t.forced_names (lower (Filename.basename c.path)) ())

let of_checkouts ?(confidential_roots = []) cs =
  let t = create ~confidential_roots in
  List.iter (index t) cs;
  t

let expand r =
  match Result.to_option (Ctx.home ()) with
  | Some h -> Config.expand_home ~home:h r
  | None -> r

let is_dir d = Sys.file_exists d && Sys.is_directory d

let local_roots (ctx : Ctx.t) =
  List.filter_map
    (fun r ->
      let d = expand r in
      if is_dir d then Some d else None)
    ctx.identity.local_roots
  |> List.sort_uniq compare

let confidential_roots (ctx : Ctx.t) =
  List.map
    (fun r ->
      let d = expand r in
      try Unix.realpath d with Unix.Unix_error _ -> d)
    ctx.identity.confidential_roots
  @ List.map expand ctx.identity.confidential_roots

let configured_checkouts (ctx : Ctx.t) ~call =
  List.concat_map (fun d -> Local_scan.scan ~id:ctx.identity ~call d) (local_roots ctx)

let index ctx cs = of_checkouts ~confidential_roots:(confidential_roots ctx) cs
let configured ctx ~call = index ctx (configured_checkouts ctx ~call)

let paths row =
  match Jsonx.get "local_paths" row with
  | `List l -> List.filter_map (function `String p -> Some p | _ -> None) l
  | _ -> []

let is_checkout d = Sys.file_exists (Filename.concat d ".git")

(* The checkouts at the rows' local paths that the index does not hold, read now. *)
let unindexed t ~id ~call rows =
  List.filter_map
    (fun p ->
      if Hashtbl.mem t.by_path p || not (is_checkout p) then None
      else Some (Local_scan.checkout ~id ~call p))
    (List.sort_uniq compare (List.concat_map paths rows))

let row_key rows =
  List.find_map (fun r -> Option.map Key.normalize (Jsonx.str "key" r)) rows

let for_row t ~id ~call rows =
  let indexed =
    match row_key rows with
    | Some k -> Option.value (Hashtbl.find_opt t.by_key k) ~default:[]
    | None -> []
  in
  let at_paths =
    List.concat_map
      (fun p -> Option.value (Hashtbl.find_opt t.by_path p) ~default:[])
      (List.concat_map paths rows)
    @ List.concat_map keys (unindexed t ~id ~call rows)
  in
  List.sort_uniq compare (indexed @ at_paths)

let forced_row t ~id ~call rows =
  (match row_key rows with Some k -> Hashtbl.mem t.forced_keys k | None -> false)
  || List.exists (fun p -> Hashtbl.mem t.forced_paths p) (List.concat_map paths rows)
  || List.exists
       (fun r ->
         match Jsonx.str "name" r with
         | Some n -> Hashtbl.mem t.forced_names (lower n)
         | None -> false)
       rows
  || List.exists (forced t) (unindexed t ~id ~call rows)

let check t =
  match List.sort compare t.unknown with
  | [] -> Ok ()
  | paths ->
      Error
        (Printf.sprintf
           "cannot read the remotes of the local checkout(s) %s (git refuses them and \
            their config is unreadable), so their repositories cannot be classified: fix \
            their permissions or move them out of the local root; nothing was fetched"
           (String.concat ", " paths))
