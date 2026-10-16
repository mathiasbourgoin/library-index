type row = {
  id : int;
  key : string;
  host : string;
  owner : string;
  name : string;
  full_name : string;
  archived : bool;
  fork : bool;
  parent : string option;
  recency : string;
  maintained_side : string option;
}

type t = {
  by_key : (string, row) Hashtbl.t;
  by_name : (string, row list) Hashtbl.t;
  by_full_name : (string, row list) Hashtbl.t;
  children : (string, row list) Hashtbl.t;  (** Parent key -> its forks. *)
}

let lower = String.lowercase_ascii
let host_rank = function "github" -> 0 | "gitlab" -> 1 | "local" -> 2 | _ -> 3

let row_of r =
  let s i = Db.to_string_opt r.(i) in
  let str i = Option.value (s i) ~default:"" in
  let non_empty i =
    match s i with Some v when String.trim v <> "" -> Some v | _ -> None
  in
  {
    id = Option.value (Db.to_int_opt r.(0)) ~default:0;
    key = str 1;
    host = str 2;
    owner = str 3;
    name = str 4;
    full_name = str 5;
    archived = Db.to_bool r.(6);
    fork = Db.to_bool r.(7);
    parent = non_empty 8;
    recency = Option.value (List.find_map non_empty [ 9; 10 ]) ~default:"";
    maintained_side = non_empty 11;
  }

let add tbl k r =
  Hashtbl.replace tbl k (r :: Option.value (Hashtbl.find_opt tbl k) ~default:[])

let find idx key = Hashtbl.find_opt idx.by_key (Key.normalize key)
let by_key a b = compare a.key b.key

(* The fork parent: same host, full name equal to [parent]. *)
let parent_of idx r =
  match (r.fork, r.parent) with
  | true, Some p ->
      List.find_opt
        (fun c -> c.host = r.host)
        (Option.value (Hashtbl.find_opt idx.by_full_name (lower p)) ~default:[])
  | _ -> None

let same_name idx r =
  List.filter
    (fun c -> c.host <> r.host)
    (Option.value (Hashtbl.find_opt idx.by_name (lower r.name)) ~default:[])

let forks_of idx r = Option.value (Hashtbl.find_opt idx.children r.key) ~default:[]

let load db =
  let idx =
    {
      by_key = Hashtbl.create 1024;
      by_name = Hashtbl.create 1024;
      by_full_name = Hashtbl.create 1024;
      children = Hashtbl.create 64;
    }
  in
  Db.query db
    {|SELECT id, key, host, owner, name, full_name, archived, fork, parent, last_commit,
  pushed_at, maintained_side FROM repos WHERE host <> 'whitehat' ORDER BY key|}
    [] row_of
  |> List.iter (fun r ->
      Hashtbl.replace idx.by_key r.key r;
      add idx.by_name (lower r.name) r;
      add idx.by_full_name (lower r.full_name) r);
  Hashtbl.iter
    (fun _ r ->
      match parent_of idx r with
      | Some p when p.key <> r.key -> add idx.children p.key r
      | _ -> ())
    idx.by_key;
  idx

let neighbours idx r = same_name idx r @ Option.to_list (parent_of idx r) @ forks_of idx r

(* The connected component of [r] over the copy edges, [r] first, then by key. *)
let component idx r =
  let seen = Hashtbl.create 8 in
  let rec visit acc = function
    | [] -> acc
    | x :: rest when Hashtbl.mem seen x.key -> visit acc rest
    | x :: rest ->
        Hashtbl.replace seen x.key ();
        visit (x :: acc) (neighbours idx x @ rest)
  in
  let others = List.filter (fun c -> c.key <> r.key) (visit [] [ r ]) in
  r :: List.sort by_key others

let group idx key = match find idx key with None -> [] | Some r -> component idx r

let copies idx key =
  match find idx key with
  | None -> []
  | Some r -> ( match component idx r with _ :: rest -> rest | [] -> [])

let self_words = [ "fork"; "his"; "this"; "self"; "mine"; "here" ]

(* The row a curated [maintained_side] of [m] designates, among [members]. *)
let designated idx members m =
  match m.maintained_side with
  | None -> None
  | Some v -> (
      let v = lower (String.trim v) in
      let mem k = List.exists (fun c -> c.key = k) members in
      match v with
      | _ when mem (Key.normalize v) -> Some (Key.normalize v)
      | "github" | "gitlab" | "local" ->
          List.find_map
            (fun c ->
              if c.host = v && lower c.name = lower m.name then Some c.key else None)
            members
      | "upstream" | "parent" -> Option.map (fun p -> p.key) (parent_of idx m)
      | _ when List.mem v self_words -> Some m.key
      | _ -> None)

let most_recent rows =
  let live = List.filter (fun r -> not r.archived) rows in
  let pool = if live = [] then rows else live in
  let better a b =
    match compare b.recency a.recency with
    | 0 -> (
        match compare (host_rank a.host) (host_rank b.host) with
        | 0 -> compare a.key b.key
        | c -> c)
    | c -> c
  in
  match List.sort better pool with r :: _ -> Some r.key | [] -> None

let canonical idx key =
  match find idx key with
  | None -> None
  | Some r -> (
      let members = component idx r in
      match List.find_map (designated idx members) members with
      | Some k -> Some k
      | None -> (
          match List.filter (fun c -> lower c.name = lower r.name) members with
          | _ :: _ :: _ as same -> most_recent same
          | _ -> (
              match parent_of idx r with
              | Some p when not p.archived -> Some p.key
              | _ -> Some r.key)))

let maintained_copy idx key =
  match canonical idx key with Some k when k <> Key.normalize key -> Some k | _ -> None
