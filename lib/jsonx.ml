let member k = function `Assoc m -> List.assoc_opt k m | _ -> None
let get k j = Option.value (member k j) ~default:`Null
let str k j = match member k j with Some (`String s) -> Some s | _ -> None
let bool k j = match member k j with Some (`Bool b) -> b | _ -> false
let int k j = match member k j with Some (`Int n) -> n | _ -> 0

let int_opt k j =
  match member k j with
  | Some (`Int n) -> Some n
  | Some (`Intlit s) -> int_of_string_opt s
  | _ -> None

let number = function `Int n -> Some (Float.of_int n) | `Float f -> Some f | _ -> None
let float_opt k j = Option.bind (member k j) number

let truthy = function
  | Some (`Bool b) -> b
  | Some (`Int n) -> n <> 0
  | Some (`String s) -> s <> ""
  | Some (`List l) -> l <> []
  | _ -> false

let set k v = function
  | `Assoc m when List.mem_assoc k m ->
      `Assoc (List.map (fun (k', v') -> if k' = k then (k, v) else (k', v')) m)
  | `Assoc m -> `Assoc (m @ [ (k, v) ])
  | j -> j

let of_string_opt = function Some s -> `String s | None -> `Null
let strings l = List.filter_map (function `String s -> Some s | _ -> None) l
let parse s = try Some (Yojson.Safe.from_string s) with Yojson.Json_error _ -> None

let stored_strings col =
  match Option.bind col parse with Some (`List l) -> strings l | _ -> []

let stored_assoc col = match Option.bind col parse with Some (`Assoc m) -> m | _ -> []
