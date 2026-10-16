type obj = (string * Yojson.Safe.t) list

let members = function
  | `Null -> Ok []
  | `Assoc m -> Ok m
  | _ -> Error "arguments must be a JSON object"

let object_ ~allowed v =
  Result.bind (members v) (fun m ->
      match List.find_opt (fun (k, _) -> not (List.mem k allowed)) m with
      | Some (k, _) ->
          Error
            (Printf.sprintf "unknown argument '%s'; allowed: %s" k
               (String.concat ", " allowed))
      | None -> Ok m)

let find o k =
  match List.assoc_opt k o with None | Some `Null -> None | Some v -> Some v

let req_string o k =
  match find o k with
  | None -> Error (Printf.sprintf "missing required argument '%s'" k)
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "argument '%s' must be a string" k)

let opt_string o k =
  match find o k with
  | None -> Ok None
  | Some (`String s) -> Ok (Some s)
  | Some _ -> Error (Printf.sprintf "argument '%s' must be a string" k)

let opt_int o k =
  match find o k with
  | None -> Ok None
  | Some (`Int n) -> Ok (Some n)
  | Some (`Float f) when Float.is_integer f && Float.abs f < 1e15 ->
      Ok (Some (int_of_float f))
  | Some _ -> Error (Printf.sprintf "argument '%s' must be an integer" k)

let opt_bool o k =
  match find o k with
  | None -> Ok None
  | Some (`Bool b) -> Ok (Some b)
  | Some _ -> Error (Printf.sprintf "argument '%s' must be a boolean" k)

let opt_object o k =
  match find o k with
  | None -> Ok None
  | Some (`Assoc m) -> Ok (Some m)
  | Some _ -> Error (Printf.sprintf "argument '%s' must be an object" k)

let clamp ~lo ~hi n = max lo (min hi n)
