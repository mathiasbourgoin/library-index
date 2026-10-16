let get = Jsonx.get
let str = Jsonx.str
let key row = str "key" row
let or_null = Jsonx.of_string_opt
let truthy = function `Null | `Bool false -> false | _ -> true
let bool_or_false = function `Bool b -> `Bool b | _ -> `Bool false
let desc j = match get "description" j with `String s -> `String s | _ -> `String ""

let gh r =
  match (str "full_name" r, str "login" (get "owner" r), str "name" r) with
  | Some full_name, Some owner, Some name ->
      Some
        (`Assoc
           [
             ("key", `String ("github:" ^ String.lowercase_ascii full_name));
             ("host", `String "github");
             ("full_name", `String full_name);
             ("owner", `String owner);
             ("name", `String name);
             ("url", get "html_url" r);
             ("private", get "private" r);
             ("fork", get "fork" r);
             ("archived", bool_or_false (get "archived" r));
             ("language", get "language" r);
             ("description", desc r);
             ("default_branch", get "default_branch" r);
             ("pushed_at", get "pushed_at" r);
             ("updated_at", get "updated_at" r);
             ("parent", or_null (str "full_name" (get "parent" r)));
           ])
  | _ -> None

let gl p =
  match
    ( str "path_with_namespace" p,
      str "full_path" (get "namespace" p),
      str "path" p,
      get "id" p )
  with
  | Some full_name, Some owner, Some name, (`Int _ as id) ->
      let parent = get "forked_from_project" p in
      Some
        (`Assoc
           [
             ("key", `String ("gitlab:" ^ String.lowercase_ascii full_name));
             ("host", `String "gitlab");
             ("full_name", `String full_name);
             ("owner", `String owner);
             ("name", `String name);
             ("url", get "web_url" p);
             ("private", `Bool (str "visibility" p <> Some "public"));
             ("fork", `Bool (truthy parent));
             ("archived", bool_or_false (get "archived" p));
             ("language", `Null);
             ("description", desc p);
             ("default_branch", get "default_branch" p);
             ("pushed_at", get "last_activity_at" p);
             ("updated_at", get "last_activity_at" p);
             ("parent", or_null (str "path_with_namespace" parent));
             ("gl_id", id);
           ])
  | _ -> None

let gl_readme_url p = str "readme_url" p
