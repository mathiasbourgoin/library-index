type outcome = {
  index_md : string;
  notes_html : string option;
  deleted : string list;
  warnings : string list;
}

let notes_file_name now = Render_data.local_date now ^ "-library-index.html"

let page_name =
  Re.compile (Re.Perl.re "^[0-9]{4}-[0-9]{2}-[0-9]{2}-library-index\\.html$")

let title = Re.compile (Re.str "<title>Library index</title>")
let is_stale_page ~name ~content = Re.execp page_name name && Re.execp title content
let atomic_write path contents = Fsio.atomic_write path contents

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

type deletion = Deleted of string | Kept of string | Ignored

(* AM-7: strict name pattern + title, never the page just written. A page that cannot be
   read or removed stays and is reported (EC-22 style warning, exit 2). *)
let delete_one ~remove ~notes_dir ~keep name =
  let path = Filename.concat notes_dir name in
  if name = keep || not (Re.execp page_name name) then Ignored
  else
    match read_file path with
    | exception Sys_error _ -> Ignored
    | content when is_stale_page ~name ~content -> (
        match remove path with () -> Deleted name | exception Sys_error _ -> Kept name)
    | _ -> Ignored

let delete_stale ?(remove = Sys.remove) ~notes_dir ~keep () =
  let names =
    try Sys.readdir notes_dir |> Array.to_list |> List.sort compare
    with Sys_error _ -> []
  in
  List.fold_right
    (fun name (deleted, warnings) ->
      match delete_one ~remove ~notes_dir ~keep name with
      | Deleted n -> (n :: deleted, warnings)
      | Kept n -> (deleted, ("could not delete the stale notes page " ^ n) :: warnings)
      | Ignored -> (deleted, warnings))
    names ([], [])

let write_notes ~notes_dir ~now html =
  if not (Sys.file_exists notes_dir && Sys.is_directory notes_dir) then
    Error "notes directory not found; only INDEX.md was written"
  else
    let name = notes_file_name now in
    match atomic_write (Filename.concat notes_dir name) html with
    | () -> Ok (Filename.concat notes_dir name, delete_stale ~notes_dir ~keep:name ())
    | exception (Sys_error _ | Unix.Unix_error _) ->
        Error "notes directory not writable; only INDEX.md was written"

let run db ~settings ~out ~notes_dir ~now ~home =
  Errors.guard (fun () ->
      let data = Render_data.load db ~settings ~now in
      atomic_write out (Render_md.render ~home data);
      match notes_dir with
      | None -> Ok { index_md = out; notes_html = None; deleted = []; warnings = [] }
      | Some notes_dir -> (
          match write_notes ~notes_dir ~now (Render_html.render ~home data) with
          | Ok (path, (deleted, warnings)) ->
              Ok { index_md = out; notes_html = Some path; deleted; warnings }
          | Error w ->
              Ok { index_md = out; notes_html = None; deleted = []; warnings = [ w ] }))
