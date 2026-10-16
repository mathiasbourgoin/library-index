let curated_file = "curated.json"
let whitehat_file = "whitehat_tools.json"
let lock_file = ".export.lock"

let with_lock dir f =
  Lockfile.with_lock ~blocking:true ~busy:"export lock busy"
    (Filename.concat dir lock_file)
    f

let atomic_write dir name contents = Fsio.atomic_write (Filename.concat dir name) contents
let mark db ok = Db.set_meta db "export_dirty" (if ok then "0" else "1")

(* The DB snapshot is read, the files written and a success recorded in [export_dirty]
   while holding the lock. Otherwise a writer that committed earlier but reaches the lock
   later would overwrite a newer export with its older snapshot (lost update), and a late
   success could clear the flag a concurrent failure had just set. Any failure (lock
   included) sets the flag, which is always safe. WAL readers never wait on the lock
   file, and the snapshot is a few milliseconds on ~1100 rows. *)
let write_locked db ~dir ~curated ~whitehat ~track =
  let r =
    Errors.guard (fun () ->
        with_lock dir (fun () ->
            if curated then
              atomic_write dir curated_file
                (Json_out.to_string (Curated_io.curated_json db));
            if whitehat then
              atomic_write dir whitehat_file
                (Json_out.to_string (Curated_io.whitehat_json db));
            if track then mark db true;
            Ok ()))
  in
  (match r with
  | Error _ when track -> ( try mark db false with Db.Db_error _ -> ())
  | _ -> ());
  r

let write_files db ~dir ~curated ~whitehat =
  write_locked db ~dir ~curated ~whitehat ~track:false

let same_dir a b =
  try
    let sa = Unix.stat a and sb = Unix.stat b in
    sa.st_dev = sb.st_dev && sa.st_ino = sb.st_ino
  with Unix.Unix_error _ -> String.equal a b

let export (ctx : Ctx.t) ~dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    Error
      (Printf.sprintf "export failed: directory %s does not exist (create it first)" dir)
  else
    Result.map_error
      (fun e -> "export failed: " ^ e)
      (write_locked ctx.db ~dir ~curated:true ~whitehat:true
         ~track:(same_dir dir ctx.export_dir))

let rewrite (ctx : Ctx.t) ~whitehat =
  Result.map_error
    (fun e -> "DB updated, export failed: " ^ e)
    (write_locked ctx.db ~dir:ctx.export_dir ~curated:true ~whitehat ~track:true)

let after_write (ctx : Ctx.t) ~whitehat_changed =
  rewrite ctx ~whitehat:(whitehat_changed || Db.export_dirty ctx.db)

let retry_if_dirty (ctx : Ctx.t) =
  if Db.export_dirty ctx.db then rewrite ctx ~whitehat:true else Ok ()
