let write_all fd s =
  let b = Bytes.unsafe_of_string s in
  let rec go off =
    if off < Bytes.length b then go (off + Unix.write fd b off (Bytes.length b - off))
  in
  go 0

let atomic_write ?perm path contents =
  let tmp =
    Filename.concat (Filename.dirname path)
      (Printf.sprintf ".%s.tmp.%d" (Filename.basename path) (Unix.getpid ()))
  in
  match
    let mode = match perm with Some _ -> 0o600 | None -> 0o666 in
    let fd = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] mode in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        write_all fd contents;
        Unix.fsync fd);
    Option.iter (Unix.chmod tmp) perm;
    Unix.rename tmp path
  with
  | () -> ()
  | exception e ->
      (try Sys.remove tmp with Sys_error _ -> ());
      raise e
