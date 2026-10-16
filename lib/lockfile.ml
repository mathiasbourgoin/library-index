let classified what e = Error (what ^ ": " ^ Errors.classify e)

let with_lock ~blocking ~busy path f =
  match Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ] 0o600 with
  | exception (Unix.Unix_error _ as e) -> classified "cannot open the lock file" e
  | fd ->
      Fun.protect
        ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
        (fun () ->
          match Unix.lockf fd (if blocking then Unix.F_LOCK else Unix.F_TLOCK) 0 with
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _)
            when not blocking ->
              Error busy
          | exception (Unix.Unix_error _ as e) -> classified "cannot lock the lock file" e
          | () -> f ())
