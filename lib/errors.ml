let unix_name e =
  match e with
  | Unix.EACCES -> "EACCES"
  | Unix.ENOENT -> "ENOENT"
  | Unix.ENOSPC -> "ENOSPC"
  | Unix.EROFS -> "EROFS"
  | Unix.EEXIST -> "EEXIST"
  | Unix.EISDIR -> "EISDIR"
  | Unix.ENOTDIR -> "ENOTDIR"
  | Unix.EPERM -> "EPERM"
  | Unix.EINTR -> "EINTR"
  | _ -> "other"

let classify = function
  | Db.Db_error m -> m
  | Unix.Unix_error (e, _, _) -> "I/O error (" ^ unix_name e ^ ")"
  | Sys_error _ -> "I/O error"
  | Sqlite3.Error _ | Sqlite3.SqliteError _ -> "database error"
  | Out_of_memory -> "out of memory"
  | _ -> "internal error"

let guard f = try f () with e -> Error (classify e)
