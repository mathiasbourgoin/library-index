type failure = [ `Auth | `Not_found | `Rate_limit | `Network | `Other ]
type runner = timeout:float -> string array -> (string, failure) result

let default_timeout = 30.
let max_output = 64 * 1024 * 1024
let max_stderr = 64 * 1024

let describe = function
  | `Auth -> "auth"
  | `Not_found -> "not-found"
  | `Rate_limit -> "rate-limit"
  | `Network -> "network"
  | `Other -> "other"

(* Phrases match on word boundaries, so "tls" never matches inside a word and a bare
   "not found" (e.g. "command not found") is not a 404. *)
let any_of words =
  let phrase w = Re.seq [ Re.bow; Re.str w; Re.eow ] in
  let re = Re.compile (Re.no_case (Re.alt (List.map phrase words))) in
  fun s -> Re.execp re s

let is_rate_limit =
  any_of
    [ "rate limit"; "rate-limit"; "http 429"; "too many requests"; "abuse detection" ]

let is_auth =
  any_of
    [
      "http 401";
      "401 unauthorized";
      "bad credentials";
      "not logged in";
      "auth login";
      "authentication required";
      "authentication failed";
      "requires authentication";
      "token is invalid";
      "invalid token";
    ]

let is_not_found =
  any_of [ "http 404"; "404 not found"; "404 project not found"; "404 file not found" ]

let is_network =
  any_of
    [
      "could not resolve";
      "no such host";
      "connection refused";
      "connection reset";
      "network is unreachable";
      "timeout";
      "timed out";
      "i/o timeout";
      "tls handshake";
      "tls handshake timeout";
      "unexpected eof";
      "http 500";
      "http 502";
      "http 503";
      "http 504";
      "dial tcp";
      "error connecting to";
      "failed to connect";
    ]

let classify_failure s =
  if is_rate_limit s then `Rate_limit
  else if is_auth s then `Auth
  else if is_not_found s then `Not_found
  else if is_network s then `Network
  else `Other

(* ------------------------------------------------------------ the real runner *)

type stream = { fd : Unix.file_descr; buf : Buffer.t; mutable open_ : bool }

let close_quietly fd = try Unix.close fd with Unix.Unix_error _ -> ()

(* Appends what is available on [s] to its buffer (at most [cap] bytes are kept; the
   rest is read and dropped); closes it on EOF or error. *)
let drain ~cap bytes s =
  match Unix.read s.fd bytes 0 (Bytes.length bytes) with
  | 0 ->
      s.open_ <- false;
      close_quietly s.fd
  | n ->
      let room = cap - Buffer.length s.buf in
      if room > 0 then Buffer.add_subbytes s.buf bytes 0 (min n room)
  | exception Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN), _, _) -> ()
  | exception Unix.Unix_error _ ->
      s.open_ <- false;
      close_quietly s.fd

let kill_and_reap pid =
  (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
  try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ()

(* Pumps both pipes until EOF on both, the deadline or the output cap. *)
let pump ~deadline out err =
  let bytes = Bytes.create 65536 in
  let rec loop () =
    let live = List.filter (fun s -> s.open_) [ out; err ] in
    if live = [] then `Done
    else if Buffer.length out.buf > max_output then `Too_big
    else
      let left = deadline -. Unix.gettimeofday () in
      if left <= 0. then `Timeout
      else
        match Unix.select (List.map (fun s -> s.fd) live) [] [] left with
        | ready, _, _ ->
            List.iter
              (fun s ->
                if List.mem s.fd ready then
                  drain ~cap:(if s == err then max_stderr else max_output + 1) bytes s)
              live;
            loop ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

(* Waits for [pid] until [deadline]: a child that closed both pipes but keeps running
   is killed at the deadline like any other overrun. *)
let status_of ~deadline pid =
  let rec wait () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then (
          kill_and_reap pid;
          None)
        else (
          Unix.sleepf 0.01;
          wait ())
    | _, st -> Some st
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
    | exception Unix.Unix_error _ -> None
  in
  wait ()

let spawn argv =
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let err_r, err_w = Unix.pipe ~cloexec:true () in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  let pid =
    Fun.protect
      ~finally:(fun () -> List.iter close_quietly [ out_w; err_w; null ])
      (fun () ->
        try Some (Unix.create_process argv.(0) argv null out_w err_w)
        with Unix.Unix_error _ -> None)
  in
  match pid with
  | None ->
      List.iter close_quietly [ out_r; err_r ];
      None
  | Some pid -> Some (pid, out_r, err_r)

let run ~timeout argv =
  if Array.length argv = 0 then Error `Other
  else
    match spawn argv with
    | None -> Error `Other
    | Some (pid, out_r, err_r) -> (
        let out = { fd = out_r; buf = Buffer.create 4096; open_ = true } in
        let err = { fd = err_r; buf = Buffer.create 256; open_ = true } in
        let deadline = Unix.gettimeofday () +. timeout in
        let outcome = pump ~deadline out err in
        List.iter (fun s -> if s.open_ then close_quietly s.fd) [ out; err ];
        match outcome with
        | `Timeout ->
            kill_and_reap pid;
            Error `Network
        | `Too_big ->
            kill_and_reap pid;
            Error `Other
        | `Done -> (
            match status_of ~deadline pid with
            | None -> Error `Network
            | Some (Unix.WEXITED 0) -> Ok (Buffer.contents out.buf)
            | Some _ -> Error (classify_failure (Buffer.contents err.buf))))
