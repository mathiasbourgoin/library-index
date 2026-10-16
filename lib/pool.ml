type 'b shared = {
  lock : Mutex.t;
  ready : Condition.t;
  results : ('b, exn) result Queue.t;
  mutable next : int;
}

let worker items f st () =
  let n = Array.length items in
  let rec loop () =
    let i =
      Mutex.protect st.lock (fun () ->
          let i = st.next in
          st.next <- i + 1;
          i)
    in
    if i < n then (
      let r = try Ok (f items.(i)) with e -> Error e in
      Mutex.protect st.lock (fun () ->
          Queue.push r st.results;
          Condition.signal st.ready);
      loop ())
  in
  loop ()

let take_ready st =
  Mutex.protect st.lock (fun () ->
      while Queue.is_empty st.results do
        Condition.wait st.ready st.lock
      done;
      let batch = List.of_seq (Queue.to_seq st.results) in
      Queue.clear st.results;
      batch)

let run ~workers items f on_result =
  let items = Array.of_list items in
  let n = Array.length items in
  if n > 0 then
    let st =
      {
        lock = Mutex.create ();
        ready = Condition.create ();
        results = Queue.create ();
        next = 0;
      }
    in
    let threads =
      List.init (min n (max 1 workers)) (fun _ -> Thread.create (worker items f st) ())
    in
    let received = ref 0 in
    Fun.protect
      ~finally:(fun () -> List.iter Thread.join threads)
      (fun () ->
        while !received < n do
          List.iter
            (fun r ->
              incr received;
              on_result r)
            (take_ready st)
        done)
