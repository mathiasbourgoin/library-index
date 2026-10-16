let rec emit buf indent (v : Yojson.Safe.t) =
  let pad n = Buffer.add_string buf (String.make n ' ') in
  match v with
  | `Assoc [] -> Buffer.add_string buf "{}"
  | `List [] -> Buffer.add_string buf "[]"
  | `Assoc members ->
      Buffer.add_string buf "{\n";
      List.iteri
        (fun i (k, x) ->
          if i > 0 then Buffer.add_string buf ",\n";
          pad (indent + 1);
          Buffer.add_string buf (Yojson.Safe.to_string (`String k));
          Buffer.add_string buf ": ";
          emit buf (indent + 1) x)
        members;
      Buffer.add_char buf '\n';
      pad indent;
      Buffer.add_char buf '}'
  | `List items ->
      Buffer.add_string buf "[\n";
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_string buf ",\n";
          pad (indent + 1);
          emit buf (indent + 1) x)
        items;
      Buffer.add_char buf '\n';
      pad indent;
      Buffer.add_char buf ']'
  | scalar -> Buffer.add_string buf (Yojson.Safe.to_string scalar)

let to_string v =
  let buf = Buffer.create 65536 in
  emit buf 0 v;
  Buffer.add_char buf '\n';
  Buffer.contents buf
