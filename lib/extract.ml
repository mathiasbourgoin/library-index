let core =
  [
    Extract_ocaml.extractor;
    Extract_ts.extractor;
    Extract_rust.extractor;
    Extract_go.extractor;
    Extract_python.extractor;
    Extract_c.extractor;
    Extract_quint.extractor;
    Extract_rocq.extractor;
  ]

let detected ctx = List.filter (fun (e : Api_item.extractor) -> e.detect ctx) core

let select ctx =
  List.sort_uniq compare
    (List.concat_map (fun (e : Api_item.extractor) -> e.select ctx) (detected ctx))

let for_path p = List.find_opt (fun (e : Api_item.extractor) -> e.handles p) core

let items ~path text =
  if Api_item.generated_text text then []
  else
    match for_path path with
    | None -> []
    | Some e -> (
        (* A text an extractor cannot handle yields no item (the documents still index
           it); a resource exhaustion propagates. *)
        try e.extract ~path text
        with Failure _ | Invalid_argument _ | Not_found | Re.Perl.Parse_error -> [])

let chunks ~path text = List.map Api_item.chunk (items ~path text)
