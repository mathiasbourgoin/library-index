(** Repository keys: [host:owner/name] lowercased, [local:<dir>],
    [whitehat:<campaign>/<path>] (spec Entities, EC-4). *)

type stub = {
  host : string;
  owner : string;
  name : string;
  full_name : string;
  url : string option;
  private_ : bool option;
}
(** Identity columns of a curated-only stub row (spec C-5). *)

val normalize : string -> string
(** [normalize k] trims and lowercases a key. *)

val stub_of_curated_key : string -> (stub, string) result
(** [stub_of_curated_key k] derives host/owner/name from a [curated.json] key. [local:]
    keys get owner ["(local)"] and no URL; [github:]/[gitlab:] keys get the forge URL. An
    unknown host prefix or a malformed path is an error naming the key (EC-1). *)

val whitehat : campaign:string -> path:string -> string
(** [whitehat ~campaign ~path] is ["whitehat:" ^ lowercase (campaign ^ "/" ^ path)]. *)
