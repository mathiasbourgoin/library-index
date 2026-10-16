(* Shared fixtures of the round-3 hardening suites (test_hardening, test_hardening_misc):
   temp data directories holding a synthetic sources.toml, git commits on temp checkouts
   and a campaign checkout. Names are fictional. *)

open Library_index
open Support
module F = Doc_fixture

let contains s sub = Re.execp (Re.compile (Re.str sub)) s
let sentinel = "ZQX-HARDENING-SENTINEL"

let data_ctx ?(config = "") () =
  let ctx = empty_ctx () in
  let path = Filename.concat ctx.data_dir Config.file_name in
  if config = "" then copy_file (fixture "sources.test.toml") path
  else F.write_file path config;
  ctx

let config_text (ctx : Ctx.t) = read_file (Filename.concat ctx.data_dir Config.file_name)
let get ctx key = ok_or_fail (Ops.get ctx (`Assoc [ ("repo", `String key) ]))

let scalar ctx sql key =
  Option.join
    (Db.query_one ctx.Ctx.db sql [ Db.text key ] (fun r -> Db.to_string_opt r.(0)))

let commit dir ~author =
  let q = Filename.quote dir in
  let cmd =
    Printf.sprintf
      "GIT_CONFIG_NOSYSTEM=1 git -C %s -c user.name=%s -c user.email=a@example.org -c \
       commit.gpgsign=false commit -q -m init"
      q (Filename.quote author)
  in
  if Sys.command cmd <> 0 then failwith "git commit failed"

(* A checkout of a campaign remote that the forge cannot read. *)
let campaign_checkout () =
  let dev = temp_dir () in
  let dir = Filename.concat dev "secret-campaign" in
  F.git_checkout dir [ ("README.md", "# Secret\n\n" ^ sentinel ^ " exploit notes.\n") ];
  commit dir ~author:"Ada Example";
  ignore
    (Sys.command
       ("git -C " ^ Filename.quote dir
      ^ " remote add origin git@gitlab.com:acme-campaigns/secret-campaign.git"));
  dev
