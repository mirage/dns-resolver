module RNG = Mirage_crypto_rng.Fortuna

let _2s = 2_000_000_000
let ( let@ ) finally fn = Fun.protect ~finally fn
let rec forever () = Mkernel.sleep _2s; forever ()
let rng () = Mirage_crypto_rng_mkernel.initialize (module RNG)
let rng = Mkernel.map rng Mkernel.[]
let ( let* ) = Result.bind

let run _ (cidr, gateway, ipv6) features authenticator =
  Mkernel.(run [ rng; Mnet.stack ~name:"service" ?gateway ~ipv6 cidr ])
  @@ fun rng (daemon, tcp, udp) () ->
  let@ () = fun () -> Mirage_crypto_rng_mkernel.kill rng in
  let@ () = fun () -> Mnet.kill daemon in
  let rng = Mirage_crypto_rng.generate in
  let root = Dns_resolver_shared.Root.reserved in
  let primary = Dns_server.Primary.create ~rng root in
  let tls = Tls.Config.client ~authenticator () in
  let tls = Result.get_ok tls in
  let _resolver, daemon = Resolver.create ~features tls tcp udp primary in
  let@ () = fun () -> Resolver.kill daemon in
  forever ()

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Mkernel.clock_monotonic ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Mkernel.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let fn acc = function `Re re -> re :: acc | _ -> acc in
      let res = List.fold_left fn [] res in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stdout);
  Option.is_none level

let setup_logs =
  let open Term in
  const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity

let features =
  let open Arg in
  let dnssec = info [ "dnssec" ] ~doc:"DNSSec validation" in
  let opportunistic_tls_authoritative =
    let doc = "Opportunistic encryption using TLS to the authoritative" in
    info [ "opportunistic-tls-authoritative" ] ~doc
  in
  let qname_minimisation =
    let doc = "Query name minimisation" in
    info [ "qname-minimisation" ] ~doc
  in
  let flags =
    [
      (`Dnssec, dnssec)
    ; (`Opportunistic_tls_authoritative, opportunistic_tls_authoritative)
    ; (`Qname_minimisation, qname_minimisation)
    ]
  in
  let defaults = [] in
  value & vflag_all defaults flags

let authenticator =
  let parser str =
    let* fn = X509.Authenticator.of_string str in
    Ok (fn, str)
  in
  let pp ppf (_fn, str) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let authenticator =
  let doc = "X.509 authenticator to validate TLS certificates." in
  let open Arg in
  value
  & opt (some authenticator) None
  & info [ "a"; "authenticator" ] ~doc ~docv:"AUTHENTICATOR"

let setup_authenticator = function
  | None ->
      let authenticator = Ca_certs_nss.authenticator () in
      Result.get_ok authenticator
  | Some (fn, _) -> fn (Fun.compose Option.some Resolver.wall)

let setup_authenticator =
  let open Term in
  const setup_authenticator $ authenticator

let term =
  let open Term in
  const run $ setup_logs $ Mnet_cli.setup $ features $ setup_authenticator

let cmd =
  let info = Cmd.info "pagejaune" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
