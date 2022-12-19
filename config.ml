(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let with_dnssec =
  let doc = Key.Arg.info ~doc:"Use DNSSEC when it's possible to resolve domain-names." [ "with-dnssec" ] in
  Key.(create "with_dnssec" Arg.(opt bool false doc))

let dns_handler =
  let packages =
    [
      package "logs";
      package "dns" ~min:"6.4.1" ~max:"6.5.0";
      package "dns-server" ~min:"6.4.1" ~max:"6.5.0";
      package "dns-mirage" ~min:"6.4.1" ~max:"6.5.0";
      package "dnssec" ~min:"6.4.1" ~max:"6.5.0";
      package ~sublibs:[ "mirage" ] "dns-resolver" ~min:"6.4.1" ~max:"6.5.0";
    ]
  in
  foreign
    ~packages
    ~keys:[ Key.v with_dnssec ]
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> job)

let () =
  register "resolver" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4v6 default_network ]
