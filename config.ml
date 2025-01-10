(* mirage >= 4.8.1 & < 4.9.0 *)
(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage
let dns_handler =
  let packages =
    [
      package "logs";
      package "dns" ~min:"9.0.0" ~max:"9.1.0";
      package "dns-server" ~min:"9.0.0" ~max:"9.1.0";
      package "dns-mirage" ~min:"9.0.0" ~max:"9.1.0";
      package "dnssec" ~min:"9.0.0" ~max:"9.1.0";
      package ~sublibs:[ "mirage" ] "dns-resolver" ~min:"9.0.0" ~max:"9.1.0";
    ]
  in
   main
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> job)

let () =
  register ~reporter:no_reporter "resolver" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4v6 default_network ]
