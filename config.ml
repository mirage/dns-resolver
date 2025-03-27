(* mirage >= 4.9.0 & < 4.10.0 *)
(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage
let dns_handler =
  let packages =
    [
      package "logs";
      package "dns" ~min:"10.0.0";
      package "dns-server" ~min:"10.0.0";
      package "dns-mirage" ~min:"10.0.0";
      package "dnssec" ~min:"10.0.0";
      package ~sublibs:[ "mirage" ] "dns-resolver" ~min:"10.0.0";
    ]
  in
   main
    ~packages
    "Unikernel.Main" (stackv4v6 @-> job)

let () =
  register ~reporter:no_reporter "resolver" [dns_handler $ generic_stackv4v6 default_network ]
