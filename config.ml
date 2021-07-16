(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let dns_handler =
  let pin = "git+https://github.com/mirage/ocaml-dns.git#main" in
  let packages =
    [
      package "logs" ;
      package "dns" ~pin;
      package "dns-server" ~pin;
      package "dns-mirage" ~pin;
      package ~sublibs:[ "mirage" ] "dns-resolver" ~pin;
    ]
  in
  foreign
    ~packages
    "Unikernel.Main" (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> job)

let () =
  register "resolver" [dns_handler $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4v6 default_network ]
