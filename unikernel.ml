(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
open Cmdliner


let with_dnssec =
  let doc = Arg.info ~doc:"Use DNSSEC when it's possible to resolve domain-names." [ "with-dnssec" ] in
  Mirage_runtime.register_arg Arg.(value & opt bool false doc)

module Main (R : Mirage_crypto_rng_mirage.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) = struct
  module D = Dns_resolver_mirage.Make(R)(P)(M)(T)(S)

  let start _r _pclock _mclock _ s =
    let now = M.elapsed_ns () in
    let server =
      Dns_server.Primary.create ~rng:R.generate Dns_resolver_root.reserved
    in
    let p = Dns_resolver.create ~dnssec:(with_dnssec ()) now R.generate server in
    D.resolver ~timer:1000 ~root:true s p ;
    S.listen s
end
