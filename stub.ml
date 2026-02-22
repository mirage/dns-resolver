let create ?(with_reserved= true) primary he =
  let rng = Mirage_crypto_rng.generate in
  let primary = Dns_server.Primary.create ~rng Dns_trie.empty in
  let primary =
    if with_reserved then
      let trie = Dns_server.Primary.data primary in
      let trie = Dns_trie.insert_map Dns_resolver_root.reserved_zones trie in
      let primary, _ =
        Dns_server.Primary.with_data primary (wall ()) (now ()) trie
      in
      primary
    else primary
  in
  let server = Dns_server.Primary.server primary in

