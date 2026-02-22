(* XXX(dinosaure): design & implementation

   The design and implementation of the DNS resolver is as follows:
   - A DNS resolver can respond to queries from UDP, TCP and possibly TLS (if a
     TLS configuration is offered).
   - A DNS resolver can send queries and wait for responses from DNS servers via
     UDP, TCP or TLS (if the [opportunistict-tls-authoritative] option is given)

   Thus, a DNS resolver acts as a server on one side but also as a client on
   the other side with authoritative DNS servers.

   The implementation is therefore divided into several parts dealing
   exclusively with the client side and the server side. We will now explain a
   notable difference between UDP and TCP and/or TLS communication.

   With regard to UDP communication, the server waits for queries and our
   [Dns_resolver.t] state will respond to these queries as soon as it has the
   opportunity. This state is responsible for correctly linking the responses
   to the queries (in particular by the DNS packet ID).

   These queries may require our resolver to also send these queries to other
   authoritative DNS servers. In this case, we send these queries to the
   servers and then wait for a response, which we then send to our
   [Dns_resolver.t] state. [handle_query3] implements querying an external
   authoritative DNS server and transferring the response to our state.

   With regard to TCP/TLS, the process is a little more complex. This is
   because the cost of initiating a TCP/TLS connection is not insignificant. If
   we need to send a query to a DNS server via TCP/TLS, we can possibly reuse an
   already initiated connection instead of initiating a new one. Thus, the
   connection to a DNS server via TCP/TLS is managed by the [new_connection]
   function (with an abstraction on TCP/TLS). This task has two subtasks: a
   [reader] that does not exceed 2 seconds and a packet [writer] according to a
   queue.

   The objective is to be able to take the opportunity to send packets "at the
   same time" while attempting to read several packets from the external
   authoritative DNS server (still within the 2-second limit). In this way,
   sending a query to an external authoritative DNS server first involves
   searching for an active connection with that server (see our [t.outs]). If
   one is found, an attempt is made to notify the task, using [Qout.t], of the
   sending of a new query. At the same time, DNS packets are read from this
   external authoritative DNS server and the task ends after 2 seconds. This
   task of reading/writing to an external authoritative DNS server is
   implemented in the [new_connection] function.

   The attempt to search for an already active connection with the external
   authoritative DNS server is implemented in the [handle_query0] function and
   in the [try_on_existing_connections].

   If the query could not be sent (because the 1 seconds elapsed before anything
   could be written), an attempt is made to create a new TCP connection with
   the external authoritative DNS server (this attempt is implemented in
   [handle_query2]). Finally, if the query still cannot be sent, a last attempt
   is made by sending the query via UDP ([handle_query3]).

   ---

   From an observation, it seems slow to trust only on TCP/TLS connections. So
   we do both: we try to send the packet via TCP/TLS and we also send the packet
   via UDP. We see then which one is faster.

   ---

   In other words, if a query must be sent via TCP (without [opportunistic]
   mode), here are the three attempts:
   - [handle_query0]: to see if there is already an active connection
   - [handle_query2]: if there is no connection, a new one is created
   - [handle_query3]: if the TCP connection is impossible, we try via UDP

   In the case of the TCP/TLS server, a task is used to retrieve new clients
   and for each client, we instantiate a new task implemented by the
   [incoming_connection] function, which retrieves the queries and potentially
   responds until the client decides to close the connection.

   All these operations have one thing in common: transmitting queries
   (server side) and responses (client side) to our state. To do this, a
   [tick] task exists and waits for packets for 2 seconds and re-dispatches the
   questions to existing clients or creates new client tasks to external DNS
   servers and re-dispatches the responses to active connections with our
   clients who asked us the questions or directly via a simple UDP packet. *)

let src = Logs.Src.create "resolver"

module Log = (val Logs.src_log src : Logs.LOG)

type pktin = {
    conn: [ `Udp | `Tcp ]
  ; dst: Ipaddr.t
  ; port: int
  ; data: string
  ; mono: int64
  ; wall: Ptime.t
  ; query: bool
}

type pktout = { ivar: bool Miou.Computation.t; packet: string }

module Qout = struct
  type t = {
      queue: pktout Queue.t
    ; mutex: Miou.Mutex.t
    ; condition: Miou.Condition.t
  }

  let create () =
    {
      queue= Queue.create ()
    ; mutex= Miou.Mutex.create ()
    ; condition= Miou.Condition.create ()
    }

  let push t packet =
    let ivar = Miou.Computation.create () in
    Miou.Mutex.protect t.mutex @@ fun () ->
    Queue.push { ivar; packet } t.queue;
    Miou.Condition.signal t.condition;
    ivar

  let consume ~fn t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    while Queue.is_empty t.queue do
      Miou.Condition.wait t.condition t.mutex
    done;
    (* NOTE(dinosaure): It is very important to use [peek]/[drop] here rather
       than [pop] because we are certain to have consumed [pktout] only after
       [fn] is complete. However, [fn] can be interleaved with several effects
       (which involve suspensions and give Miou a chance to cancel the task in
       the middle of [fn]).

       The associated finaliser ([cancel]) cleans everything up nicely (and
       ensures that all [Miou.Computation.t] are filled!) but we must take care
       not to lose a [pktout] during a cancellation. *)
    let rec go () =
      match Queue.peek t.queue with
      | pktout -> fn pktout; Queue.drop t.queue; go ()
      | exception Queue.Empty -> ()
    in
    go ()

  let cancel ~fn t =
    Miou.Mutex.protect t.mutex @@ fun () -> Queue.iter fn t.queue
end

type qout = pktout Queue.t

type t = {
    mutable state: Dns_resolver.t
  ; timer: int
  ; outs: (Ipaddr.t, [ `TCP | `TLS ] * Qout.t) Hashtbl.t
  ; ins: (Ipaddr.t * int, Qout.t) Hashtbl.t
  ; user's_space: (Ipaddr.t * int, Qout.t) Hashtbl.t
  ; mutex: Miou.Mutex.t
  ; condition: Miou.Condition.t
  ; queue: pktin Queue.t
  ; tcp: Mnet.TCP.state
  ; udp: Mnet.UDP.state
  ; cfg: Tls.Config.client
  ; opportunistic: bool
  ; orphans: unit Miou.orphans
}

let nsec_per_day = 86_400 * 1_000_000_000
and ps_per_ns = 1_000L

let now () =
  let nsec = Mkernel.clock_wall () in
  let days = nsec / nsec_per_day in
  let rem_ns = nsec mod nsec_per_day in
  let rem_ps = Int64.mul (Int64.of_int rem_ns) ps_per_ns in
  Ptime.v (days, rem_ps)

exception Timeout

let with_timeout ~timeout fn =
  let prm0 = Miou.async @@ fun () -> Mkernel.sleep timeout; raise Timeout in
  let prm1 = Miou.async fn in
  match Miou.await_first [ prm0; prm1 ] with
  | Error Timeout -> Error `Timeout
  | Ok value -> Ok value
  | Error exn -> Error (`Exn exn)

let _1s = 500_000_000
let _2s = 2_000_000_000
let _2d = Float.to_int 1.728e+14

module type FLOW = sig
  type flow

  val connect : t -> Ipaddr.t * int -> flow
  val peers : flow -> (Ipaddr.t * int) * (Ipaddr.t * int)
  val really_read : flow -> ?off:int -> ?len:int -> bytes -> unit
  val close : flow -> unit
  val write : flow -> ?off:int -> ?len:int -> string -> unit
end

module A = struct
  include Mnet.TCP

  let connect t (dst, port) = connect t.tcp (dst, port)
end

module B = struct
  include Mnet_tls

  type flow = t

  let peers flow = Mnet.TCP.peers (file_descr flow)

  let connect t (dst, port) =
    let fn () =
      let flow = Mnet.TCP.connect t.tcp (dst, port) in
      client_of_fd t.cfg flow
    in
    match with_timeout ~timeout:_1s fn with
    | Ok flow -> flow
    | Error `Timeout -> raise Timeout
    | Error (`Exn exn) -> raise exn
end

let incoming_connection : type flow.
    (module FLOW with type flow = flow) -> t -> Qout.t -> flow -> unit =
 fun (module Flow) ->
  ();
  fun t qout flow ->
    let _, (dst, port) = Flow.peers flow in
    Log.debug (fun m -> m "new conn/DNS packet from %a:%d" Ipaddr.pp dst port);
    let finally (qout, flow) =
      let fn { ivar; _ } = ignore (Miou.Computation.try_return ivar false) in
      Qout.cancel ~fn qout;
      Hashtbl.remove t.ins (dst, port);
      try Flow.close flow with _ -> ()
    in
    let o = Miou.Ownership.create ~finally (qout, flow) in
    Miou.Ownership.own o;
    let rec reader () =
      let hdr = Bytes.create 2 in
      Flow.really_read flow hdr;
      let len = Bytes.get_uint16_be hdr 0 in
      let buf = Bytes.create len in
      Flow.really_read flow buf;
      let () =
        Miou.Mutex.protect t.mutex @@ fun () ->
        let data = Bytes.unsafe_to_string buf in
        let conn = `Tcp in
        let mono = Int64.of_int (Mkernel.clock_monotonic ()) in
        let wall = now () in
        let query = true in
        let pkt = { conn; dst; port; data; mono; wall; query } in
        Queue.push pkt t.queue;
        Miou.Condition.signal t.condition
      in
      reader ()
    in
    let rec writer () =
      let fn { packet; ivar } =
        Flow.write flow packet;
        assert (Miou.Computation.try_return ivar true)
      in
      Qout.consume ~fn qout; writer ()
    in
    let prm0 = Miou.async reader and prm1 = Miou.async writer in
    (* NOTE(dinosaure): here, we already safely close our incoming flow with
       our [finally] function. If [prm0] fails (due to a reading error) and/or
       [prm1] fails (due to a writing error), we close our incoming flow and
       we report the remaining packets that we have not yet been able to send as
       not having been sent. *)
    ignore (Miou.await_first [ prm0; prm1 ]);
    finally (qout, flow);
    Miou.Ownership.disown o

let incoming_tcp_connection = incoming_connection (module A)
let incoming_tls_connection = incoming_connection (module B)

let new_connection : type flow.
    (module FLOW with type flow = flow) -> t -> Qout.t -> Ipaddr.t * int -> unit
    =
 fun (module Flow) ->
  ();
  fun t qout (dst, port) ->
    let finally0 qout =
      Log.debug (fun m ->
          m "%a:%d errored, clean-up everything" Ipaddr.pp dst port);
      let fn { ivar; _ } = ignore (Miou.Computation.try_return ivar false) in
      Qout.cancel ~fn qout; Hashtbl.remove t.outs dst
    in
    let o0 = Miou.Ownership.create ~finally:finally0 qout in
    Miou.Ownership.own o0;
    Log.debug (fun m -> m "new connection to %a:%d" Ipaddr.pp dst port);
    let flow =
      try Flow.connect t (dst, port)
      with exn ->
        Log.err (fun m ->
            m "Got an error while connecting to %a:%d: %s" Ipaddr.pp dst port
              (Printexc.to_string exn));
        raise exn
    in
    Log.debug (fun m -> m "connected to %a:%d" Ipaddr.pp dst port);
    let finally1 flow = try Flow.close flow with _ -> () in
    let o1 = Miou.Ownership.create ~finally:finally1 flow in
    Miou.Ownership.own o1;
    let rec reader () =
      let hdr = Bytes.create 2 in
      Flow.really_read flow hdr;
      let len = Bytes.get_uint16_be hdr 0 in
      let buf = Bytes.create len in
      Flow.really_read flow buf;
      let () =
        Miou.Mutex.protect t.mutex @@ fun () ->
        let data = Bytes.unsafe_to_string buf in
        Log.debug (fun m -> m "<- @[<hov>%a@]" (Hxd_string.pp Hxd.default) data);
        let conn = `Tcp in
        let mono = Int64.of_int (Mkernel.clock_monotonic ()) in
        let wall = now () in
        let query = false in
        let pkt = { conn; dst; port; data; mono; wall; query } in
        Queue.push pkt t.queue;
        Miou.Condition.signal t.condition
      in
      reader ()
    in
    let rec writer () =
      let fn { packet; ivar } =
        Flow.write flow packet;
        assert (Miou.Computation.try_return ivar true)
      in
      Qout.consume ~fn qout; writer ()
    in
    let bounded_reader () =
      match with_timeout ~timeout:_2s reader with
      | Ok () -> ()
      | Error `Timeout -> ()
      | Error (`Exn exn) ->
          Log.err (fun m ->
              m "Unexpected exception from the reader on %a:%d: %s" Ipaddr.pp
                dst port (Printexc.to_string exn))
    in
    (* TODO(dinosaure): move [o0] to [prm0]. *)
    let prm0 = Miou.async writer in
    let prm1 = Miou.async bounded_reader in
    ignore (Miou.await_first [ prm0; prm1 ]);
    ignore (finally1 flow);
    Miou.Ownership.disown o1;
    ignore (finally0 qout);
    Miou.Ownership.disown o0

let new_tcp_connection = new_connection (module A)
let new_tls_connection = new_connection (module B)

(*/*)

let try_on_existing_connections t (_protocol, dst, data) =
  match Hashtbl.find_opt t.outs dst with
  | Some ((`TCP | `TLS), qout) ->
      let ivar = Qout.push qout data in
      Miou.Computation.await_exn ivar
  | None -> false

let random_src_port =
  let buf = Bytes.create 2 in
  fun () ->
    Mirage_crypto_rng.generate_into buf 2;
    Bytes.get_uint16_ne buf 0

let handle_query3 t (_protocol, dst, data) =
  let ( let* ) = Result.bind in
  let src_port = random_src_port () in
  Log.debug (fun m -> m "query *:%d -> %a:%d" src_port Ipaddr.pp dst 53);
  let* () = Mnet.UDP.sendto t.udp ~dst ~src_port ~port:53 data in
  let buf = Bytes.create 1500 in
  let fn () = Mnet.UDP.recvfrom t.udp ~port:src_port buf in
  let* len, _ = with_timeout ~timeout:_2s fn in
  let data = Bytes.sub_string buf 0 len in
  let conn = `Udp in
  let mono = Int64.of_int (Mkernel.clock_monotonic ()) in
  let wall = now () in
  let query = false in
  Log.debug (fun m ->
      m "new answer from *:%d -> %a:%d (mono: %Ldns, wall: %a)" src_port
        Ipaddr.pp dst 53 mono (Ptime.pp_human ()) wall);
  Log.debug (fun m -> m "<~ @[<hov>%a@]" (Hxd_string.pp Hxd.default) data);
  let pkt = { conn; dst; port= 53; data; mono; wall; query } in
  Miou.Mutex.protect t.mutex @@ fun () ->
  Queue.push pkt t.queue;
  Miou.Condition.signal t.condition;
  Ok ()

let handle_query3 t elt =
  ignore
    begin
      Miou.async ~orphans:t.orphans @@ fun () ->
      match handle_query3 t elt with Ok () -> () | Error _err -> ()
    end

let handle_query2 t ((_protocol, dst, data) as elt) =
  let qout = Qout.create () in
  let ivar = Qout.push qout data in
  Hashtbl.add t.outs dst (`TCP, qout);
  let _ =
    Miou.async ~orphans:t.orphans @@ fun () ->
    new_tcp_connection t qout (dst, 53)
  in
  let sent = Miou.Computation.await_exn ivar in
  if not sent then handle_query3 t elt

let handle_query1 t ((protocol, dst, data) as elt) =
  let qout = Qout.create () in
  let ivar = Qout.push qout data in
  Hashtbl.add t.outs dst (`TLS, qout);
  let _ =
    Miou.async ~orphans:t.orphans @@ fun () ->
    new_tls_connection t qout (dst, 853)
  in
  let sent = Miou.Computation.await_exn ivar in
  (* TODO(dinosaure): can we re-try on existing connections? *)
  if not sent then
    match protocol with
    | `Udp -> handle_query3 t elt
    | `Tcp -> handle_query2 t elt

let handle_query0 t ((protocol, _dst, _data) as elt) =
  handle_query3 t elt;
  let sent = try_on_existing_connections t elt in
  Log.debug (fun m -> m "packet sent via existing connection? %b" sent);
  if not sent then
    match (protocol, t.opportunistic) with
    | (`Udp | `Tcp), true -> handle_query1 t elt
    | `Tcp, false -> handle_query2 t elt
    | `Udp, false -> ()

(*/*)

let handle_answer_on_udp t (_protocol, dst, port, _ttl, data) =
  match Mnet.UDP.sendto t.udp ~dst ~src_port:53 ~port data with
  | Ok () -> ()
  | Error _ ->
      Log.err (fun m -> m "Impossible to answer to %a:%d" Ipaddr.pp dst port)

let handle_answer_on_conn t (_protocol, dst, port, _ttl, data) =
  Log.debug (fun m ->
      m "search an active connection with %a:%d" Ipaddr.pp dst port);
  let from_connection = Hashtbl.find_opt t.ins (dst, port) in
  let from_user's_space = Hashtbl.find_opt t.user's_space (dst, port) in
  match (from_connection, from_user's_space) with
  | Some qout, None | None, Some qout ->
      Log.debug (fun m ->
          m "found an active connection (user's space: %b)"
            (Option.is_some from_user's_space));
      let buf = Bytes.create (2 + String.length data) in
      Bytes.blit_string data 0 buf 2 (String.length data);
      Bytes.set_uint16_be buf 0 (String.length data);
      let ivar = Qout.push qout (Bytes.unsafe_to_string buf) in
      let sent = Miou.Computation.await_exn ivar in
      if not sent then
        Log.err (fun m -> m "Impossible to answer to %a:%d" Ipaddr.pp dst port)
      else Log.debug (fun m -> m "answer sent to %a:%d" Ipaddr.pp dst port)
  | None, None ->
      Logs.err (fun m ->
          m "Impossible to answer to %a:%d: no active connections" Ipaddr.pp dst
            port)
  | Some _, Some _ -> assert false

let handle_answer t (protocol, dst, port, ttl, data, _, _, _, _) =
  match protocol with
  | `Udp -> handle_answer_on_udp t (protocol, dst, port, ttl, data)
  | `Tcp -> handle_answer_on_conn t (protocol, dst, port, ttl, data)

(*/*)

let rec on_udp t =
  let buf = Bytes.create 4096 in
  let len, (peer, pport) = Mnet.UDP.recvfrom t.udp ~port:53 buf in
  Log.debug (fun m -> m "new UDP/DNS packet from %a:%d" Ipaddr.pp peer pport);
  let data = Bytes.sub_string buf 0 len in
  let conn = `Udp in
  let port = pport in
  let mono = Int64.of_int (Mkernel.clock_monotonic ()) in
  let wall = now () in
  let query = true in
  let pkt = { conn; dst= peer; port; data; mono; wall; query } in
  let () =
    Miou.Mutex.protect t.mutex @@ fun () ->
    Queue.push pkt t.queue;
    Miou.Condition.signal t.condition
  in
  on_udp t

let rec clean_up orphans =
  match Miou.care orphans with
  | Some None | None -> ()
  | Some (Some prm) -> begin
      match Miou.await prm with
      | Ok () -> clean_up orphans
      | Error Timeout -> clean_up orphans
      | Error exn ->
          Log.err (fun m ->
              m "Unexpected exception from a promise: %s"
                (Printexc.to_string exn));
          clean_up orphans
    end

let on_tcp t =
  let rec go orphans listen =
    clean_up orphans;
    let flow = Mnet.TCP.accept t.tcp listen in
    let _, (dst, port) = Mnet.TCP.peers flow in
    Log.debug (fun m ->
        m "new TCP/DNS connection from %a:%d" Ipaddr.pp dst port);
    let _ =
      Miou.async ~orphans @@ fun () ->
      let qout = Qout.create () in
      Hashtbl.replace t.ins (dst, port) qout;
      incoming_tcp_connection t qout flow
    in
    go orphans listen
  in
  go (Miou.orphans ()) (Mnet.TCP.listen t.tcp 53)

let on_tls t cfg =
  let rec go orphans listen =
    let flow = Mnet.TCP.accept t.tcp listen in
    let _ =
      Miou.async ~orphans @@ fun () ->
      let qout = Qout.create () in
      let _, (dst, port) = Mnet.TCP.peers flow in
      Hashtbl.replace t.ins (dst, port) qout;
      let fn () = Mnet_tls.server_of_fd cfg flow in
      match with_timeout ~timeout:_2s fn with
      | Ok flow -> incoming_tls_connection t qout flow
      | Error _ -> Hashtbl.remove t.ins (dst, port)
    in
    go orphans listen
  in
  go (Miou.orphans ()) (Mnet.TCP.listen t.tcp 853)

let listener t () =
  Miou.Mutex.protect t.mutex @@ fun () ->
  while Queue.is_empty t.queue do
    Miou.Condition.wait t.condition t.mutex
  done;
  let elts = Queue.to_seq t.queue in
  let elts = List.of_seq elts in
  Queue.clear t.queue; elts

let rec tick timeout t =
  Log.debug (fun m -> m "resolver tick (timeout: %d)" timeout);
  clean_up t.orphans;
  let now = Fun.compose Int64.of_int Mkernel.clock_wall in
  let t0 = Mkernel.clock_monotonic () in
  match with_timeout ~timeout (listener t) with
  | Ok elts ->
      let fn (answers, queries) pktin =
        let state, answers', queries' =
          Dns_resolver.handle_buf t.state pktin.wall pktin.mono pktin.query
            pktin.conn pktin.dst pktin.port pktin.data
        in
        t.state <- state;
        let answers = List.rev_append answers' answers in
        let queries = List.rev_append queries' queries in
        (answers, queries)
      in
      Log.debug (fun m -> m "handle %d DNS packet(s)" (List.length elts));
      let answers, queries = List.fold_left fn ([], []) elts in
      Log.debug (fun m ->
          m "%d answer(s) and %d querie(s) produced" (List.length answers)
            (List.length queries));
      List.iter (handle_answer t) answers;
      List.iter (handle_query0 t) queries;
      let t1 = Mkernel.clock_monotonic () in
      let delta = t1 - t0 in
      let timeout = timeout - delta in
      let timeout = if timeout <= 0 then t.timer else timeout in
      tick timeout t
  | Error `Timeout ->
      let state, answers, queries = Dns_resolver.timer t.state (now ()) in
      t.state <- state;
      List.iter (handle_answer t) answers;
      List.iter (handle_query0 t) queries;
      tick t.timer t
  | Error (`Exn _exn) -> tick t.timer t

type daemon = {
    tcp_server: unit Miou.t
  ; tls_server: unit Miou.t option
  ; udp_server: unit Miou.t
  ; ticker: unit Miou.t
}

let kill daemon =
  Miou.cancel daemon.tcp_server;
  Miou.cancel daemon.udp_server;
  Option.iter Miou.cancel daemon.tls_server;
  Miou.cancel daemon.ticker

let nsec_per_day = Int64.mul 86_400L 1_000_000_000L
let ps_per_ns = 1_000L

let now_d_ps () =
  let nsec = Mkernel.clock_wall () in
  let nsec = Int64.of_int nsec in
  let days = Int64.div nsec nsec_per_day in
  let rem_ns = Int64.rem nsec nsec_per_day in
  let rem_ps = Int64.mul rem_ns ps_per_ns in
  (Int64.to_int days, rem_ps)

let wall () = Ptime.v (now_d_ps ())
let monotonic () = Int64.of_int (Mkernel.clock_monotonic ())

let create ?(features = []) ?tls cfg tcp udp primary =
  let rng len = Mirage_crypto_rng.generate len in
  let ip_protocol = `Ipv4_only in
  let state =
    Dns_resolver.create ~ip_protocol features (wall ()) (monotonic ()) rng
      primary
  in
  let opportunistic = List.mem `Opportunistic_tls_authoritative features in
  let t =
    {
      state
    ; timer= _2s
    ; outs= Hashtbl.create 0x100
    ; ins= Hashtbl.create 0x100
    ; user's_space= Hashtbl.create 0x100
    ; mutex= Miou.Mutex.create ()
    ; condition= Miou.Condition.create ()
    ; queue= Queue.create ()
    ; tcp
    ; udp
    ; cfg
    ; opportunistic
    ; orphans= Miou.orphans ()
    }
  in
  let tcp_server = Miou.async @@ fun () -> on_tcp t in
  let fn cfg = Miou.async @@ fun () -> on_tls t cfg in
  let tls_server = Option.map fn tls in
  let udp_server = Miou.async @@ fun () -> on_udp t in
  let ticker = Miou.async @@ fun () -> tick _2s t in
  (t, { tcp_server; tls_server; udp_server; ticker })
