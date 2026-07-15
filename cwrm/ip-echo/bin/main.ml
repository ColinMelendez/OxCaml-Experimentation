open Cmdliner

(* Copy a local_ string to a global string.
   Needed because httpz_eio's request_info fields are stack-allocated. *)
let globalize (local_ s : string) : string =
  let len = String.length s in
  let dst = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set dst i (String.unsafe_get s i)
  done;
  Bytes.unsafe_to_string dst

let peer_of_addr = function
  | `Tcp (ip, port) -> (Format.asprintf "%a" Eio.Net.Ipaddr.pp ip, port)
  | `Unix path -> (path, 0)
  | _ -> ("unknown", 0)

let respond_field respond value =
  Httpz_server.Route.plain respond (value ^ "\n")

let make_routes ~ip ~port =
  let open Httpz_server.Route in
  let open Httpz.Header_name in
  let headers =
    User_agent
    +> (Accept_language
       +> (Referer
          +> (Accept_encoding
             +> (Accept
                +> (Accept_charset
                   +> (X_forwarded_for +> (Via +> (Connection +> h0))))))))
  in
  let handle_request
      ( user_agent,
        ( language,
          ( referer,
            ( encoding,
              (mime, (charset, (forwarded, (via, (connection, ()))))) ) ) ) )
      ctx respond =
    let meth = Httpz.Method.to_string (meth ctx) in
    let info =
      Ip_echo.make ~ip ~port ~meth ~user_agent ~language ~referer ~encoding ~mime
        ~charset ~via ~forwarded ~connection
    in
    match path ctx with
    | "/" | "" ->
        if Ip_echo.wants_html ~mime then html respond (Ip_echo.to_html info)
        else respond_field respond (Ip_echo.effective_ip info)
    | "/ip" -> respond_field respond (Ip_echo.effective_ip info)
    | "/ua" -> respond_field respond info.user_agent
    | "/lang" -> respond_field respond info.language
    | "/encoding" -> respond_field respond info.encoding
    | "/mime" -> respond_field respond info.mime
    | "/charset" -> respond_field respond info.charset
    | "/forwarded" -> respond_field respond info.forwarded
    | "/via" -> respond_field respond info.via
    | "/port" -> respond_field respond (string_of_int info.port)
    | "/all" -> plain respond (Ip_echo.to_plain_all info)
    | "/all.json" -> json respond (Ip_echo.to_json info)
    | _ -> not_found respond
  in
  (* Catch-all GET so every path sees the same headers; dispatch by path. *)
  of_list
    [
      get_h tail headers (fun _segments hdrs ctx respond ->
          handle_request hdrs ctx respond);
    ]

let run port bind_host verbose =
  Printf.printf "ip-echo listening on %s:%d\n%!" bind_host port;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let on_request (local_ info : Httpz_eio.request_info) =
    if verbose then
      let meth = Httpz.Method.to_string info.meth in
      let path = globalize info.path in
      let status = Httpz.Res.status_to_string info.status in
      let remote = globalize info.remote_addr in
      Printf.printf "%s %s %s -> %s (%dus)\n%!" remote meth path status
        info.duration_us
  in
  let on_error exn =
    Printf.eprintf "Connection error: %s\n%!" (Printexc.to_string exn)
  in
  let net = Eio.Stdenv.net env in
  let ipaddr =
    match bind_host with
    | "0.0.0.0" | "*" | "any" -> Eio.Net.Ipaddr.V4.any
    | "127.0.0.1" | "localhost" -> Eio.Net.Ipaddr.V4.loopback
    | "::" | "[::]" -> Eio.Net.Ipaddr.V6.any
    | "::1" | "[::1]" -> Eio.Net.Ipaddr.V6.loopback
    | host ->
        try Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host)
        with _ -> failwith ("Invalid bind address: " ^ host)
  in
  let addr = `Tcp (ipaddr, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  Eio.Net.run_server socket
    ~on_error:(fun exn ->
      Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn))
    (fun flow peer ->
      let ip, peer_port = peer_of_addr peer in
      let routes = make_routes ~ip ~port:peer_port in
      Httpz_eio.handle_client ~routes ~on_request ~on_error flow peer)

let port_t =
  Arg.(
    value & opt int 8080
    & info [ "port"; "p" ] ~docv:"PORT"
        ~doc:"Listen port (default: $(b,8080)).")

let bind_t =
  Arg.(
    value & opt string "0.0.0.0"
    & info [ "bind"; "b" ] ~docv:"ADDR"
        ~doc:
          "Address to bind (default: $(b,0.0.0.0)). Use $(b,127.0.0.1) for \
           loopback only.")

let verbose_t =
  Arg.(value & flag & info [ "verbose"; "v" ] ~doc:"Log each request.")

let cmd =
  let doc = "Echo client IP address and HTTP connection information" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "ip-echo is a small HTTP server (similar to ifconfig.me) that reports \
         the connecting client's IP address and selected request headers.";
      `S Manpage.s_examples;
      `Pre
        "  curl localhost:8080/ip\n\
        \  curl localhost:8080/ua\n\
        \  curl localhost:8080/all.json";
      `S Manpage.s_see_also;
      `P "https://ifconfig.me";
    ]
  in
  let info = Cmd.info "ip-echo" ~version:"0.1.0" ~doc ~man in
  let term =
    let open Term.Syntax in
    let+ port = port_t and+ bind_host = bind_t and+ verbose = verbose_t in
    run port bind_host verbose
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
