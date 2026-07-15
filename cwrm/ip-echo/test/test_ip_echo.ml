(* Expect-tests for ip-echo connection info and HTTP endpoints. *)

let sample ?(ip = "203.0.113.10") ?(port = 54321)
    ?(meth = "GET") ?(user_agent = Some "curl/8.7.1")
    ?(language = Some "en-GB,en;q=0.9") ?(referer = None)
    ?(encoding = Some "gzip, deflate") ?(mime = Some "*/*")
    ?(charset = None) ?(via = None) ?(forwarded = None)
    ?(connection = Some "close") () =
  Ip_echo.make ~ip ~port ~meth ~user_agent ~language ~referer ~encoding ~mime
    ~charset ~via ~forwarded ~connection

let print_body = function
  | Ip_echo.Plain s -> Printf.printf "plain:%s" s
  | Ip_echo.Html s -> Printf.printf "html:\n%s" s
  | Ip_echo.Json s -> Printf.printf "json:%s\n" s
  | Ip_echo.Not_found -> print_endline "not_found"

let%expect_test "effective_ip prefers first X-Forwarded-For hop" =
  let t = sample ~forwarded:(Some "203.0.113.9, 198.51.100.1") () in
  print_endline (Ip_echo.effective_ip t);
  [%expect {| 203.0.113.9 |}];
  let t = sample ~forwarded:(Some "  198.51.100.2  ") () in
  print_endline (Ip_echo.effective_ip t);
  [%expect {| 198.51.100.2 |}];
  let t = sample ~forwarded:None () in
  print_endline (Ip_echo.effective_ip t);
  [%expect {| 203.0.113.10 |}];
  let t = sample ~forwarded:(Some "") () in
  print_endline (Ip_echo.effective_ip t);
  [%expect {| 203.0.113.10 |}]
;;

let%expect_test "wants_html detects Accept: text/html" =
  let yes = function true -> print_endline "yes" | false -> print_endline "no" in
  yes (Ip_echo.wants_html ~mime:None);
  [%expect {| no |}];
  yes (Ip_echo.wants_html ~mime:(Some "*/*"));
  [%expect {| no |}];
  yes (Ip_echo.wants_html ~mime:(Some "text/html"));
  [%expect {| yes |}];
  yes (Ip_echo.wants_html ~mime:(Some "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"));
  [%expect {| yes |}];
  yes (Ip_echo.wants_html ~mime:(Some "TEXT/HTML"));
  [%expect {| yes |}]
;;

let%expect_test "to_plain_all and field endpoints" =
  let t =
    sample
      ~forwarded:(Some "203.0.113.9, 198.51.100.1")
      ~referer:(Some "https://example.com/")
      ~charset:(Some "utf-8")
      ~via:(Some "1.1 proxy")
      ()
  in
  print_string (Ip_echo.to_plain_all t);
  [%expect
    {|
    ip_addr: 203.0.113.9
    remote_host: unavailable
    user_agent: curl/8.7.1
    port: 54321
    language: en-GB,en;q=0.9
    referer: https://example.com/
    connection: close
    method: GET
    encoding: gzip, deflate
    mime: */*
    charset: utf-8
    via: 1.1 proxy
    forwarded: 203.0.113.9, 198.51.100.1
    |}];
  print_body (Ip_echo.body_for t ~path:"/ip" ~mime:None);
  [%expect {|
    plain:203.0.113.9
    |}];
  print_body (Ip_echo.body_for t ~path:"/ua" ~mime:None);
  [%expect {|
    plain:curl/8.7.1
    |}];
  print_body (Ip_echo.body_for t ~path:"/lang" ~mime:None);
  [%expect {|
    plain:en-GB,en;q=0.9
    |}];
  print_body (Ip_echo.body_for t ~path:"/forwarded" ~mime:None);
  [%expect {|
    plain:203.0.113.9, 198.51.100.1
    |}];
  print_body (Ip_echo.body_for t ~path:"/port" ~mime:None);
  [%expect {|
    plain:54321
    |}];
  print_body (Ip_echo.body_for t ~path:"/nope" ~mime:None);
  [%expect {| not_found |}]
;;

let%expect_test "json escaping and /all.json" =
  let t =
    sample
      ~user_agent:(Some {|say "hi" \ bye|})
      ~referer:(Some "line1\nline2")
      ~language:(Some "a\tb")
      ()
  in
  print_body (Ip_echo.body_for t ~path:"/all.json" ~mime:None);
  [%expect
    {|
    json:{"ip_addr":"203.0.113.10","remote_host":"unavailable","user_agent":"say \"hi\" \\ bye","port":54321,"language":"a\tb","referer":"line1\nline2","connection":"close","method":"GET","encoding":"gzip, deflate","mime":"*/*","charset":"","via":"","forwarded":""}
    |}]
;;

let%expect_test "root is plain IP unless Accept wants HTML" =
  let t = sample () in
  print_body (Ip_echo.body_for t ~path:"/" ~mime:(Some "*/*"));
  [%expect {|
    plain:203.0.113.10
    |}];
  print_body (Ip_echo.body_for t ~path:"/" ~mime:(Some "text/html"));
  [%expect
    {|
    html:
    <!DOCTYPE html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>What Is My IP Address?</title>
    <style>
      :root { color-scheme: light dark; }
      body { font-family: ui-sans-serif, system-ui, sans-serif; margin: 2rem auto;
             max-width: 42rem; padding: 0 1rem; line-height: 1.5; }
      h1 { font-size: 1.5rem; margin-bottom: 0.25rem; }
      .ip { font-size: 2rem; font-weight: 700; letter-spacing: -0.02em;
            margin: 0.5rem 0 1.5rem; font-variant-numeric: tabular-nums; }
      table { width: 100%; border-collapse: collapse; }
      th, td { text-align: left; padding: 0.4rem 0.6rem; border-bottom: 1px solid #8884; }
      th { width: 40%; font-weight: 600; color: #666; }
      @media (prefers-color-scheme: dark) { th { color: #aaa; } }
      code { font-size: 0.9em; }
      .cli { margin-top: 2rem; }
      .cli pre { overflow-x: auto; padding: 0.75rem 1rem; background: #8881;
                 border-radius: 6px; }
    </style>
    </head>
    <body>
    <h1>What Is My IP Address?</h1>
    <p class="ip">203.0.113.10</p>
    <h2>Your Connection</h2>
    <table>
    <tr><th scope="row">IP Address</th><td>203.0.113.10</td></tr>
    <tr><th scope="row">User Agent</th><td>curl/8.7.1</td></tr>
    <tr><th scope="row">Language</th><td>en-GB,en;q=0.9</td></tr>
    <tr><th scope="row">Referer</th><td></td></tr>
    <tr><th scope="row">Method</th><td>GET</td></tr>
    <tr><th scope="row">Encoding</th><td>gzip, deflate</td></tr>
    <tr><th scope="row">MIME Type</th><td>*/*</td></tr>
    <tr><th scope="row">Charset</th><td></td></tr>
    <tr><th scope="row">Via</th><td></td></tr>
    <tr><th scope="row">X-Forwarded-For</th><td></td></tr>
    <tr><th scope="row">Port</th><td>54321</td></tr>
    <tr><th scope="row">Connection</th><td>close</td></tr>
    </table>
    <section class="cli">
    <h2>Command Line</h2>
    <pre><code>curl localhost/ip
    curl localhost/ua
    curl localhost/all
    curl localhost/all.json</code></pre>
    </section>
    </body>
    </html>
    |}]
;;

let%expect_test "HTML escapes angle brackets and ampersands" =
  let t = sample ~user_agent:(Some {|<script>alert("x")</script> & more|}) () in
  (match Ip_echo.body_for t ~path:"/" ~mime:(Some "text/html") with
   | Html html ->
       let needle =
         {|<tr><th scope="row">User Agent</th><td>&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt; &amp; more</td></tr>|}
       in
       if String.split_on_char '\n' html |> List.exists (( = ) needle) then
         print_endline "escaped"
       else print_endline "missing escape"
   | _ -> print_endline "wrong body kind");
  [%expect {| escaped |}]
;;

(* --- HTTP integration ---------------------------------------------------- *)

let peer_of_addr = function
  | `Tcp (ip, port) -> (Format.asprintf "%a" Eio.Net.Ipaddr.pp ip, port)
  | `Unix path -> (path, 0)
  | _ -> ("unknown", 0)

let http_get ~net ~port ?(headers = []) path =
  Eio.Switch.run @@ fun sw ->
  let flow =
    Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let header_lines =
    headers
    |> List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v)
    |> String.concat ""
  in
  let req =
    Printf.sprintf
      "GET %s HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n%s\r\n" path
      header_lines
  in
  Eio.Flow.copy_string req flow;
  let buf = Buffer.create 256 in
  (try
     while true do
       let chunk = Cstruct.create 4096 in
       let n = Eio.Flow.single_read flow chunk in
       Buffer.add_string buf (Cstruct.to_string chunk ~len:n)
     done
   with End_of_file -> ());
  Buffer.contents buf

let status_and_body response =
  match String.split_on_char '\n' response with
  | status :: rest ->
      let status = String.trim status in
      let body =
        let rec skip = function
          | "" :: rest | "\r" :: rest -> String.concat "\n" rest
          | _ :: rest -> skip rest
          | [] -> ""
        in
        skip rest |> String.trim
      in
      (status, body)
  | [] -> ("", "")

let with_server f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen net ~sw ~backlog:5 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> failwith "expected TCP listen address"
  in
  let stop, set_stop = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server ~stop socket ~on_error:(fun _ -> ()) (fun flow peer ->
          let ip, peer_port = peer_of_addr peer in
          Httpz_eio.handle_client
            ~routes:(Ip_echo.make_routes ~ip ~port:peer_port)
            ~on_request:(fun (local_ _) -> ())
            ~on_error:(fun _ -> ())
            flow peer);
      `Stop_daemon);
  Eio.Fiber.yield ();
  Fun.protect
    (fun () -> f ~net ~port)
    ~finally:(fun () -> Eio.Promise.resolve set_stop ())

let%expect_test "HTTP /ip and /ua and forwarded /ip" =
  with_server (fun ~net ~port ->
      let status, body = http_get ~net ~port "/ip" |> status_and_body in
      Printf.printf "%s\n%s\n" status body;
      [%expect
        {|
        HTTP/1.1 200 OK
        127.0.0.1
        |}];
      let status, body =
        http_get ~net ~port "/ua" ~headers:[ ("User-Agent", "ip-echo-test/1.0") ]
        |> status_and_body
      in
      Printf.printf "%s\n%s\n" status body;
      [%expect
        {|
        HTTP/1.1 200 OK
        ip-echo-test/1.0
        |}];
      let status, body =
        http_get ~net ~port "/ip"
          ~headers:[ ("X-Forwarded-For", "203.0.113.9, 198.51.100.1") ]
        |> status_and_body
      in
      Printf.printf "%s\n%s\n" status body;
      [%expect
        {|
        HTTP/1.1 200 OK
        203.0.113.9
        |}];
      let status, _body = http_get ~net ~port "/missing" |> status_and_body in
      Printf.printf "%s\n" status;
      [%expect {| HTTP/1.1 404 Not Found |}])
;;
