type t = {
  ip_addr : string;
  remote_host : string;
  user_agent : string;
  port : int;
  language : string;
  referer : string;
  method_ : string;
  encoding : string;
  mime : string;
  charset : string;
  via : string;
  forwarded : string;
  connection : string;
}

let opt = Option.value ~default:""

let make ~ip ~port ~meth ~user_agent ~language ~referer ~encoding ~mime
    ~charset ~via ~forwarded ~connection =
  {
    ip_addr = ip;
    remote_host = "unavailable";
    user_agent = opt user_agent;
    port;
    language = opt language;
    referer = opt referer;
    method_ = meth;
    encoding = opt encoding;
    mime = opt mime;
    charset = opt charset;
    via = opt via;
    forwarded = opt forwarded;
    connection = opt connection;
  }

let effective_ip t =
  match String.split_on_char ',' t.forwarded with
  | hop :: _ when String.trim hop <> "" -> String.trim hop
  | _ -> t.ip_addr

let wants_html ~mime =
  match mime with
  | None -> false
  | Some s ->
      let lower = String.lowercase_ascii s in
      let has_html =
        let rec scan i =
          if i >= String.length lower then false
          else if
            i + 9 <= String.length lower
            && String.sub lower i 9 = "text/html"
          then true
          else scan (i + 1)
        in
        scan 0
      in
      has_html

let to_plain_all t =
  Printf.sprintf
    "ip_addr: %s\n\
     remote_host: %s\n\
     user_agent: %s\n\
     port: %d\n\
     language: %s\n\
     referer: %s\n\
     connection: %s\n\
     method: %s\n\
     encoding: %s\n\
     mime: %s\n\
     charset: %s\n\
     via: %s\n\
     forwarded: %s\n"
    (effective_ip t) t.remote_host t.user_agent t.port t.language t.referer
    t.connection t.method_ t.encoding t.mime t.charset t.via t.forwarded

let json_escape s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let to_json t =
  Printf.sprintf
    {|{"ip_addr":"%s","remote_host":"%s","user_agent":"%s","port":%d,"language":"%s","referer":"%s","connection":"%s","method":"%s","encoding":"%s","mime":"%s","charset":"%s","via":"%s","forwarded":"%s"}|}
    (json_escape (effective_ip t))
    (json_escape t.remote_host)
    (json_escape t.user_agent)
    t.port
    (json_escape t.language)
    (json_escape t.referer)
    (json_escape t.connection)
    (json_escape t.method_)
    (json_escape t.encoding)
    (json_escape t.mime)
    (json_escape t.charset)
    (json_escape t.via)
    (json_escape t.forwarded)

let html_escape s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let row label value =
  Printf.sprintf
    "<tr><th scope=\"row\">%s</th><td>%s</td></tr>\n" label
    (html_escape value)

let to_html t =
  let ip = effective_ip t in
  let rows =
    String.concat ""
      [
        row "IP Address" ip;
        row "User Agent" t.user_agent;
        row "Language" t.language;
        row "Referer" t.referer;
        row "Method" t.method_;
        row "Encoding" t.encoding;
        row "MIME Type" t.mime;
        row "Charset" t.charset;
        row "Via" t.via;
        row "X-Forwarded-For" t.forwarded;
        row "Port" (string_of_int t.port);
        row "Connection" t.connection;
      ]
  in
  Printf.sprintf
    {|<!DOCTYPE html>
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
  table { width: 100%%; border-collapse: collapse; }
  th, td { text-align: left; padding: 0.4rem 0.6rem; border-bottom: 1px solid #8884; }
  th { width: 40%%; font-weight: 600; color: #666; }
  @media (prefers-color-scheme: dark) { th { color: #aaa; } }
  code { font-size: 0.9em; }
  .cli { margin-top: 2rem; }
  .cli pre { overflow-x: auto; padding: 0.75rem 1rem; background: #8881;
             border-radius: 6px; }
</style>
</head>
<body>
<h1>What Is My IP Address?</h1>
<p class="ip">%s</p>
<h2>Your Connection</h2>
<table>
%s</table>
<section class="cli">
<h2>Command Line</h2>
<pre><code>curl localhost/ip
curl localhost/ua
curl localhost/all
curl localhost/all.json</code></pre>
</section>
</body>
</html>
|}
    (html_escape ip) rows
