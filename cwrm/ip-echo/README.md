# ip-echo

A small OxCaml HTTP server that reports the client's IP address and connection
metadata, similar to [ifconfig.me](https://ifconfig.me). Built with Eio and
[httpz](../../avsm/httpz).

## Build

From the oxmono repository root:

```bash
dune build cwrm/ip-echo
```

## Run

```bash
dune exec -- ip-echo --port 8080
# or, after install:
ip-echo -p 8080 -v
```

By default the server listens on `0.0.0.0:8080`. Use `--bind 127.0.0.1` for
loopback only.

## Endpoints

| Path | Response |
|------|----------|
| `/` | Client IP (plain text), or an HTML summary if `Accept` includes `text/html` |
| `/ip` | Client IP |
| `/ua` | `User-Agent` |
| `/lang` | `Accept-Language` |
| `/encoding` | `Accept-Encoding` |
| `/mime` | `Accept` |
| `/charset` | `Accept-Charset` |
| `/forwarded` | `X-Forwarded-For` |
| `/via` | `Via` |
| `/port` | Client TCP port |
| `/all` | All fields as `key: value` lines |
| `/all.json` | All fields as JSON |

When `X-Forwarded-For` is present, `/` and `/ip` use the first forwarded hop
(typical reverse-proxy setup); otherwise they use the TCP peer address.

## Test

```bash
dune runtest cwrm/ip-echo
```

Expect-tests cover connection-info formatting (`effective_ip`, Accept
detection, JSON/HTML escaping, path dispatch) and a small in-process HTTP
integration against the real Eio/httpz stack.

```bash
curl localhost:8080/ip
curl -H 'Accept: text/html' localhost:8080/
curl localhost:8080/all.json
```

This will of course not give you your public Ip, for which you would have to ping from a different server to receive, but it should demonstrate that the app works.
