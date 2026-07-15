(** Client connection information extracted from the peer address and
    request headers. *)

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

val make :
  ip:string ->
  port:int ->
  meth:string ->
  user_agent:string option ->
  language:string option ->
  referer:string option ->
  encoding:string option ->
  mime:string option ->
  charset:string option ->
  via:string option ->
  forwarded:string option ->
  connection:string option ->
  t

(** Prefer [X-Forwarded-For] (first hop) when present, else the peer IP. *)
val effective_ip : t -> string

val wants_html : mime:string option -> bool

val to_plain_all : t -> string
val to_json : t -> string
val to_html : t -> string
