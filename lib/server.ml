(** Minimal HTTP server with hot reload support.

    Serves static files from a directory and provides an SSE endpoint
    at [/__reload] for browser live-reload.
*)

module ClientSet = Set.Make(struct
  type t = Unix.file_descr
  let compare = Stdlib.compare
end)

type t = {
  mutable clients : ClientSet.t;
  mutable running : bool;
  port : int;
}

let create ~port () = {
  clients = ClientSet.empty;
  running = false;
  port;
}

(* -------------------------------------------------------------------------- *)
(* HTTP protocol helpers                                                      *)
(* -------------------------------------------------------------------------- *)

let read_request ic =
  let lines = ref [] in
  let rec read () =
    let line = input_line ic in
    if line = "\r" || line = "" then
      List.rev !lines
    else (
      lines := line :: !lines;
      read ()
    )
  in
  try read () with End_of_file -> List.rev !lines

let parse_path request_lines =
  match request_lines with
  | [] -> "/"
  | first :: _ ->
      match String.split_on_char ' ' first with
      | _ :: path :: _ -> path
      | _ -> "/"

let send_string oc status content_type body =
  Printf.fprintf oc "HTTP/1.1 %s\r\n" status;
  Printf.fprintf oc "Content-Type: %s\r\n" content_type;
  Printf.fprintf oc "Content-Length: %d\r\n" (String.length body);
  Printf.fprintf oc "Connection: close\r\n";
  Printf.fprintf oc "\r\n";
  output_string oc body;
  flush oc

let send_sse_headers oc =
  Printf.fprintf oc "HTTP/1.1 200 OK\r\n";
  Printf.fprintf oc "Content-Type: text/event-stream\r\n";
  Printf.fprintf oc "Cache-Control: no-cache\r\n";
  Printf.fprintf oc "Connection: keep-alive\r\n";
  Printf.fprintf oc "\r\n";
  Printf.fprintf oc "data: connected\n\n";
  flush oc

let send_sse_reload oc =
  try
    Printf.fprintf oc "data: reload\n\n";
    flush oc;
    true
  with _ -> false

(* -------------------------------------------------------------------------- *)
(* File serving                                                               *)
(* -------------------------------------------------------------------------- *)

let mime_type path =
  if Filename.check_suffix path ".html" then "text/html"
  else if Filename.check_suffix path ".css" then "text/css"
  else if Filename.check_suffix path ".js" then "application/javascript"
  else if Filename.check_suffix path ".json" then "application/json"
  else if Filename.check_suffix path ".png" then "image/png"
  else if Filename.check_suffix path ".jpg" || Filename.check_suffix path ".jpeg" then "image/jpeg"
  else if Filename.check_suffix path ".svg" then "image/svg+xml"
  else "text/plain"

let read_file_binary path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let normalize_path path =
  let parts = String.split_on_char '/' path in
  let rec aux acc = function
    | [] -> List.rev acc
    | "" :: rest | "." :: rest -> aux acc rest
    | ".." :: rest ->
        (match acc with
         | _ :: acc' -> aux acc' rest
         | [] -> aux acc rest)
    | part :: rest -> aux (part :: acc) rest
  in
  String.concat "/" (aux [] parts)

let serve_file oc doc_root path =
  let safe_path =
    let p = if String.length path > 0 && path.[0] = '/' then
      String.sub path 1 (String.length path - 1)
    else path in
    if p = "" then "index.html" else p
  in
  let file_path = Filename.concat doc_root safe_path in
  let normalized = normalize_path file_path in
  let root_normalized = normalize_path doc_root in
  (* Security: ensure file is within doc_root *)
  if not (String.starts_with ~prefix:root_normalized normalized) then
    send_string oc "403 Forbidden" "text/plain" "Forbidden"
  else if Sys.file_exists normalized && not (Sys.is_directory normalized) then
    let body = read_file_binary normalized in
    send_string oc "200 OK" (mime_type normalized) body
  else if Sys.file_exists (normalized ^ ".html") then
    let body = read_file_binary (normalized ^ ".html") in
    send_string oc "200 OK" "text/html" body
  else
    send_string oc "404 Not Found" "text/plain" "Not Found"

(* -------------------------------------------------------------------------- *)
(* SSE client management                                                      *)
(* -------------------------------------------------------------------------- *)

let add_client server fd =
  server.clients <- ClientSet.add fd server.clients

let remove_client server fd =
  server.clients <- ClientSet.remove fd server.clients;
  try Unix.close fd with _ -> ()

let broadcast_reload server =
  let dead = ref [] in
  ClientSet.iter (fun fd ->
    if not (send_sse_reload (Unix.out_channel_of_descr fd)) then
      dead := fd :: !dead
  ) server.clients;
  List.iter (remove_client server) !dead

(* -------------------------------------------------------------------------- *)
(* Main server loop                                                           *)
(* -------------------------------------------------------------------------- *)

let handle_client server doc_root client_fd =
  let ic = Unix.in_channel_of_descr client_fd in
  let request = read_request ic in
  let path = parse_path request in
  
  if path = "/__reload" then (
    let oc = Unix.out_channel_of_descr client_fd in
    send_sse_headers oc;
    add_client server client_fd;
    (* Client stays connected for SSE; don't close here *)
    true
  ) else (
    let oc = Unix.out_channel_of_descr client_fd in
    serve_file oc doc_root path;
    Unix.close client_fd;
    false
  )

let rec accept_loop server doc_root listen_fd =
  if not server.running then ()
  else
    let readable, _, _ = Unix.select [listen_fd] [] [] 0.1 in
    if List.mem listen_fd readable then
      try
        let client_fd, _ = Unix.accept listen_fd in
        let is_sse = handle_client server doc_root client_fd in
        if not is_sse then
          () (* Regular request already closed *)
        else
          () (* SSE client stays open, managed in ClientSet *)
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      | exn ->
          Printf.eprintf "[server] Client error: %s\n%!" (Printexc.to_string exn);
          ()
    else
      ();
    accept_loop server doc_root listen_fd

let run server ~doc_root () =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, server.port) in
  let listen_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listen_fd Unix.SO_REUSEADDR true;
  Unix.bind listen_fd addr;
  Unix.listen listen_fd 10;
  server.running <- true;
  Printf.printf "[server] Serving %s at http://localhost:%d\n%!" doc_root server.port;
  
  try
    accept_loop server doc_root listen_fd
  with exn ->
    Printf.eprintf "[server] Error: %s\n%!" (Printexc.to_string exn);
    server.running <- false;
    Unix.close listen_fd

let stop server =
  server.running <- false;
  ClientSet.iter (fun fd -> try Unix.close fd with _ -> ()) server.clients;
  server.clients <- ClientSet.empty

let is_running server = server.running
