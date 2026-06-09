(** Minimal HTTP server with hot reload support. *)

type t

val create : port:int -> unit -> t
(** [create ~port ()] creates a new server listening on [port]. *)

val run : t -> doc_root:string -> unit -> unit
(** [run server ~doc_root ()] starts the server blocking loop.
    Serves static files from [doc_root]. Provides SSE endpoint at [/__reload]. *)

val stop : t -> unit
(** [stop server] stops the server and closes all connections. *)

val is_running : t -> bool
(** [is_running server] returns whether the server is currently running. *)

val broadcast_reload : t -> unit
(** [broadcast_reload server] sends a reload event to all connected SSE clients. *)
