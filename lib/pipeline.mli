(** DAG pipeline engine for functional composition of transforms.

    A pipeline is a directed acyclic graph (DAG) of pure functions.
    Nodes can be composed sequentially with [bind] or in parallel
    with [pair], enabling complex dependency graphs.
*)

type 'a t
(** A pipeline that produces a value of type ['a]. *)

(** {1 Constructors} *)

val pure : 'a -> 'a t
(** [pure v] creates a pipeline that always produces [v]. *)

val return : 'a -> 'a t
(** Alias for [pure]. *)

(** {1 Transforms} *)

val arr : ?name:string -> ('a -> 'b) -> 'a t -> 'b t
(** [arr ~name f p] applies pure function [f] to the output of [p]. *)

val map : ?name:string -> ('a -> 'b) -> 'a t -> 'b t
(** Alias for [arr]. *)

val node : string -> ('a -> 'b) -> 'a t -> 'b t
(** [node name f p] is [arr ~name f p]. Useful for naming nodes. *)

val bind : ?name:string -> 'a t -> ('a -> 'b t) -> 'b t
(** [bind ~name p f] sequences [p] into pipeline-producing function [f]. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix alias for [bind]. *)

val (>>|) : 'a t -> ('a -> 'b) -> 'b t
(** Infix operator for [map]. *)

(** {1 Parallel composition} *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair p1 p2] runs [p1] and [p2] independently and pairs results. *)

val both : 'a t -> 'b t -> ('a * 'b) t
(** Alias for [pair]. *)

(** {1 Utilities} *)

val cached : string -> 'a t -> 'a t
(** [cached name p] wraps pipeline [p] with a named cache layer.
    Results are stored in a cache and reused on subsequent runs
    if the cache entry is still valid. *)

val trace : ('a -> unit) -> 'a t -> 'a t
(** [trace f p] applies [f] to the output of [p] for side effects
    (logging, debugging), then returns the value unchanged. *)

val run : 'a t -> ('a, exn) result
(** [run p] executes pipeline [p] and returns its result,
    catching any exceptions. *)

val run_cached : cache:Cache.t -> 'a t -> ('a, exn) result
(** [run_cached ~cache p] executes pipeline [p] with caching.
    Intermediate results for named nodes and [cached] wrappers
    are stored in [cache] and reused when available. *)

val to_string : 'a t -> string
(** [to_string p] returns a string representation of the pipeline structure. *)
