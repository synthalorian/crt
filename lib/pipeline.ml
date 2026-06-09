(** DAG pipeline engine implementation.

    Uses a GADT to type the pipeline AST, supporting:
    - Pure values
    - Function application (map/arr)
    - Monadic bind for sequencing
    - Pair for parallel composition (fork/join)
*)

type 'a t =
  | Pure : 'a -> 'a t
  | Map : string option * ('a -> 'b) * 'a t -> 'b t
  | Bind : string option * 'a t * ('a -> 'b t) -> 'b t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Cached : string * 'a t -> 'a t

let pure x = Pure x
let return = pure

let arr ?name f p = Map (name, f, p)
let map = arr
let node name f p = arr ~name f p

let bind ?name p f = Bind (name, p, f)
let (>>=) p f = bind p f
let (>>|) p f = map f p

let pair p1 p2 = Pair (p1, p2)
let both = pair

let cached name p = Cached (name, p)

let trace f p = arr (fun x -> f x; x) p

let rec exec : type a. a t -> a = function
  | Pure x -> x
  | Map (_, f, p) -> f (exec p)
  | Bind (_, p, f) -> exec (f (exec p))
  | Pair (p1, p2) -> (exec p1, exec p2)
  | Cached (_, p) -> exec p

let run p =
  try Ok (exec p)
  with exn -> Error exn

let rec run_cached : type a. cache:Cache.t -> a t -> (a, exn) result =
  fun ~cache p ->
    try
      match p with
      | Pure x -> Ok x
      | Map (name, f, p') ->
          (match run_cached ~cache p' with
           | Ok x ->
               let key_base = match name with Some n -> n | None -> "map" in
               let key = key_base ^ ":" ^ Digest.string (Marshal.to_string x []) in
               (match Cache.get cache ~key ~mtime:0.0 with
                | Some data -> Ok (Marshal.from_string data 0)
                | None ->
                    let result = f x in
                    Cache.set cache ~key ~mtime:0.0 (Marshal.to_string result []);
                    Ok result)
           | Error e -> Error e)
      | Bind (_name, p', f) ->
          (match run_cached ~cache p' with
           | Ok x -> run_cached ~cache (f x)
           | Error e -> Error e)
      | Pair (p1, p2) ->
          (match run_cached ~cache p1, run_cached ~cache p2 with
           | Ok x1, Ok x2 -> Ok (x1, x2)
           | Error e, _ | _, Error e -> Error e)
      | Cached (name, p') ->
          let key = "cached:" ^ name in
          (match Cache.get cache ~key ~mtime:0.0 with
           | Some data -> Ok (Marshal.from_string data 0)
           | None ->
               (match run_cached ~cache p' with
                | Ok result ->
                    Cache.set cache ~key ~mtime:0.0 (Marshal.to_string result []);
                    Ok result
                | Error e -> Error e))
    with exn -> Error exn

let rec to_string : type a. a t -> string = function
  | Pure _ -> "Pure"
  | Map (Some name, _, p) -> Printf.sprintf "Map(%s, %s)" name (to_string p)
  | Map (None, _, p) -> Printf.sprintf "Map(_, %s)" (to_string p)
  | Bind (Some name, p, _) -> Printf.sprintf "Bind(%s, %s, _)" name (to_string p)
  | Bind (None, p, _) -> Printf.sprintf "Bind(_, %s, _)" (to_string p)
  | Pair (p1, p2) -> Printf.sprintf "Pair(%s, %s)" (to_string p1) (to_string p2)
  | Cached (name, p) -> Printf.sprintf "Cached(%s, %s)" name (to_string p)
