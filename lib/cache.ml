(** Incremental build cache.

    Provides on-disk caching for pipeline results and build artifacts.
    Cache entries are keyed by a string key and track the source file
    mtime for automatic invalidation.
*)

type t = {
  dir : string;
}

let create ~dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    Sys.mkdir dir 0o755;
  { dir }

let cache_file t key =
  let sanitized =
    String.map (fun c ->
      if c = '/' || c = '\\' || c = ':' then '_'
      else c
    ) key
  in
  Filename.concat t.dir (sanitized ^ ".cache")

let meta_file t key =
  let sanitized =
    String.map (fun c ->
      if c = '/' || c = '\\' || c = ':' then '_'
      else c
    ) key
  in
  Filename.concat t.dir (sanitized ^ ".meta")

let get t ~key ~mtime =
  let cf = cache_file t key in
  let mf = meta_file t key in
  if not (Sys.file_exists cf && Sys.file_exists mf) then
    None
  else
    try
      let ic = open_in mf in
      let stored_mtime_str = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let stored_mtime = float_of_string stored_mtime_str in
      if stored_mtime < mtime then
        None
      else
        let ic = open_in_bin cf in
        let n = in_channel_length ic in
        let data = really_input_string ic n in
        close_in ic;
        Some data
    with _ -> None

let set t ~key ~mtime value =
  try
    let cf = cache_file t key in
    let mf = meta_file t key in
    let oc = open_out_bin cf in
    output_string oc value;
    close_out oc;
    let oc = open_out mf in
    output_string oc (Printf.sprintf "%.6f" mtime);
    close_out oc
  with _ -> ()

let invalidate t ~key =
  try
    let cf = cache_file t key in
    let mf = meta_file t key in
    if Sys.file_exists cf then Sys.remove cf;
    if Sys.file_exists mf then Sys.remove mf
  with _ -> ()

let clear t =
  try
    let entries = Sys.readdir t.dir in
    Array.iter (fun entry ->
      let path = Filename.concat t.dir entry in
      if not (Sys.is_directory path) then
        Sys.remove path
    ) entries
  with _ -> ()

let is_valid t ~key ~mtime =
  match get t ~key ~mtime with
  | Some _ -> true
  | None -> false
