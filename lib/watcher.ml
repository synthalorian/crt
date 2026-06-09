(** File watcher for hot reload.

    Uses a simple polling-based approach to detect file changes.
    Works without extra OS-specific dependencies.
*)

module PathMap = Map.Make(String)

type state = {
  mutable last_check : float;
  mutable file_mtimes : float PathMap.t;
}

let create () = {
  last_check = 0.0;
  file_mtimes = PathMap.empty;
}

let get_mtime path =
  try
    let stats = Unix.stat path in
    Some stats.Unix.st_mtime
  with _ -> None

let rec collect_files acc dir =
  try
    let entries = Sys.readdir dir in
    Array.fold_left (fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then
        if entry <> "_build" && entry <> ".git" && entry <> "node_modules" then
          collect_files acc path
        else
          acc
      else
        path :: acc
    ) acc entries
  with _ -> acc

let scan_directory dir =
  collect_files [] dir

let check_changes state paths =
  let changed = ref [] in
  let new_mtimes = ref state.file_mtimes in
  List.iter (fun path ->
    match get_mtime path with
    | Some mtime ->
        (match PathMap.find_opt path state.file_mtimes with
        | Some old_mtime when old_mtime < mtime ->
            changed := path :: !changed;
            new_mtimes := PathMap.add path mtime !new_mtimes
        | None ->
            changed := path :: !changed;
            new_mtimes := PathMap.add path mtime !new_mtimes
        | _ ->
            new_mtimes := PathMap.add path mtime !new_mtimes)
    | None -> ()
  ) paths;
  state.file_mtimes <- !new_mtimes;
  !changed

let watch ~dir ~delay ~on_change ~should_stop =
  let state = create () in
  Printf.printf "[watch] Watching %s for changes...\n%!" dir;
  while not (should_stop ()) do
    let files = scan_directory dir in
    let changed = check_changes state files in
    if changed <> [] then (
      Printf.printf "[watch] Detected changes in %d file(s)\n%!" (List.length changed);
      List.iter (fun path -> Printf.printf "  - %s\n%!" path) changed;
      on_change changed
    );
    Unix.sleepf delay
  done;
  Printf.printf "[watch] Stopped watching.\n%!"
