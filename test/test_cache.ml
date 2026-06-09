open Crt_lib

let pass_count = ref 0
let fail_count = ref 0

let assert_true name cond =
  if cond then (
    incr pass_count;
    Printf.printf "  PASS: %s\n%!" name)
  else (
    incr fail_count;
    Printf.printf "  FAIL: %s\n%!" name)

let assert_eq name expected actual =
  if expected = actual then (
    incr pass_count;
    Printf.printf "  PASS: %s\n%!" name)
  else (
    incr fail_count;
    Printf.printf "  FAIL: %s\n%!" name;
    Printf.printf "    expected: %s\n%!" (String.escaped expected);
    Printf.printf "    actual:   %s\n%!" (String.escaped actual))

(* -------------------------------------------------------------------------- *)
(* Cache module tests                                                         *)
(* -------------------------------------------------------------------------- *)

let test_cache_create () =
  let test_dir = "_test_cache_dir" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let _cache = Cache.create ~dir:test_dir in
  assert_true "cache create makes directory" (Sys.file_exists test_dir && Sys.is_directory test_dir);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_set_and_get () =
  let test_dir = "_test_cache_getset" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  Cache.set cache ~key:"test_key" ~mtime:1.0 "hello world";
  (match Cache.get cache ~key:"test_key" ~mtime:1.0 with
   | Some v -> assert_eq "cache get after set" "hello world" v
   | None -> assert_true "cache get after set" false);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_invalidates_on_newer_mtime () =
  let test_dir = "_test_cache_invalidate" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  Cache.set cache ~key:"test_key" ~mtime:1.0 "old data";
  (match Cache.get cache ~key:"test_key" ~mtime:2.0 with
   | Some _ -> assert_true "cache invalidates on newer mtime" false
   | None -> assert_true "cache invalidates on newer mtime" true);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_valid_on_same_mtime () =
  let test_dir = "_test_cache_same" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  Cache.set cache ~key:"test_key" ~mtime:5.0 "same data";
  (match Cache.get cache ~key:"test_key" ~mtime:5.0 with
   | Some v -> assert_eq "cache valid on same mtime" "same data" v
   | None -> assert_true "cache valid on same mtime" false);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_valid_on_older_mtime () =
  let test_dir = "_test_cache_older" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  Cache.set cache ~key:"test_key" ~mtime:5.0 "data";
  (match Cache.get cache ~key:"test_key" ~mtime:4.0 with
   | Some v -> assert_eq "cache valid on older mtime" "data" v
   | None -> assert_true "cache valid on older mtime" false);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_missing_key () =
  let test_dir = "_test_cache_missing" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  (match Cache.get cache ~key:"nonexistent" ~mtime:1.0 with
   | Some _ -> assert_true "cache missing key returns none" false
   | None -> assert_true "cache missing key returns none" true);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_invalidate () =
  let test_dir = "_test_cache_inv" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  Cache.set cache ~key:"test_key" ~mtime:1.0 "data";
  Cache.invalidate cache ~key:"test_key";
  (match Cache.get cache ~key:"test_key" ~mtime:1.0 with
   | Some _ -> assert_true "cache invalidate removes entry" false
   | None -> assert_true "cache invalidate removes entry" true);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_clear () =
  let test_dir = "_test_cache_clear" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  Cache.set cache ~key:"key1" ~mtime:1.0 "data1";
  Cache.set cache ~key:"key2" ~mtime:1.0 "data2";
  Cache.clear cache;
  (match Cache.get cache ~key:"key1" ~mtime:1.0 with
   | Some _ -> assert_true "cache clear removes all" false
   | None -> assert_true "cache clear removes all" true);
  (match Cache.get cache ~key:"key2" ~mtime:1.0 with
   | Some _ -> assert_true "cache clear removes all 2" false
   | None -> assert_true "cache clear removes all 2" true);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_cache_is_valid () =
  let test_dir = "_test_cache_valid" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  Cache.set cache ~key:"test_key" ~mtime:1.0 "data";
  assert_true "cache is_valid true" (Cache.is_valid cache ~key:"test_key" ~mtime:1.0);
  assert_true "cache is_valid false" (not (Cache.is_valid cache ~key:"test_key" ~mtime:2.0));
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

(* -------------------------------------------------------------------------- *)
(* Pipeline caching tests                                                     *)
(* -------------------------------------------------------------------------- *)

let test_pipeline_cached_node () =
  let test_dir = "_test_pipeline_cache" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  let call_count = ref 0 in
  let p = Pipeline.pure 10
    |> Pipeline.arr ~name:"double" (fun x -> incr call_count; x * 2)
    |> Pipeline.cached "result"
  in
  (* First run - should compute *)
  (match Pipeline.run_cached ~cache p with
   | Ok 20 -> assert_true "cached first run correct" true
   | _ -> assert_true "cached first run correct" false);
  assert_true "cached first run call count" (!call_count = 1);
  (* Second run - should use cache *)
  (match Pipeline.run_cached ~cache p with
   | Ok 20 -> assert_true "cached second run correct" true
   | _ -> assert_true "cached second run correct" false);
  assert_true "cached second run call count" (!call_count = 1);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_pipeline_cached_with_map () =
  let test_dir = "_test_pipeline_map_cache" in
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ());
  let cache = Cache.create ~dir:test_dir in
  let call_count = ref 0 in
  let p = Pipeline.pure 5
    |> Pipeline.arr ~name:"inc" (fun x -> incr call_count; x + 1)
  in
  (* First run *)
  (match Pipeline.run_cached ~cache p with
   | Ok 6 -> assert_true "map cached first run" true
   | _ -> assert_true "map cached first run" false);
  assert_true "map cached first call count" (!call_count = 1);
  (* Second run - should use cache *)
  (match Pipeline.run_cached ~cache p with
   | Ok 6 -> assert_true "map cached second run" true
   | _ -> assert_true "map cached second run" false);
  assert_true "map cached second call count" (!call_count = 1);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

let test_pipeline_to_string_cached () =
  let s = Pipeline.to_string (Pipeline.pure 42 |> Pipeline.cached "answer") in
  assert_eq "to_string cached" "Cached(answer, Pure)" s

let run () =
  Printf.printf "Running cache tests...\n%!";
  test_cache_create ();
  test_cache_set_and_get ();
  test_cache_invalidates_on_newer_mtime ();
  test_cache_valid_on_same_mtime ();
  test_cache_valid_on_older_mtime ();
  test_cache_missing_key ();
  test_cache_invalidate ();
  test_cache_clear ();
  test_cache_is_valid ();
  test_pipeline_cached_node ();
  test_pipeline_cached_with_map ();
  test_pipeline_to_string_cached ();
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()