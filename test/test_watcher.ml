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

(* -------------------------------------------------------------------------- *)
(* Watcher tests                                                              *)
(* -------------------------------------------------------------------------- *)

let test_watcher_create () =
  let _state = Watcher.create () in
  assert_true "watcher create" true

let test_watcher_scan_directory () =
  let test_dir = "_test_watcher_dir" in
  (try Sys.rmdir test_dir with _ -> ());
  Sys.mkdir test_dir 0o755;
  
  let oc = open_out (Filename.concat test_dir "a.md") in
  output_string oc "hello";
  close_out oc;
  
  let oc = open_out (Filename.concat test_dir "b.txt") in
  output_string oc "world";
  close_out oc;
  
  let files = Watcher.scan_directory test_dir in
  assert_true "scan finds files" (List.length files >= 2);
  assert_true "scan finds a.md" (List.exists (fun p -> Filename.basename p = "a.md") files);
  assert_true "scan finds b.txt" (List.exists (fun p -> Filename.basename p = "b.txt") files);
  
  (try Sys.remove (Filename.concat test_dir "a.md") with _ -> ());
  (try Sys.remove (Filename.concat test_dir "b.txt") with _ -> ());
  (try Sys.rmdir test_dir with _ -> ())

let test_watcher_detects_new_file () =
  let test_dir = "_test_watcher_detect" in
  (try Sys.rmdir test_dir with _ -> ());
  Sys.mkdir test_dir 0o755;
  
  let state = Watcher.create () in
  let files1 = Watcher.scan_directory test_dir in
  let changed1 = Watcher.check_changes state files1 in
  assert_true "initial check empty" (changed1 = []);
  
  (* Create a new file *)
  let oc = open_out (Filename.concat test_dir "new.md") in
  output_string oc "content";
  close_out oc;
  
  let files2 = Watcher.scan_directory test_dir in
  let changed2 = Watcher.check_changes state files2 in
  assert_true "detects new file" (List.exists (fun p -> Filename.basename p = "new.md") changed2);
  
  (try Sys.remove (Filename.concat test_dir "new.md") with _ -> ());
  (try Sys.rmdir test_dir with _ -> ())

let test_watcher_detects_modification () =
  let test_dir = "_test_watcher_modify" in
  (try Sys.rmdir test_dir with _ -> ());
  Sys.mkdir test_dir 0o755;
  
  let test_file = Filename.concat test_dir "existing.md" in
  let oc = open_out test_file in
  output_string oc "version1";
  close_out oc;
  
  let state = Watcher.create () in
  let files1 = Watcher.scan_directory test_dir in
  let _changed1 = Watcher.check_changes state files1 in
  
  (* Small delay to ensure mtime changes *)
  Unix.sleepf 0.1;
  
  (* Modify the file *)
  let oc = open_out test_file in
  output_string oc "version2";
  close_out oc;
  
  let files2 = Watcher.scan_directory test_dir in
  let changed2 = Watcher.check_changes state files2 in
  assert_true "detects modification" (List.exists (fun p -> Filename.basename p = "existing.md") changed2);
  
  (try Sys.remove test_file with _ -> ());
  (try Sys.rmdir test_dir with _ -> ())

let test_watcher_ignores_unchanged () =
  let test_dir = "_test_watcher_unchanged" in
  (try Sys.rmdir test_dir with _ -> ());
  Sys.mkdir test_dir 0o755;
  
  let test_file = Filename.concat test_dir "stable.md" in
  let oc = open_out test_file in
  output_string oc "stable content";
  close_out oc;
  
  let state = Watcher.create () in
  let files1 = Watcher.scan_directory test_dir in
  let _changed1 = Watcher.check_changes state files1 in
  
  (* Scan again without changing anything *)
  let files2 = Watcher.scan_directory test_dir in
  let changed2 = Watcher.check_changes state files2 in
  assert_true "no change on unchanged" (changed2 = []);
  
  (try Sys.remove test_file with _ -> ());
  (try Sys.rmdir test_dir with _ -> ())

let run () =
  Printf.printf "Running watcher tests...\n%!";
  test_watcher_create ();
  test_watcher_scan_directory ();
  test_watcher_detects_new_file ();
  test_watcher_detects_modification ();
  test_watcher_ignores_unchanged ();
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()
