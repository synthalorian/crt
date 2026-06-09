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
(* Plugin creation and registry tests                                         *)
(* -------------------------------------------------------------------------- *)

let test_plugin_create () =
  let p = Plugin.create ~name:"test-plugin" ~version:"1.0.0" () in
  assert_eq "plugin name" "test-plugin" p.name;
  assert_eq "plugin version" "1.0.0" p.version;
  assert_true "plugin empty transforms" (p.transforms = []);
  assert_true "plugin empty doc_hooks" (p.doc_hooks = []);
  assert_true "plugin empty html_hooks" (p.html_hooks = [])

let test_plugin_create_with_transforms () =
  let transforms = [("capitalize", fun doc -> doc)] in
  let p = Plugin.create ~name:"transformer" ~version:"0.1.0" ~transforms () in
  assert_true "plugin with transforms" (List.length p.transforms = 1);
  assert_eq "transform name" "capitalize" (fst (List.hd p.transforms))

let test_plugin_create_with_hooks () =
  let doc_hooks = [(Plugin.Post_parse, fun doc -> doc)] in
  let html_hooks = [(fun html -> html ^ "<!-- hooked -->")] in
  let p = Plugin.create ~name:"hooker" ~version:"0.2.0" ~doc_hooks ~html_hooks () in
  assert_true "plugin with doc_hooks" (List.length p.doc_hooks = 1);
  assert_true "plugin with html_hooks" (List.length p.html_hooks = 1)

(* -------------------------------------------------------------------------- *)
(* Registry tests                                                             *)
(* -------------------------------------------------------------------------- *)

let test_register_and_registry () =
  Plugin.clear_registry ();
  let p = Plugin.create ~name:"reg-test" ~version:"1.0.0" () in
  Plugin.register p;
  let registry = Plugin.registry () in
  assert_true "registry has one plugin" (List.length registry = 1);
  assert_eq "registry plugin name" "reg-test" (List.hd registry).name

let test_clear_registry () =
  Plugin.clear_registry ();
  let p = Plugin.create ~name:"to-clear" ~version:"1.0.0" () in
  Plugin.register p;
  assert_true "before clear has plugin" (List.length (Plugin.registry ()) = 1);
  Plugin.clear_registry ();
  assert_true "after clear empty" (Plugin.registry () = [])

let test_plugin_names () =
  Plugin.clear_registry ();
  Plugin.register (Plugin.create ~name:"alpha" ~version:"1.0.0" ());
  Plugin.register (Plugin.create ~name:"beta" ~version:"2.0.0" ());
  let names = Plugin.plugin_names () in
  assert_true "plugin names count" (List.length names = 2);
  assert_true "plugin names contains alpha" (List.mem "alpha" names);
  assert_true "plugin names contains beta" (List.mem "beta" names)

let test_transform_names () =
  Plugin.clear_registry ();
  let transforms = [("t1", fun d -> d); ("t2", fun d -> d)] in
  Plugin.register (Plugin.create ~name:"t-plugin" ~version:"1.0.0" ~transforms ());
  let names = Plugin.transform_names () in
  assert_true "transform names count" (List.length names = 2);
  assert_true "transform names contains t1" (List.mem "t1" names);
  assert_true "transform names contains t2" (List.mem "t2" names)

(* -------------------------------------------------------------------------- *)
(* Doc hook application tests                                                 *)
(* -------------------------------------------------------------------------- *)

let test_apply_doc_hooks_post_parse () =
  Plugin.clear_registry ();
  let hook_fn doc =
    match doc with
    | Ast.Paragraph [Ast.Text "hello"] :: rest ->
        Ast.Paragraph [Ast.Text "HELLO"] :: rest
    | _ -> doc
  in
  let doc_hooks = [(Plugin.Post_parse, hook_fn)] in
  Plugin.register (Plugin.create ~name:"upper" ~version:"1.0.0" ~doc_hooks ());
  let doc = [Ast.Paragraph [Ast.Text "hello"]] in
  let result = Plugin.apply_doc_hooks ~stage:Post_parse doc in
  match result with
  | [Ast.Paragraph [Ast.Text "HELLO"]] -> assert_true "post_parse hook applied" true
  | _ -> assert_true "post_parse hook applied" false

let test_apply_doc_hooks_pre_render () =
  Plugin.clear_registry ();
  let hook_fn doc =
    Ast.Heading { level = 1; content = [Ast.Text "Injected"] } :: doc
  in
  let doc_hooks = [(Plugin.Pre_render, hook_fn)] in
  Plugin.register (Plugin.create ~name:"injector" ~version:"1.0.0" ~doc_hooks ());
  let doc = [Ast.Paragraph [Ast.Text "content"]] in
  let result = Plugin.apply_doc_hooks ~stage:Pre_render doc in
  match result with
  | Ast.Heading { level = 1; content = [Ast.Text "Injected"] } :: _ ->
      assert_true "pre_render hook applied" true
  | _ -> assert_true "pre_render hook applied" false

let test_apply_doc_hooks_stage_filtering () =
  Plugin.clear_registry ();
  let post_hook doc =
    match doc with
    | Ast.Paragraph [Ast.Text t] :: rest -> Ast.Paragraph [Ast.Text (t ^ "-post")] :: rest
    | _ -> doc
  in
  let pre_hook doc =
    match doc with
    | Ast.Paragraph [Ast.Text t] :: rest -> Ast.Paragraph [Ast.Text (t ^ "-pre")] :: rest
    | _ -> doc
  in
  let doc_hooks = [(Plugin.Post_parse, post_hook); (Plugin.Pre_render, pre_hook)] in
  Plugin.register (Plugin.create ~name:"staged" ~version:"1.0.0" ~doc_hooks ());
  let doc = [Ast.Paragraph [Ast.Text "base"]] in
  let post_result = Plugin.apply_doc_hooks ~stage:Post_parse doc in
  let pre_result = Plugin.apply_doc_hooks ~stage:Pre_render doc in
  (match post_result with
   | [Ast.Paragraph [Ast.Text "base-post"]] -> assert_true "post_parse stage filter" true
   | _ -> assert_true "post_parse stage filter" false);
  (match pre_result with
   | [Ast.Paragraph [Ast.Text "base-pre"]] -> assert_true "pre_render stage filter" true
   | _ -> assert_true "pre_render stage filter" false)

let test_apply_doc_hooks_no_plugins () =
  Plugin.clear_registry ();
  let doc = [Ast.Paragraph [Ast.Text "unchanged"]] in
  let result = Plugin.apply_doc_hooks ~stage:Post_parse doc in
  assert_true "no plugins leaves doc unchanged" (result = doc)

(* -------------------------------------------------------------------------- *)
(* Transform application tests                                                *)
(* -------------------------------------------------------------------------- *)

let test_apply_transforms () =
  Plugin.clear_registry ();
  let transform1 doc =
    match doc with
    | Ast.Paragraph [Ast.Text t] :: rest -> Ast.Paragraph [Ast.Text (t ^ "-t1")] :: rest
    | _ -> doc
  in
  let transform2 doc =
    match doc with
    | Ast.Paragraph [Ast.Text t] :: rest -> Ast.Paragraph [Ast.Text (t ^ "-t2")] :: rest
    | _ -> doc
  in
  let transforms = [("t1", transform1); ("t2", transform2)] in
  Plugin.register (Plugin.create ~name:"transformer" ~version:"1.0.0" ~transforms ());
  let doc = [Ast.Paragraph [Ast.Text "base"]] in
  let result = Plugin.apply_transforms doc in
  match result with
  | [Ast.Paragraph [Ast.Text "base-t1-t2"]] -> assert_true "transforms applied in order" true
  | _ -> assert_true "transforms applied in order" false

let test_apply_transforms_no_plugins () =
  Plugin.clear_registry ();
  let doc = [Ast.Paragraph [Ast.Text "unchanged"]] in
  let result = Plugin.apply_transforms doc in
  assert_true "no transform plugins leaves doc unchanged" (result = doc)

(* -------------------------------------------------------------------------- *)
(* HTML hook application tests                                                *)
(* -------------------------------------------------------------------------- *)

let test_apply_html_hooks () =
  Plugin.clear_registry ();
  let html_hooks = [
    (fun html -> html ^ "<!-- hook1 -->");
    (fun html -> html ^ "<!-- hook2 -->")
  ] in
  Plugin.register (Plugin.create ~name:"html-hooker" ~version:"1.0.0" ~html_hooks ());
  let html = "<html>" in
  let result = Plugin.apply_html_hooks html in
  assert_eq "html hooks applied" "<html><!-- hook1 --><!-- hook2 -->" result

let test_apply_html_hooks_no_plugins () =
  Plugin.clear_registry ();
  let html = "<html></html>" in
  let result = Plugin.apply_html_hooks html in
  assert_eq "no html plugins leaves html unchanged" html result

(* -------------------------------------------------------------------------- *)
(* Multiple plugins interaction tests                                         *)
(* -------------------------------------------------------------------------- *)

let test_multiple_plugins () =
  Plugin.clear_registry ();
  let p1_doc_hooks = [(Plugin.Post_parse, fun doc ->
    match doc with
    | Ast.Paragraph [Ast.Text t] :: rest -> Ast.Paragraph [Ast.Text (t ^ "-p1")] :: rest
    | _ -> doc
  )] in
  let p2_doc_hooks = [(Plugin.Post_parse, fun doc ->
    match doc with
    | Ast.Paragraph [Ast.Text t] :: rest -> Ast.Paragraph [Ast.Text (t ^ "-p2")] :: rest
    | _ -> doc
  )] in
  Plugin.register (Plugin.create ~name:"p1" ~version:"1.0.0" ~doc_hooks:p1_doc_hooks ());
  Plugin.register (Plugin.create ~name:"p2" ~version:"1.0.0" ~doc_hooks:p2_doc_hooks ());
  let doc = [Ast.Paragraph [Ast.Text "base"]] in
  let result = Plugin.apply_doc_hooks ~stage:Post_parse doc in
  (* Plugins are applied in registration order: p1 first, then p2 *)
  match result with
  | [Ast.Paragraph [Ast.Text "base-p1-p2"]] -> assert_true "multiple plugins applied in order" true
  | Ast.Paragraph [Ast.Text t] :: _ ->
      assert_eq "multiple plugins applied in order (actual)" "base-p1-p2" t
  | _ -> assert_true "multiple plugins applied in order" false

(* -------------------------------------------------------------------------- *)
(* Dynamic loading error handling tests                                       *)
(* -------------------------------------------------------------------------- *)

let test_load_cmxs_missing_file () =
  match Plugin.load_cmxs "/nonexistent/plugin.cmxs" with
  | Ok () -> assert_true "load missing cmxs returns error" false
  | Error _ -> assert_true "load missing cmxs returns error" true

let test_load_dir_missing_dir () =
  match Plugin.load_dir "/nonexistent/plugins" with
  | Ok _ -> assert_true "load missing dir returns error" false
  | Error _ -> assert_true "load missing dir returns error" true

let test_load_dir_empty_dir () =
  let test_dir = "_test_plugin_empty_dir" in
  (try
    if Sys.file_exists test_dir then (
      let entries = Sys.readdir test_dir in
      Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
      Sys.rmdir test_dir
    )
  with _ -> ());
  Sys.mkdir test_dir 0o755;
  (match Plugin.load_dir test_dir with
   | Ok 0 -> assert_true "load empty dir returns 0" true
   | Ok n -> assert_true ("load empty dir returns 0 (got " ^ string_of_int n ^ ")") false
   | Error _ -> assert_true "load empty dir returns 0" false);
  (try
    let entries = Sys.readdir test_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat test_dir e)) entries;
    Sys.rmdir test_dir
  with _ -> ())

(* -------------------------------------------------------------------------- *)
(* Integration test: full pipeline with plugins                               *)
(* -------------------------------------------------------------------------- *)

let test_integration_pipeline () =
  Plugin.clear_registry ();
  (* Plugin that adds a horizontal rule after the first paragraph *)
  let doc_hooks = [(Plugin.Post_parse, fun doc ->
    match doc with
    | p :: rest -> p :: Ast.ThematicBreak :: rest
    | _ -> doc
  )] in
  let html_hooks = [(fun html ->
    html ^ "<!-- integration -->" (* dummy transform, just check it runs *)
  )] in
  Plugin.register (Plugin.create ~name:"integration" ~version:"1.0.0" ~doc_hooks ~html_hooks ());
  let doc = [Ast.Paragraph [Ast.Text "hello"]] in
  let after_post = Plugin.apply_doc_hooks ~stage:Post_parse doc in
  assert_true "integration post_parse adds hr" (List.length after_post = 2);
  let html = "<p>hello</p>" in
  let after_html = Plugin.apply_html_hooks html in
  assert_true "integration html hook runs" (after_html <> html)

let run () =
  Printf.printf "Running plugin tests...\n%!";
  test_plugin_create ();
  test_plugin_create_with_transforms ();
  test_plugin_create_with_hooks ();
  test_register_and_registry ();
  test_clear_registry ();
  test_plugin_names ();
  test_transform_names ();
  test_apply_doc_hooks_post_parse ();
  test_apply_doc_hooks_pre_render ();
  test_apply_doc_hooks_stage_filtering ();
  test_apply_doc_hooks_no_plugins ();
  test_apply_transforms ();
  test_apply_transforms_no_plugins ();
  test_apply_html_hooks ();
  test_apply_html_hooks_no_plugins ();
  test_multiple_plugins ();
  test_load_cmxs_missing_file ();
  test_load_dir_missing_dir ();
  test_load_dir_empty_dir ();
  test_integration_pipeline ();
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()
