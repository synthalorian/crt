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

let assert_contains name substr s =
  let sub_len = String.length substr in
  let s_len = String.length s in
  let found =
    if sub_len = 0 then true
    else if sub_len > s_len then false
    else
      let rec check i =
        if i > s_len - sub_len then false
        else if String.sub s i sub_len = substr then true
        else check (i + 1)
      in
      check 0
  in
  if found then (
    incr pass_count;
    Printf.printf "  PASS: %s\n%!" name)
  else (
    incr fail_count;
    Printf.printf "  FAIL: %s\n%!" name;
    Printf.printf "    expected to contain: %s\n%!" (String.escaped substr);
    Printf.printf "    actual:   %s\n%!" (String.escaped s))

(* -------------------------------------------------------------------------- *)
(* Template parsing tests                                                     *)
(* -------------------------------------------------------------------------- *)

let test_template_text_only () =
  let tmpl = Template.of_string "hello world" in
  assert_true "text only" (tmpl = [Template.Text "hello world"])

let test_template_variable () =
  let tmpl = Template.of_string "hello {{ name }}" in
  assert_true "variable" (tmpl = [Template.Text "hello "; Template.Var "name"])

let test_template_multiple_vars () =
  let tmpl = Template.of_string "{{ a }} and {{ b }}" in
  assert_true "multiple vars"
    (tmpl = [Template.Var "a"; Template.Text " and "; Template.Var "b"])

let test_template_block () =
  let tmpl = Template.of_string "{% block content %}hello{% endblock %}" in
  match tmpl with
  | [Template.Block ("content", [Template.Text "hello"])] ->
      incr pass_count;
      Printf.printf "  PASS: block parsing\n%!"
  | _ ->
      incr fail_count;
      Printf.printf "  FAIL: block parsing\n%!";
      Printf.printf "    got: %s\n%!" (String.escaped (Template.render (Template.create_context ()) tmpl))

let test_template_extends () =
  let tmpl = Template.of_string "{% extends \"base.html\" %}" in
  assert_true "extends" (tmpl = [Template.Extends "base.html"])

let test_template_include () =
  let tmpl = Template.of_string "{% include \"header.html\" %}" in
  assert_true "include" (tmpl = [Template.Include "header.html"])

let test_template_if () =
  let tmpl = Template.of_string "{% if show %}yes{% endif %}" in
  match tmpl with
  | [Template.If ("show", [Template.Text "yes"], [])] ->
      incr pass_count;
      Printf.printf "  PASS: if parsing\n%!"
  | _ ->
      incr fail_count;
      Printf.printf "  FAIL: if parsing\n%!"

let test_template_if_else () =
  let tmpl = Template.of_string "{% if show %}yes{% else %}no{% endif %}" in
  match tmpl with
  | [Template.If ("show", [Template.Text "yes"], [Template.Text "no"])] ->
      incr pass_count;
      Printf.printf "  PASS: if-else parsing\n%!"
  | _ ->
      incr fail_count;
      Printf.printf "  FAIL: if-else parsing\n%!"

let test_template_for () =
  let tmpl = Template.of_string "{% for item in items %}{{ item }}{% endfor %}" in
  match tmpl with
  | [Template.For ("item", "items", [Template.Var "item"])] ->
      incr pass_count;
      Printf.printf "  PASS: for parsing\n%!"
  | _ ->
      incr fail_count;
      Printf.printf "  FAIL: for parsing\n%!"

(* -------------------------------------------------------------------------- *)
(* Template rendering tests                                                   *)
(* -------------------------------------------------------------------------- *)

let test_render_text () =
  let ctx = Template.create_context () in
  let result = Template.render_string ctx "hello world" in
  assert_eq "render text" "hello world" result

let test_render_variable () =
  let ctx = Template.create_context () in
  Template.set_var ctx "name" "Alice";
  let result = Template.render_string ctx "hello {{ name }}" in
  assert_eq "render variable" "hello Alice" result

let test_render_undefined_variable () =
  let ctx = Template.create_context () in
  let result = Template.render_string ctx "hello {{ missing }}" in
  assert_eq "render undefined var" "hello " result

let test_render_block () =
  let ctx = Template.create_context () in
  let result = Template.render_string ctx "{% block content %}hello{% endblock %}" in
  assert_eq "render block" "hello" result

let test_render_if_true () =
  let ctx = Template.create_context () in
  Template.set_var ctx "show" "true";
  let result = Template.render_string ctx "{% if show %}yes{% else %}no{% endif %}" in
  assert_eq "render if true" "yes" result

let test_render_if_false () =
  let ctx = Template.create_context () in
  let result = Template.render_string ctx "{% if show %}yes{% else %}no{% endif %}" in
  assert_eq "render if false" "no" result

let test_render_for_loop () =
  let ctx = Template.create_context () in
  Template.set_list ctx "items" ["a"; "b"; "c"];
  let result = Template.render_string ctx "{% for item in items %}{{ item }}{% endfor %}" in
  assert_eq "render for loop" "abc" result

(* -------------------------------------------------------------------------- *)
(* Template inheritance tests                                                 *)
(* -------------------------------------------------------------------------- *)

let test_template_inheritance () =
  let base = Template.of_string
    "<html>{% block content %}base{% endblock %}</html>" in
  let child = Template.of_string
    "{% extends \"base.html\" %}{% block content %}child{% endblock %}" in
  let loader = function
    | "base.html" -> base
    | _ -> []
  in
  let ctx = Template.create_context () in
  let result = Template.render ~loader ctx child in
  assert_eq "template inheritance" "<html>child</html>" result

let test_template_inheritance_preserves_base () =
  let base = Template.of_string
    "<html><head>{% block head %}base-head{% endblock %}</head><body>{% block body %}base-body{% endblock %}</body></html>" in
  let child = Template.of_string
    "{% extends \"base.html\" %}{% block body %}child-body{% endblock %}" in
  let loader = function
    | "base.html" -> base
    | _ -> []
  in
  let ctx = Template.create_context () in
  let result = Template.render ~loader ctx child in
  assert_contains "inheritance preserves unmodified" "base-head" result;
  assert_contains "inheritance overrides block" "child-body" result

let test_template_include_render () =
  let header = Template.of_string "<header>hi</header>" in
  let tmpl = Template.of_string "{% include \"header.html\" %}<body>content</body>" in
  let loader = function
    | "header.html" -> header
    | _ -> []
  in
  let ctx = Template.create_context () in
  let result = Template.render ~loader ctx tmpl in
  assert_eq "template include" "<header>hi</header><body>content</body>" result

(* -------------------------------------------------------------------------- *)
(* Theme loading tests                                                        *)
(* -------------------------------------------------------------------------- *)

let test_theme_load () =
  let theme_dir = "_test_theme" in
  (try
    let entries = Sys.readdir theme_dir in
    Array.iter (fun e ->
      let p = Filename.concat theme_dir e in
      if Sys.is_directory p then (
        let sub = Sys.readdir p in
        Array.iter (fun f -> Sys.remove (Filename.concat p f)) sub;
        Sys.rmdir p
      ) else
        Sys.remove p
    ) entries;
    Sys.rmdir theme_dir
  with _ -> ());
  
  Theme.write_default_theme theme_dir;
  
  assert_true "theme dir exists" (Sys.file_exists theme_dir && Sys.is_directory theme_dir);
  assert_true "theme templates dir exists"
    (Sys.file_exists (Filename.concat theme_dir "templates") && Sys.is_directory (Filename.concat theme_dir "templates"));
  assert_true "theme static dir exists"
    (Sys.file_exists (Filename.concat theme_dir "static") && Sys.is_directory (Filename.concat theme_dir "static"));
  assert_true "theme.txt exists"
    (Sys.file_exists (Filename.concat theme_dir "theme.txt"));
  assert_true "base.html exists"
    (Sys.file_exists (Filename.concat (Filename.concat theme_dir "templates") "base.html"));
  assert_true "page.html exists"
    (Sys.file_exists (Filename.concat (Filename.concat theme_dir "templates") "page.html"));
  
  (match Theme.load theme_dir with
  | Ok theme ->
      let meta = Theme.theme_metadata theme in
      assert_eq "theme name" "default" meta.name;
      assert_eq "theme version" "1.0.0" meta.version;
      incr pass_count;
      Printf.printf "  PASS: theme load\n%!"
  | Error msg ->
      incr fail_count;
      Printf.printf "  FAIL: theme load: %s\n%!" msg);
  
  (* Cleanup *)
  (try
    Sys.remove (Filename.concat theme_dir "theme.txt");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "base.html");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "page.html");
    Sys.rmdir (Filename.concat theme_dir "templates");
    Sys.rmdir (Filename.concat theme_dir "static");
    Sys.rmdir theme_dir
  with _ -> ())

let test_theme_render_page () =
  let theme_dir = "_test_theme2" in
  (try
    let entries = Sys.readdir theme_dir in
    Array.iter (fun e ->
      let p = Filename.concat theme_dir e in
      if Sys.is_directory p then (
        let sub = Sys.readdir p in
        Array.iter (fun f -> Sys.remove (Filename.concat p f)) sub;
        Sys.rmdir p
      ) else
        Sys.remove p
    ) entries;
    Sys.rmdir theme_dir
  with _ -> ());
  
  Theme.write_default_theme theme_dir;
  
  (match Theme.load theme_dir with
  | Ok theme ->
      let result = Theme.render_page theme ~vars:[("title", "Test Page")] "<h1>Hello</h1>" in
      (match result with
      | Ok html ->
          assert_contains "theme render has title" "<title>Test Page</title>" html;
          assert_contains "theme render has content" "<h1>Hello</h1>" html;
          assert_contains "theme render has doctype" "<!DOCTYPE html>" html
      | Error msg ->
          incr fail_count;
          Printf.printf "  FAIL: theme render_page: %s\n%!" msg)
  | Error msg ->
      incr fail_count;
      Printf.printf "  FAIL: theme load for render: %s\n%!" msg);
  
  (* Cleanup *)
  (try
    Sys.remove (Filename.concat theme_dir "theme.txt");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "base.html");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "page.html");
    Sys.rmdir (Filename.concat theme_dir "templates");
    Sys.rmdir (Filename.concat theme_dir "static");
    Sys.rmdir theme_dir
  with _ -> ())

let test_theme_template_inheritance () =
  let theme_dir = "_test_theme3" in
  (try
    let entries = Sys.readdir theme_dir in
    Array.iter (fun e ->
      let p = Filename.concat theme_dir e in
      if Sys.is_directory p then (
        let sub = Sys.readdir p in
        Array.iter (fun f -> Sys.remove (Filename.concat p f)) sub;
        Sys.rmdir p
      ) else
        Sys.remove p
    ) entries;
    Sys.rmdir theme_dir
  with _ -> ());
  
  Theme.write_default_theme theme_dir;
  
  (* Create a custom page template that extends base and overrides content block *)
  let custom_page = Filename.concat (Filename.concat theme_dir "templates") "custom.html" in
  let oc = open_out custom_page in
  output_string oc "{% extends \"base.html\" %}\n{% block content %}<article>{{ content }}</article>{% endblock %}";
  close_out oc;
  
  (match Theme.load theme_dir with
  | Ok theme ->
      let result = Theme.render_page theme ~template:"custom.html" ~vars:[("title", "Custom")] "<p>text</p>" in
      (match result with
      | Ok html ->
          assert_contains "custom template has article" "<article>" html;
          assert_contains "custom template has content" "<p>text</p>" html
      | Error msg ->
          incr fail_count;
          Printf.printf "  FAIL: custom template render: %s\n%!" msg)
  | Error msg ->
      incr fail_count;
      Printf.printf "  FAIL: theme load for custom: %s\n%!" msg);
  
  (* Cleanup *)
  (try
    Sys.remove (Filename.concat theme_dir "theme.txt");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "base.html");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "page.html");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "custom.html");
    Sys.rmdir (Filename.concat theme_dir "templates");
    Sys.rmdir (Filename.concat theme_dir "static");
    Sys.rmdir theme_dir
  with _ -> ())

let test_theme_static_copy () =
  let theme_dir = "_test_theme4" in
  let output_dir = "_test_output_static" in
  (try
    let entries = Sys.readdir theme_dir in
    Array.iter (fun e ->
      let p = Filename.concat theme_dir e in
      if Sys.is_directory p then (
        let sub = Sys.readdir p in
        Array.iter (fun f -> Sys.remove (Filename.concat p f)) sub;
        Sys.rmdir p
      ) else
        Sys.remove p
    ) entries;
    Sys.rmdir theme_dir
  with _ -> ());
  (try
    let entries = Sys.readdir output_dir in
    Array.iter (fun e -> Sys.remove (Filename.concat output_dir e)) entries;
    Sys.rmdir output_dir
  with _ -> ());
  
  Theme.write_default_theme theme_dir;
  
  (* Create a CSS file in static dir *)
  let css_path = Filename.concat (Filename.concat theme_dir "static") "style.css" in
  let oc = open_out css_path in
  output_string oc "body { color: red; }";
  close_out oc;
  
  (match Theme.load theme_dir with
  | Ok theme ->
      Sys.mkdir output_dir 0o755;
      Theme.copy_static theme output_dir;
      let copied_css = Filename.concat output_dir "style.css" in
      assert_true "static copied" (Sys.file_exists copied_css);
      (if Sys.file_exists copied_css then
        let content =
          let ic = open_in copied_css in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          s
        in
        assert_eq "static content" "body { color: red; }" content);
      (try
        Sys.remove (Filename.concat output_dir "style.css");
        Sys.rmdir output_dir
      with _ -> ())
  | Error msg ->
      incr fail_count;
      Printf.printf "  FAIL: theme load for static: %s\n%!" msg;
      (try
        let entries = Sys.readdir output_dir in
        Array.iter (fun e -> Sys.remove (Filename.concat output_dir e)) entries;
        Sys.rmdir output_dir
      with _ -> ()));
  
  (* Cleanup *)
  (try
    Sys.remove (Filename.concat theme_dir "theme.txt");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "base.html");
    Sys.remove (Filename.concat (Filename.concat theme_dir "templates") "page.html");
    Sys.remove (Filename.concat (Filename.concat theme_dir "static") "style.css");
    Sys.rmdir (Filename.concat theme_dir "templates");
    Sys.rmdir (Filename.concat theme_dir "static");
    Sys.rmdir theme_dir
  with _ -> ())

(* -------------------------------------------------------------------------- *)
(* Build integration tests                                                    *)
(* -------------------------------------------------------------------------- *)

let test_build_with_theme () =
  let input_dir = "_test_input_theme" in
  let output_dir = "_test_output_theme" in
  let theme_dir = "_test_theme_build" in
  
  List.iter (fun dir ->
    (try
      let entries = Sys.readdir dir in
      Array.iter (fun e ->
        let p = Filename.concat dir e in
        if Sys.is_directory p then (
          let sub = Sys.readdir p in
          Array.iter (fun f -> Sys.remove (Filename.concat p f)) sub;
          Sys.rmdir p
        ) else
          Sys.remove p
      ) entries;
      Sys.rmdir dir
    with _ -> ())
  ) [input_dir; output_dir; theme_dir];
  
  Sys.mkdir input_dir 0o755;
  Sys.mkdir output_dir 0o755;
  Theme.write_default_theme theme_dir;
  
  (* Create a markdown file *)
  let md_path = Filename.concat input_dir "test.md" in
  let oc = open_out md_path in
  output_string oc "# Test Page\n\nHello from theme.\n";
  close_out oc;
  
  (* Build with theme *)
  (match Build.build ~input_dir ~output_dir ~theme_dir () with
  | Ok () ->
      let html_path = Filename.concat output_dir "test.html" in
      if Sys.file_exists html_path then (
        let content =
          let ic = open_in html_path in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          s
        in
        assert_contains "build with theme has title" "<title>Test Page</title>" content;
        assert_contains "build with theme has content" "<h1>Test Page</h1>" content;
        assert_contains "build with theme has doctype" "<!DOCTYPE html>" content
      ) else (
        incr fail_count;
        Printf.printf "  FAIL: build with theme - output file not found\n%!")
  | Error e ->
      incr fail_count;
      Printf.printf "  FAIL: build with theme failed: %s\n%!" e);
  
  (* Cleanup *)
  List.iter (fun dir ->
    (try
      let entries = Sys.readdir dir in
      Array.iter (fun e ->
        let p = Filename.concat dir e in
        if Sys.is_directory p then (
          let sub = Sys.readdir p in
          Array.iter (fun f -> Sys.remove (Filename.concat p f)) sub;
          Sys.rmdir p
        ) else
          Sys.remove p
      ) entries;
      Sys.rmdir dir
    with _ -> ())
  ) [input_dir; output_dir; theme_dir]

(* -------------------------------------------------------------------------- *)
(* Run all tests                                                              *)
(* -------------------------------------------------------------------------- *)

let run () =
  Printf.printf "Running template parsing tests...\n%!";
  test_template_text_only ();
  test_template_variable ();
  test_template_multiple_vars ();
  test_template_block ();
  test_template_extends ();
  test_template_include ();
  test_template_if ();
  test_template_if_else ();
  test_template_for ();
  
  Printf.printf "\nRunning template rendering tests...\n%!";
  test_render_text ();
  test_render_variable ();
  test_render_undefined_variable ();
  test_render_block ();
  test_render_if_true ();
  test_render_if_false ();
  test_render_for_loop ();
  
  Printf.printf "\nRunning template inheritance tests...\n%!";
  test_template_inheritance ();
  test_template_inheritance_preserves_base ();
  test_template_include_render ();
  
  Printf.printf "\nRunning theme loading tests...\n%!";
  test_theme_load ();
  test_theme_render_page ();
  test_theme_template_inheritance ();
  test_theme_static_copy ();
  
  Printf.printf "\nRunning build integration tests...\n%!";
  test_build_with_theme ();
  
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()
