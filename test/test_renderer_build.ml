open Crt_lib

let pass_count = ref 0
let fail_count = ref 0

let assert_eq name expected actual =
  if expected = actual then (
    incr pass_count;
    Printf.printf "  PASS: %s\n%!" name)
  else (
    incr fail_count;
    Printf.printf "  FAIL: %s\n%!" name;
    Printf.printf "    expected: %s\n%!" (String.escaped expected);
    Printf.printf "    actual:   %s\n%!" (String.escaped actual))

let contains_substring sub s =
  let sub_len = String.length sub in
  let s_len = String.length s in
  if sub_len = 0 then true
  else if sub_len > s_len then false
  else
    let rec check i =
      if i > s_len - sub_len then false
      else if String.sub s i sub_len = sub then true
      else check (i + 1)
    in
    check 0

let assert_contains name substr s =
  if contains_substring substr s then (
    incr pass_count;
    Printf.printf "  PASS: %s\n%!" name)
  else (
    incr fail_count;
    Printf.printf "  FAIL: %s\n%!" name;
    Printf.printf "    expected to contain: %s\n%!" (String.escaped substr);
    Printf.printf "    actual:   %s\n%!" (String.escaped s))

(* -------------------------------------------------------------------------- *)
(* Renderer tests                                                             *)
(* -------------------------------------------------------------------------- *)

let test_render_paragraph () =
  let doc = [Ast.Paragraph [Ast.Text "hello world"]] in
  let html = Renderer.render doc in
  assert_eq "render paragraph" "<p>hello world</p>\n" html

let test_render_heading () =
  let doc = [Ast.Heading { level = 1; content = [Ast.Text "Title"] }] in
  let html = Renderer.render doc in
  assert_eq "render h1" "<h1>Title</h1>\n" html

let test_render_bold () =
  let doc = [Ast.Paragraph [Ast.Bold [Ast.Text "bold"]]] in
  let html = Renderer.render doc in
  assert_eq "render bold" "<p><strong>bold</strong></p>\n" html

let test_render_italic () =
  let doc = [Ast.Paragraph [Ast.Italic [Ast.Text "italic"]]] in
  let html = Renderer.render doc in
  assert_eq "render italic" "<p><em>italic</em></p>\n" html

let test_render_code () =
  let doc = [Ast.Paragraph [Ast.Code "let x = 1"]] in
  let html = Renderer.render doc in
  assert_eq "render code" "<p><code>let x = 1</code></p>\n" html

let test_render_link () =
  let doc = [Ast.Paragraph [Ast.Link { text = [Ast.Text "link"]; url = "http://example.com" }]] in
  let html = Renderer.render doc in
  assert_eq "render link" "<p><a href=\"http://example.com\">link</a></p>\n" html

let test_render_code_block () =
  let doc = [Ast.CodeBlock { language = Some "ocaml"; code = "let x = 1" }] in
  let html = Renderer.render doc in
  assert_eq "render code block"
    "<pre><code class=\"language-ocaml\">let x = 1</code></pre>\n" html

let test_render_list () =
  let doc = [Ast.List { ordered = false; items = [[Ast.Paragraph [Ast.Text "item"]]] }] in
  let html = Renderer.render doc in
  assert_eq "render list" "<ul>\n<li><p>item</p>\n</li>\n</ul>\n" html

let test_render_page () =
  let doc = [Ast.Heading { level = 1; content = [Ast.Text "Hello"] }] in
  let html = Renderer.render_page ~title:"My Page" doc in
  assert_contains "render page has doctype" "<!DOCTYPE html>" html;
  assert_contains "render page has title" "<title>My Page</title>" html;
  assert_contains "render page has heading" "<h1>Hello</h1>" html

let test_render_escapes_html () =
  let doc = [Ast.Paragraph [Ast.Text "<script>alert('xss')</script>"]] in
  let html = Renderer.render doc in
  assert_contains "render escapes html" "&lt;script&gt;" html;
  if not (contains_substring "<script>" html) then (
    incr pass_count;
    Printf.printf "  PASS: render no raw script\n%!")
  else (
    incr fail_count;
    Printf.printf "  FAIL: render no raw script\n%!";
    Printf.printf "    raw script tag found in output\n%!")

(* -------------------------------------------------------------------------- *)
(* Build tests                                                                *)
(* -------------------------------------------------------------------------- *)

let test_build_basic () =
  let input_dir = "_test_input" in
  let output_dir = "_test_output" in
  (try Sys.remove (Filename.concat output_dir "test.md") with _ -> ());
  (try Sys.rmdir output_dir with _ -> ());
  (try Sys.remove (Filename.concat input_dir "test.md") with _ -> ());
  (try Sys.rmdir input_dir with _ -> ());
  
  Sys.mkdir input_dir 0o755;
  let oc = open_out (Filename.concat input_dir "test.md") in
  output_string oc "# Test\n\nHello world.\n";
  close_out oc;
  
  (match Build.build ~input_dir ~output_dir () with
  | Ok () ->
      let output_path = Filename.concat output_dir "test.html" in
      if Sys.file_exists output_path then (
        let content =
          let ic = open_in output_path in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          s
        in
        assert_contains "build creates html" "<h1>Test</h1>" content;
        assert_contains "build has title" "<title>Test</title>" content
      ) else (
        incr fail_count;
        Printf.printf "  FAIL: build creates output file\n%!";
        Printf.printf "    output file not found\n%!")
  | Error e ->
      incr fail_count;
      Printf.printf "  FAIL: build failed: %s\n%!" e);
  
  (try Sys.remove (Filename.concat input_dir "test.md") with _ -> ());
  (try Sys.rmdir input_dir with _ -> ());
  (try Sys.remove (Filename.concat output_dir "test.html") with _ -> ());
  (try Sys.rmdir output_dir with _ -> ())

let run () =
  Printf.printf "Running renderer and build tests...\n%!";
  test_render_paragraph ();
  test_render_heading ();
  test_render_bold ();
  test_render_italic ();
  test_render_code ();
  test_render_link ();
  test_render_code_block ();
  test_render_list ();
  test_render_page ();
  test_render_escapes_html ();
  test_build_basic ();
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()
