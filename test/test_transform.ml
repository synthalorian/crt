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
    Printf.printf "    expected: %s\n%!"
      (String.escaped (Marshal.to_string expected []));
    Printf.printf "    actual:   %s\n%!"
      (String.escaped (Marshal.to_string actual [])))

(* -------------------------------------------------------------------------- *)
(* Parse transform                                                            *)
(* -------------------------------------------------------------------------- *)

let test_parse () =
  let doc = Transform.parse "# Hello\n\nworld" in
  assert_eq "parse heading and paragraph"
    [Ast.Heading { level = 1; content = [Ast.Text "Hello"] };
     Ast.Paragraph [Ast.Text "world"]]
    doc

let test_parse_empty () =
  let doc = Transform.parse "" in
  assert_eq "parse empty" [] doc

(* -------------------------------------------------------------------------- *)
(* Highlight transform                                                        *)
(* -------------------------------------------------------------------------- *)

let test_highlight_ocaml () =
  let doc = Transform.highlight
    [Ast.CodeBlock { language = Some "ocaml"; code = "let x = 42" }]
  in
  match doc with
  | [Ast.Raw_html s] ->
      assert_eq "highlight ocaml produces Raw_html" true
        (String.starts_with ~prefix:"<pre><code class=\"language-ocaml\">" s);
      assert_eq "highlight ocaml contains keyword span" true
        (String.contains s 'k' && String.contains s 'w' && String.contains s 'd')
  | _ -> assert_eq "highlight ocaml" true false

let test_highlight_other_lang () =
  let doc = Transform.highlight
    [Ast.CodeBlock { language = Some "python"; code = "print(1)" }]
  in
  match doc with
  | [Ast.Raw_html s] ->
      assert_eq "highlight other lang produces Raw_html" true
        (String.starts_with ~prefix:"<pre><code class=\"language-python\">" s)
  | _ -> assert_eq "highlight other lang" true false

let test_highlight_no_lang () =
  let doc = Transform.highlight
    [Ast.CodeBlock { language = None; code = "some code" }]
  in
  match doc with
  | [Ast.Raw_html s] ->
      assert_eq "highlight no lang produces Raw_html" true
        (String.starts_with ~prefix:"<pre><code>" s)
  | _ -> assert_eq "highlight no lang" true false

let test_highlight_preserves_non_code () =
  let input =
    [Ast.Paragraph [Ast.Text "hello"];
     Ast.Heading { level = 1; content = [Ast.Text "Title"] }]
  in
  let doc = Transform.highlight input in
  assert_eq "highlight preserves non-code blocks" input doc

let test_highlight_mixed () =
  let input =
    [Ast.Paragraph [Ast.Text "intro"];
     Ast.CodeBlock { language = Some "ocaml"; code = "let x = 1" };
     Ast.Paragraph [Ast.Text "outro"]]
  in
  let doc = Transform.highlight input in
  match doc with
  | [Ast.Paragraph _; Ast.Raw_html _; Ast.Paragraph _] ->
      assert_eq "highlight mixed blocks" true true
  | _ -> assert_eq "highlight mixed blocks" true false

(* -------------------------------------------------------------------------- *)
(* TOC transform                                                              *)
(* -------------------------------------------------------------------------- *)

let test_toc_empty () =
  let input = [Ast.Paragraph [Ast.Text "no headings here"]] in
  let doc = Transform.toc input in
  assert_eq "toc empty document" input doc

let test_toc_simple () =
  let input = [Ast.Heading { level = 1; content = [Ast.Text "Hello"] }] in
  let doc = Transform.toc input in
  match doc with
  | [Ast.Heading { level = 2; content = [Ast.Text "Table of Contents"] };
     Ast.List { ordered = false; items = [[Ast.Paragraph [Ast.Text "• "; Ast.Link { text = [Ast.Text "Hello"]; url = "#hello" }]]] };
     Ast.Heading { level = 1; content = [Ast.Text "Hello"] }] ->
      assert_eq "toc simple" true true
  | _ -> assert_eq "toc simple" true false

let test_toc_multiple_levels () =
  let input =
    [Ast.Heading { level = 1; content = [Ast.Text "Top"] };
     Ast.Paragraph [Ast.Text "text"];
     Ast.Heading { level = 2; content = [Ast.Text "Sub"] };
     Ast.Heading { level = 3; content = [Ast.Text "Deep"] }]
  in
  let doc = Transform.toc input in
  let expected_items =
    [ [Ast.Paragraph [Ast.Text "• "; Ast.Link { text = [Ast.Text "Top"]; url = "#top" }]];
      [Ast.Paragraph [Ast.Text "    • "; Ast.Link { text = [Ast.Text "Sub"]; url = "#sub" }]];
      [Ast.Paragraph [Ast.Text "        • "; Ast.Link { text = [Ast.Text "Deep"]; url = "#deep" }]] ]
  in
  match doc with
  | [Ast.Heading { level = 2; content = [Ast.Text "Table of Contents"] };
     Ast.List { ordered = false; items };
     Ast.Heading { level = 1; content = [Ast.Text "Top"] };
     Ast.Paragraph [Ast.Text "text"];
     Ast.Heading { level = 2; content = [Ast.Text "Sub"] };
     Ast.Heading { level = 3; content = [Ast.Text "Deep"] }] ->
      assert_eq "toc multiple levels items" expected_items items
  | _ -> assert_eq "toc multiple levels" true false

let test_toc_max_level () =
  let input =
    [Ast.Heading { level = 1; content = [Ast.Text "H1"] };
     Ast.Heading { level = 4; content = [Ast.Text "H4"] }]
  in
  let doc = Transform.toc ~max_level:2 input in
  match doc with
  | [Ast.Heading { level = 2; content = [Ast.Text "Table of Contents"] };
     Ast.List { ordered = false; items = [[Ast.Paragraph [Ast.Text "• "; Ast.Link { text = [Ast.Text "H1"]; url = "#h1" }]]] };
     Ast.Heading { level = 1; content = [Ast.Text "H1"] };
     Ast.Heading { level = 4; content = [Ast.Text "H4"] }] ->
      assert_eq "toc max level" true true
  | _ -> assert_eq "toc max level" true false

let test_toc_slugify () =
  let input = [Ast.Heading { level = 1; content = [Ast.Text "Hello World"] }] in
  let doc = Transform.toc input in
  match doc with
  | [_; Ast.List { items = [[Ast.Paragraph [_; Ast.Link { url = "#hello-world"; _ }]]]; _ }; _] ->
      assert_eq "toc slugify" true true
  | _ -> assert_eq "toc slugify" true false

(* -------------------------------------------------------------------------- *)
(* Integration: pipeline composition                                          *)
(* -------------------------------------------------------------------------- *)

let test_pipeline_integration () =
  let p = Pipeline.pure "# Title\n\nSome text.\n\n```ocaml\nlet x = 1\n```"
    |> Pipeline.arr Transform.parse
    |> Pipeline.arr Transform.toc
    |> Pipeline.arr Transform.highlight
  in
  match Pipeline.run p with
  | Ok doc ->
      (* Should have: TOC heading, TOC list, H1, paragraph, highlighted code *)
      assert_eq "pipeline integration length" 5 (List.length doc);
      (* First element should be TOC heading *)
      (match List.hd doc with
       | Ast.Heading { level = 2; content = [Ast.Text "Table of Contents"] } ->
           assert_eq "pipeline integration toc heading" true true
       | _ -> assert_eq "pipeline integration toc heading" true false);
      (* Last element should be Raw_html from highlighting *)
      (match List.(hd (rev doc)) with
       | Ast.Raw_html _ -> assert_eq "pipeline integration highlighted" true true
       | _ -> assert_eq "pipeline integration highlighted" true false)
  | Error _ -> assert_eq "pipeline integration" true false

let run () =
  Printf.printf "Running transform tests...\n%!";
  test_parse ();
  test_parse_empty ();
  test_highlight_ocaml ();
  test_highlight_other_lang ();
  test_highlight_no_lang ();
  test_highlight_preserves_non_code ();
  test_highlight_mixed ();
  test_toc_empty ();
  test_toc_simple ();
  test_toc_multiple_levels ();
  test_toc_max_level ();
  test_toc_slugify ();
  test_pipeline_integration ();
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()
