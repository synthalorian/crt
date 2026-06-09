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

let test_paragraph () =
  let doc = Markdown.parse "hello world" in
  assert_eq "simple paragraph"
    [Ast.Paragraph [Ast.Text "hello world"]]
    doc

let test_heading () =
  assert_eq "h1"
    [Ast.Heading { level = 1; content = [Ast.Text "Title"] }]
    (Markdown.parse "# Title");
  assert_eq "h2"
    [Ast.Heading { level = 2; content = [Ast.Text "Subtitle"] }]
    (Markdown.parse "## Subtitle");
  assert_eq "h3"
    [Ast.Heading { level = 3; content = [Ast.Text "Deep"] }]
    (Markdown.parse "### Deep")

let test_bold () =
  assert_eq "bold with stars"
    [Ast.Paragraph [Ast.Bold [Ast.Text "bold text"]]]
    (Markdown.parse "**bold text**");
  assert_eq "bold with underscores"
    [Ast.Paragraph [Ast.Bold [Ast.Text "bold text"]]]
    (Markdown.parse "__bold text__")

let test_italic () =
  assert_eq "italic with star"
    [Ast.Paragraph [Ast.Italic [Ast.Text "italic"] ]]
    (Markdown.parse "*italic*");
  assert_eq "italic with underscore"
    [Ast.Paragraph [Ast.Italic [Ast.Text "italic"] ]]
    (Markdown.parse "_italic_")

let test_code_inline () =
  assert_eq "inline code"
    [Ast.Paragraph [Ast.Code "foo"]]
    (Markdown.parse "`foo`");
  assert_eq "inline code with spaces"
    [Ast.Paragraph [Ast.Code "hello world"]]
    (Markdown.parse "`hello world`")

let test_code_block () =
  let doc = Markdown.parse "```ocaml\nlet x = 1\n```\n" in
  assert_eq "fenced code block"
    [Ast.CodeBlock { language = Some "ocaml"; code = "let x = 1" }]
    doc

let test_thematic_break () =
  assert_eq "thematic break"
    [Ast.ThematicBreak]
    (Markdown.parse "---\n")

let test_link () =
  assert_eq "simple link"
    [Ast.Paragraph [Ast.Link { text = [Ast.Text "example"]; url = "https://example.com" }]]
    (Markdown.parse "[example](https://example.com)")

let test_multiple_paragraphs () =
  let doc = Markdown.parse "first paragraph\n\nsecond paragraph" in
  assert_eq "multiple paragraphs"
    [ Ast.Paragraph [Ast.Text "first paragraph"];
      Ast.Paragraph [Ast.Text "second paragraph"] ]
    doc

let test_list_unordered () =
  let doc = Markdown.parse "- one\n- two\n- three\n" in
  assert_eq "unordered list"
    [Ast.List { ordered = false;
                items =
                  [ [Ast.Paragraph [Ast.Text "one"]];
                    [Ast.Paragraph [Ast.Text "two"]];
                    [Ast.Paragraph [Ast.Text "three"]] ] }]
    doc

let test_blockquote () =
  let doc = Markdown.parse "> quoted" in
  assert_eq "blockquote"
    [Ast.Blockquote [Ast.Paragraph [Ast.Text "quoted"]]]
    doc

let run () =
  Printf.printf "Running parser tests...\n%!";
  test_paragraph ();
  test_heading ();
  test_bold ();
  test_italic ();
  test_code_inline ();
  test_code_block ();
  test_thematic_break ();
  test_link ();
  test_multiple_paragraphs ();
  test_list_unordered ();
  test_blockquote ();
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()
