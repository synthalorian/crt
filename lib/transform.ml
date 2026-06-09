open Ast

let parse = Markdown.parse

(* -------------------------------------------------------------------------- *)
(* Syntax highlighting                                                        *)
(* -------------------------------------------------------------------------- *)

module Highlighter = struct
  let ocaml_keywords = [
    "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
    "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
    "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type";
    "val"; "virtual"; "when"; "while"; "with"
  ]

  let is_keyword s = List.mem s ocaml_keywords

  let escape_html s =
    let b = Buffer.create (String.length s) in
    String.iter (function
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '&' -> Buffer.add_string b "&amp;"
      | c -> Buffer.add_char b c
    ) s;
    Buffer.contents b

  (* Naive tokenizer: splits on whitespace while preserving strings and comments *)
  let tokenize code =
    let len = String.length code in
    let rec aux i acc =
      if i >= len then List.rev acc
      else
        let c = code.[i] in
        if c = ' ' || c = '\t' || c = '\n' || c = '\r' then
          let j = ref (i + 1) in
          while !j < len &&
                let c = code.[!j] in
                c = ' ' || c = '\t' || c = '\n' || c = '\r'
          do
            incr j
          done;
          aux !j (String.sub code i (!j - i) :: acc)
        else if c = '"' then
          let j = ref (i + 1) in
          while !j < len && (code.[!j] <> '"' || code.[!j - 1] = '\\') do
            incr j
          done;
          let j = if !j < len then !j + 1 else !j in
          aux j (String.sub code i (j - i) :: acc)
        else if c = '(' && i + 1 < len && code.[i + 1] = '*' then
          let j = ref (i + 2) in
          while !j < len - 1 && not (code.[!j] = '*' && code.[!j + 1] = ')') do
            incr j
          done;
          let j = if !j < len - 1 then !j + 2 else !j in
          aux j (String.sub code i (j - i) :: acc)
        else
          let j = ref (i + 1) in
          while !j < len &&
                let c = code.[!j] in
                not (c = ' ' || c = '\t' || c = '\n' || c = '\r' ||
                     c = '"' ||
                     (c = '(' && !j + 1 < len && code.[!j + 1] = '*'))
          do
            incr j
          done;
          aux !j (String.sub code i (!j - i) :: acc)
    in
    aux 0 []

  let colorize_token tok =
    if String.length tok = 0 then tok
    else if tok.[0] = '"' then
      Printf.sprintf "<span class=\"str\">%s</span>" (escape_html tok)
    else if String.length tok > 1 && tok.[0] = '(' && tok.[1] = '*' then
      Printf.sprintf "<span class=\"cmt\">%s</span>" (escape_html tok)
    else if is_keyword tok then
      Printf.sprintf "<span class=\"kwd\">%s</span>" (escape_html tok)
    else
      try
        let _ = int_of_string tok in
        Printf.sprintf "<span class=\"num\">%s</span>" (escape_html tok)
      with _ ->
        escape_html tok

  let highlight_ocaml code =
    tokenize code
    |> List.map colorize_token
    |> String.concat ""
end

let highlight_block = function
  | CodeBlock { language = Some "ocaml"; code } ->
      let body = Highlighter.highlight_ocaml code in
      Raw_html (Printf.sprintf "<pre><code class=\"language-ocaml\">%s</code></pre>" body)
  | CodeBlock { language = Some lang; code } ->
      Raw_html (Printf.sprintf "<pre><code class=\"language-%s\">%s</code></pre>"
        lang (Highlighter.escape_html code))
  | CodeBlock { language = None; code } ->
      Raw_html (Printf.sprintf "<pre><code>%s</code></pre>" (Highlighter.escape_html code))
  | b -> b

let highlight doc = List.map highlight_block doc

(* -------------------------------------------------------------------------- *)
(* Table of contents                                                          *)
(* -------------------------------------------------------------------------- *)

let inline_to_string inlines =
  let rec aux = function
    | Text s -> s
    | Bold children -> String.concat "" (List.map aux children)
    | Italic children -> String.concat "" (List.map aux children)
    | Code s -> s
    | Link { text; _ } -> String.concat "" (List.map aux text)
    | Break -> " "
  in
  String.concat "" (List.map aux inlines)

let slugify s =
  let b = Buffer.create (String.length s) in
  let prev_dash = ref false in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then (
      prev_dash := false;
      Buffer.add_char b c)
    else if c >= 'A' && c <= 'Z' then (
      prev_dash := false;
      Buffer.add_char b (Char.lowercase_ascii c))
    else if (c = ' ' || c = '-' || c = '_') && not !prev_dash then (
      prev_dash := true;
      Buffer.add_char b '-')
  ) s;
  let result = Buffer.contents b in
  let result =
    if String.length result > 0 && result.[0] = '-' then
      String.sub result 1 (String.length result - 1)
    else result
  in
  if String.length result > 0 && result.[String.length result - 1] = '-' then
    String.sub result 0 (String.length result - 1)
  else result

let toc ?(max_level = 3) doc =
  let rec collect acc = function
    | [] -> List.rev acc
    | Heading { level; content } :: rest when level <= max_level ->
        let text = inline_to_string content in
        let slug = slugify text in
        collect ((level, text, slug) :: acc) rest
    | _ :: rest -> collect acc rest
  in
  let headings = collect [] doc in
  if headings = [] then doc
  else
    let toc_items = List.map (fun (level, text, slug) ->
      let indent = String.make ((level - 1) * 4) ' ' in
      [Paragraph [Text (indent ^ "• "); Link { text = [Text text]; url = "#" ^ slug }]]
    ) headings in
    let toc_doc =
      [ Heading { level = 2; content = [Text "Table of Contents"] };
        List { ordered = false; items = toc_items } ]
    in
    toc_doc @ doc
