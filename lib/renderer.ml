(** HTML generation.

    Converts the markdown AST into HTML strings.
*)

open Ast

let escape_html s =
  let b = Buffer.create (String.length s * 2) in
  String.iter (function
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '&' -> Buffer.add_string b "&amp;"
    | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let rec inline_to_html buf = function
  | Text s -> Buffer.add_string buf (escape_html s)
  | Bold children ->
      Buffer.add_string buf "<strong>";
      List.iter (inline_to_html buf) children;
      Buffer.add_string buf "</strong>"
  | Italic children ->
      Buffer.add_string buf "<em>";
      List.iter (inline_to_html buf) children;
      Buffer.add_string buf "</em>"
  | Code s ->
      Buffer.add_string buf "<code>";
      Buffer.add_string buf (escape_html s);
      Buffer.add_string buf "</code>"
  | Link { text; url } ->
      Buffer.add_string buf (Printf.sprintf "<a href=\"%s\">" (escape_html url));
      List.iter (inline_to_html buf) text;
      Buffer.add_string buf "</a>"
  | Break ->
      Buffer.add_string buf "<br>"

let rec block_to_html buf = function
  | Paragraph inlines ->
      Buffer.add_string buf "<p>";
      List.iter (inline_to_html buf) inlines;
      Buffer.add_string buf "</p>\n"
  | Heading { level; content } ->
      Printf.bprintf buf "<h%d>" level;
      List.iter (inline_to_html buf) content;
      Printf.bprintf buf "</h%d>\n" level
  | Blockquote blocks ->
      Buffer.add_string buf "<blockquote>\n";
      List.iter (block_to_html buf) blocks;
      Buffer.add_string buf "</blockquote>\n"
  | CodeBlock { language; code } ->
      let lang_attr = match language with
        | Some lang -> Printf.sprintf " class=\"language-%s\"" (escape_html lang)
        | None -> ""
      in
      Printf.bprintf buf "<pre><code%s>%s</code></pre>\n"
        lang_attr (escape_html code)
  | List { ordered; items } ->
      let tag = if ordered then "ol" else "ul" in
      Buffer.add_string buf (Printf.sprintf "<%s>\n" tag);
      List.iter (fun blocks ->
        Buffer.add_string buf "<li>";
        List.iter (block_to_html buf) blocks;
        Buffer.add_string buf "</li>\n"
      ) items;
      Buffer.add_string buf (Printf.sprintf "</%s>\n" tag)
  | ThematicBreak ->
      Buffer.add_string buf "<hr>\n"
  | Raw_html html_str ->
      Buffer.add_string buf html_str;
      Buffer.add_string buf "\n"

let render doc =
  let buf = Buffer.create 4096 in
  List.iter (block_to_html buf) doc;
  Buffer.contents buf

let render_page ?(title = "Untitled") ?(css = []) doc =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "<!DOCTYPE html>\n";
  Buffer.add_string buf "<html lang=\"en\">\n";
  Buffer.add_string buf "<head>\n";
  Buffer.add_string buf "<meta charset=\"utf-8\">\n";
  Buffer.add_string buf "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n";
  Printf.bprintf buf "<title>%s</title>\n" (escape_html title);
  List.iter (fun href ->
    Printf.bprintf buf "<link rel=\"stylesheet\" href=\"%s\">\n" (escape_html href)
  ) css;
  Buffer.add_string buf "</head>\n";
  Buffer.add_string buf "<body>\n";
  List.iter (block_to_html buf) doc;
  Buffer.add_string buf "</body>\n";
  Buffer.add_string buf "</html>\n";
  Buffer.contents buf
