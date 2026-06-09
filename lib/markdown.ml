(** Markdown parser driver.

    Exposes a single function [parse] that converts a markdown string
    into an [Ast.doc]. Uses menhir for parsing and ocamllex for lexing.
*)

exception Parse_error of string

let parse s =
  let lexbuf = Lexing.from_string s in
  try Parser.doc Lexer.main_token lexbuf
  with Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    let msg =
      Printf.sprintf "parse error at line %d, column %d"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
    in
    raise (Parse_error msg)
