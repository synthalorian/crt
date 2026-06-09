(** Markdown parser driver. *)

exception Parse_error of string

val parse : string -> Ast.doc
(** [parse s] parses markdown string [s] into an AST document.
    Raises [Parse_error msg] on syntax errors. *)
