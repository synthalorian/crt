(* Core transforms for the static site generator pipeline.

    Each transform is a pure function suitable for composition
    with [Pipeline.arr].
*)

val parse : string -> Ast.doc
(** [parse s] parses markdown string [s] into an AST document.
    Wrapper around [Markdown.parse]. *)

val highlight : Ast.doc -> Ast.doc
(** [highlight doc] syntax-highlights code blocks with recognized
    languages. Currently supports OCaml. Unrecognized languages
    and code blocks without a language are wrapped in [pre/code]
    tags without additional highlighting. *)

val toc : ?max_level:int -> Ast.doc -> Ast.doc
(** [toc ~max_level doc] extracts headings up to [max_level]
    (default 3) and prepends a table of contents to the document.
    Returns the document unchanged if no headings are found. *)
