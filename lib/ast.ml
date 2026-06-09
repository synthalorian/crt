(** Markdown AST definitions for Phase 1 parser.

    The AST separates block-level elements from inline elements,
    matching the semantic structure of markdown documents.
*)

type inline =
  | Text of string
  | Bold of inline list
  | Italic of inline list
  | Code of string
  | Link of { text : inline list; url : string }
  | Break

(** Block-level elements represent the top-level structure of a document. *)
type block =
  | Paragraph of inline list
  | Heading of { level : int; content : inline list }
  | Blockquote of block list
  | CodeBlock of { language : string option; code : string }
  | List of { ordered : bool; items : block list list }
  | ThematicBreak

(** A document is an ordered list of blocks. *)
type doc = block list
