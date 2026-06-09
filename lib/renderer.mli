(** HTML generation.

    Converts the markdown AST into HTML strings and full pages.
*)

val render : Ast.doc -> string
(** [render doc] converts an AST document to an HTML fragment string. *)

val render_page :
  ?title:string ->
  ?css:string list ->
  ?dev_mode:bool ->
  Ast.doc -> string
(** [render_page ~title ~css ~dev_mode doc] renders a complete HTML page
    with the given title, CSS links, and optional dev-mode reload script. *)

val render_page_with_theme :
  ?title:string ->
  ?css:string list ->
  ?dev_mode:bool ->
  ?template:string ->
  Theme.t ->
  Ast.doc -> string
(** [render_page_with_theme ~title ~css ~dev_mode ~template theme doc]
    renders a complete HTML page using the given [theme]. Falls back
    to [render_page] if the theme template is not found. *)
