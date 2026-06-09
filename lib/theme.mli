(** Theme engine with template inheritance.

    Themes are directories containing templates and static assets.
    The theme engine manages template loading, inheritance resolution,
    and asset copying during the build process.
*)

(** {1 Types} *)

type metadata = {
  name : string;
  version : string;
  description : string option;
  author : string option;
  parent : string option;  (** For future theme inheritance. *)
}

type t
(** An opaque theme handle. *)

(** {1 Loading} *)

val load : ?templates_subdir:string -> ?static_subdir:string -> string -> (t, string) result
(** [load dir] loads a theme from directory [dir].
    Expects a [theme.txt] metadata file and [templates/] and [static/]
    subdirectories. Returns [Error msg] if the directory doesn't exist
    or can't be read. *)

val theme_dir : t -> string
(** [theme_dir theme] returns the theme's base directory. *)

val theme_metadata : t -> metadata
(** [theme_metadata theme] returns the theme's metadata. *)

(** {1 Templates} *)

val template_path : t -> string -> string
(** [template_path theme name] returns the full path to template [name]. *)

val load_template : t -> string -> (Template.t, string) result
(** [load_template theme name] loads and parses a template by name. *)

val create_loader : t -> Template.loader
(** [create_loader theme] creates a template loader function for [theme]. *)

(** {1 Static assets} *)

val copy_static : t -> string -> unit
(** [copy_static theme output_dir] copies static assets from the theme
    to [output_dir]. *)

(** {1 Rendering} *)

val render_page :
  t ->
  ?template:string ->
  ?vars:(string * string) list ->
  ?lists:(string * string list) list ->
  string ->
  (string, string) result
(** [render_page theme ~template ~vars ~lists content_html] renders a page
    using the specified template. [content_html] is set as the [content]
    variable. Returns the rendered HTML string. *)

val render_with_template :
  t -> template:string -> Template.context -> (string, string) result
(** [render_with_template theme ~template ctx] renders a template with
    a pre-built context. *)

(** {1 Default theme} *)

val write_default_theme : string -> unit
(** [write_default_theme dir] writes a minimal default theme to [dir].
    Creates [theme.txt], [templates/base.html], and [templates/page.html]. *)
