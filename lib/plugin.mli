(** Plugin system for dynamic module loading.

    Plugins can register transforms and hooks at various stages
    of the build pipeline. Plugins are loaded dynamically using
    OCaml's [Dynlink] facility from [.cmxs] shared libraries.

    Each plugin is a compiled OCaml module that calls [Plugin.register]
    at initialization time (top-level side effect). When the [.cmxs]
    is loaded via [Dynlink], the registration runs and the plugin
    becomes active.
*)

type stage =
  | Post_parse  (** After markdown parsing, before built-in transforms *)
  | Pre_render  (** After built-in transforms, before HTML rendering *)

type transform = string * (Ast.doc -> Ast.doc)
(** A named transform function operating on the document AST. *)

type doc_hook = stage * (Ast.doc -> Ast.doc)
(** A hook that operates on the document AST at a specific stage. *)

type html_hook = string -> string
(** A hook that operates on the rendered HTML output. *)

type t = {
  name : string;
  version : string;
  transforms : transform list;
  doc_hooks : doc_hook list;
  html_hooks : html_hook list;
}
(** A plugin descriptor. *)

val create :
  name:string ->
  version:string ->
  ?transforms:transform list ->
  ?doc_hooks:doc_hook list ->
  ?html_hooks:html_hook list ->
  unit ->
  t
(** [create ~name ~version ?transforms ?doc_hooks ?html_hooks ()] creates a new plugin descriptor. *)

val register : t -> unit
(** [register plugin] adds [plugin] to the global registry.
    Plugins typically call this at module initialization time. *)

val registry : unit -> t list
(** [registry ()] returns the list of currently registered plugins. *)

val clear_registry : unit -> unit
(** [clear_registry ()] removes all plugins from the registry.
    Useful for testing. *)

(** {1 Dynamic loading} *)

val load_cmxs : string -> (unit, string) result
(** [load_cmxs path] dynamically loads a plugin from a [.cmxs] file
    using [Dynlink]. Returns [Ok ()] on success or [Error msg] on failure. *)

val load_dir : string -> (int, string) result
(** [load_dir dir] scans [dir] for [.cmxs] files and loads each one.
    Returns [Ok count] with the number of plugins loaded, or [Error msg]. *)

(** {1 Applying plugins} *)

val apply_doc_hooks : stage:stage -> Ast.doc -> Ast.doc
(** [apply_doc_hooks ~stage doc] applies all registered doc hooks for [stage]
    to [doc] in registration order. *)

val apply_html_hooks : string -> string
(** [apply_html_hooks html] applies all registered HTML hooks to [html]
    in registration order. *)

val apply_transforms : Ast.doc -> Ast.doc
(** [apply_transforms doc] applies all registered transforms to [doc]
    in registration order. *)

val transform_names : unit -> string list
(** [transform_names ()] returns the names of all registered transforms. *)

val plugin_names : unit -> string list
(** [plugin_names ()] returns the names of all registered plugins. *)
