(** Template engine with inheritance.

    A lightweight template system supporting variable substitution,
    block inheritance, includes, conditionals, and loops.

    Syntax:
    - [{{ variable }}] — variable substitution
    - [{% block name %}...{% endblock %}] — block definition
    - [{% extends "base.html" %}] — template inheritance
    - [{% include "partial.html" %}] — include partial template
    - [{% if var %}...{% else %}...{% endif %}] — conditional
    - [{% for item in items %}...{% endfor %}] — loop
*)

(** {1 Types} *)

type node =
  | Text of string
  | Var of string
  | Block of string * node list
  | Include of string
  | Extends of string
  | If of string * node list * node list
  | For of string * string * node list

type t = node list

type context
(** Rendering context holding variables and lists. *)

type loader = string -> t
(** A function that loads a template by path. *)

(** {1 Context} *)

val create_context : unit -> context
(** [create_context ()] creates a new empty rendering context. *)

val set_var : context -> string -> string -> unit
(** [set_var ctx key value] sets a string variable in the context. *)

val get_var : context -> string -> string option
(** [get_var ctx key] retrieves a variable from the context. *)

val set_list : context -> string -> string list -> unit
(** [set_list ctx key values] sets a list variable in the context. *)

val get_list : context -> string -> string list option
(** [get_list ctx key] retrieves a list from the context. *)

(** {1 Parsing} *)

val of_string : string -> t
(** [of_string s] parses a template from string [s]. *)

(** {1 Rendering} *)

val render : ?loader:loader -> context -> t -> string
(** [render ?loader ctx template] renders [template] with [ctx].
    If the template extends another, [loader] is used to resolve
    the base template. *)

val render_string : ?loader:loader -> context -> string -> string
(** [render_string ?loader ctx s] parses and renders [s] in one step. *)

(** {1 File I/O} *)

val read_file : string -> string
(** [read_file path] reads the contents of a file. *)

val load_file : string -> t
(** [load_file path] reads and parses a template file. *)
