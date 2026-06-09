(** Theme engine with template inheritance.

    Themes are directories containing templates and static assets.
    The theme engine manages template loading, inheritance resolution,
    and asset copying during the build process.
*)

(* -------------------------------------------------------------------------- *)
(* Theme metadata                                                             *)
(* -------------------------------------------------------------------------- *)

type metadata = {
  name : string;
  version : string;
  description : string option;
  author : string option;
  parent : string option;  (* For theme inheritance in the future *)
}

type t = {
  dir : string;
  metadata : metadata;
  templates_dir : string;
  static_dir : string;
  template_cache : (string, Template.t) Hashtbl.t;
}

(* -------------------------------------------------------------------------- *)
(* Loading                                                                    *)
(* -------------------------------------------------------------------------- *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let ensure_dir path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    Sys.mkdir path 0o755

let parse_metadata s =
  (* Simple key: value parser for theme metadata *)
  let lines = String.split_on_char '\n' s in
  let tbl = Hashtbl.create 8 in
  List.iter (fun line ->
    let line = String.trim line in
    if line <> "" && line.[0] <> '#' then
      match String.index_opt line ':' with
      | Some idx ->
          let key = String.trim (String.sub line 0 idx) in
          let value = String.trim (String.sub line (idx + 1) (String.length line - idx - 1)) in
          Hashtbl.replace tbl key value
      | None -> ()
  ) lines;
  {
    name = (try Hashtbl.find tbl "name" with Not_found -> "unnamed");
    version = (try Hashtbl.find tbl "version" with Not_found -> "0.0.1");
    description = (try Some (Hashtbl.find tbl "description") with Not_found -> None);
    author = (try Some (Hashtbl.find tbl "author") with Not_found -> None);
    parent = (try Some (Hashtbl.find tbl "parent") with Not_found -> None);
  }

let load ?(templates_subdir = "templates") ?(static_subdir = "static") dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    Error (Printf.sprintf "Theme directory not found: %s" dir)
  else
    try
      let meta_path = Filename.concat dir "theme.txt" in
      let metadata =
        if Sys.file_exists meta_path then
          parse_metadata (read_file meta_path)
        else
          { name = Filename.basename dir; version = "0.0.1";
            description = None; author = None; parent = None }
      in
      let templates_dir = Filename.concat dir templates_subdir in
      let static_dir = Filename.concat dir static_subdir in
      Ok {
        dir;
        metadata;
        templates_dir;
        static_dir;
        template_cache = Hashtbl.create 16;
      }
    with exn ->
      Error (Printf.sprintf "Error loading theme from %s: %s" dir (Printexc.to_string exn))

(* -------------------------------------------------------------------------- *)
(* Template resolution                                                        *)
(* -------------------------------------------------------------------------- *)

let theme_dir theme = theme.dir
let theme_metadata theme = theme.metadata

let template_path theme name =
  Filename.concat theme.templates_dir name

let load_template theme name =
  try
    match Hashtbl.find_opt theme.template_cache name with
    | Some tmpl -> Ok tmpl
    | None ->
        let path = template_path theme name in
        if Sys.file_exists path then
          let tmpl = Template.load_file path in
          Hashtbl.replace theme.template_cache name tmpl;
          Ok tmpl
        else
          Error (Printf.sprintf "Template not found: %s" path)
  with exn ->
    Error (Printf.sprintf "Error loading template %s: %s" name (Printexc.to_string exn))

let create_loader theme =
  fun name ->
    match load_template theme name with
    | Ok tmpl -> tmpl
    | Error msg ->
        Printf.eprintf "[theme] %s\n%!" msg;
        []

(* -------------------------------------------------------------------------- *)
(* Static assets                                                              *)
(* -------------------------------------------------------------------------- *)

let rec copy_dir src dst =
  if Sys.file_exists src && Sys.is_directory src then (
    ensure_dir dst;
    let entries = Sys.readdir src in
    Array.iter (fun entry ->
      let src_path = Filename.concat src entry in
      let dst_path = Filename.concat dst entry in
      if Sys.is_directory src_path then
        copy_dir src_path dst_path
      else (
        let ic = open_in_bin src_path in
        let n = in_channel_length ic in
        let data = really_input_string ic n in
        close_in ic;
        let oc = open_out_bin dst_path in
        output_string oc data;
        close_out oc
      )
    ) entries
  )

let copy_static theme output_dir =
  if Sys.file_exists theme.static_dir && Sys.is_directory theme.static_dir then
    copy_dir theme.static_dir output_dir

(* -------------------------------------------------------------------------- *)
(* Rendering helpers                                                          *)
(* -------------------------------------------------------------------------- *)

let render_page theme ?(template = "page.html") ?(vars = []) ?(lists = []) content_html =
  let loader = create_loader theme in
  let ctx = Template.create_context () in
  List.iter (fun (k, v) -> Template.set_var ctx k v) vars;
  List.iter (fun (k, v) -> Template.set_list ctx k v) lists;
  Template.set_var ctx "content" content_html;
  match load_template theme template with
  | Ok tmpl ->
      Ok (Template.render ~loader ctx tmpl)
  | Error msg ->
      (* Fallback: render without theme *)
      Printf.eprintf "[theme] %s, using fallback\n%!" msg;
      Ok content_html

let render_with_template theme ~template ctx =
  let loader = create_loader theme in
  match load_template theme template with
  | Ok tmpl -> Ok (Template.render ~loader ctx tmpl)
  | Error msg -> Error msg

(* -------------------------------------------------------------------------- *)
(* Default theme                                                              *)
(* -------------------------------------------------------------------------- *)

let default_page_template =
  "<!DOCTYPE html>\n" ^
  "<html lang=\"en\">\n" ^
  "<head>\n" ^
  "<meta charset=\"utf-8\">\n" ^
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" ^
  "<title>{{ title }}</title>\n" ^
  "{% block css %}{% endblock %}\n" ^
  "</head>\n" ^
  "<body>\n" ^
  "{% block content %}{{ content }}{% endblock %}\n" ^
  "{% block scripts %}{% endblock %}\n" ^
  "</body>\n" ^
  "</html>\n"

let write_default_theme dir =
  ensure_dir dir;
  let templates_dir = Filename.concat dir "templates" in
  ensure_dir templates_dir;
  let static_dir = Filename.concat dir "static" in
  ensure_dir static_dir;
  
  let meta_oc = open_out (Filename.concat dir "theme.txt") in
  output_string meta_oc "name: default\nversion: 1.0.0\ndescription: Default crt theme\n";
  close_out meta_oc;
  
  let base_oc = open_out (Filename.concat templates_dir "base.html") in
  output_string base_oc default_page_template;
  close_out base_oc;
  
  let page_oc = open_out (Filename.concat templates_dir "page.html") in
  output_string page_oc "{% extends \"base.html\" %}\n";
  close_out page_oc
