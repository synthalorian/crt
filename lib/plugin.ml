(** Plugin system implementation.

    Maintains a global registry of plugins loaded dynamically
    via Dynlink. Each plugin provides named transforms and
    stage-specific hooks that integrate into the build pipeline.
*)

open Ast

type stage =
  | Post_parse
  | Pre_render

type transform = string * (doc -> doc)

type doc_hook = stage * (doc -> doc)

type html_hook = string -> string

type t = {
  name : string;
  version : string;
  transforms : transform list;
  doc_hooks : doc_hook list;
  html_hooks : html_hook list;
}

let create ~name ~version ?(transforms = []) ?(doc_hooks = []) ?(html_hooks = []) () =
  { name; version; transforms; doc_hooks; html_hooks }

(* Global plugin registry *)
let _registry : t list ref = ref []

let registry () = !_registry

let register plugin =
  _registry := plugin :: !_registry;
  Printf.printf "[plugin] Registered: %s v%s (%d transforms, %d doc_hooks, %d html_hooks)\n%!"
    plugin.name plugin.version
    (List.length plugin.transforms)
    (List.length plugin.doc_hooks)
    (List.length plugin.html_hooks)

let clear_registry () =
  _registry := []

(* -------------------------------------------------------------------------- *)
(* Dynamic loading via Dynlink                                                *)
(* -------------------------------------------------------------------------- *)

let load_cmxs path =
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "Plugin file not found: %s" path)
  else
    try
      Dynlink.loadfile path;
      Ok ()
    with
    | Dynlink.Error e ->
        Error (Printf.sprintf "Dynlink error loading %s: %s"
                 path (Dynlink.error_message e))
    | exn ->
        Error (Printf.sprintf "Error loading plugin %s: %s"
                 path (Printexc.to_string exn))

let load_dir dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    Error (Printf.sprintf "Plugin directory not found: %s" dir)
  else
    try
      let entries = Sys.readdir dir in
      let cmxs_files =
        Array.to_list entries
        |> List.filter (fun f -> Filename.check_suffix f ".cmxs")
        |> List.sort String.compare
      in
      let count = ref 0 in
      List.iter (fun f ->
        let path = Filename.concat dir f in
        match load_cmxs path with
        | Ok () -> incr count
        | Error msg -> Printf.eprintf "[plugin] %s\n%!" msg
      ) cmxs_files;
      Ok !count
    with exn ->
      Error (Printf.sprintf "Error scanning plugin directory %s: %s"
               dir (Printexc.to_string exn))

(* -------------------------------------------------------------------------- *)
(* Applying plugins                                                           *)
(* -------------------------------------------------------------------------- *)

let apply_doc_hooks ~stage doc =
  List.fold_left (fun acc plugin ->
    List.fold_left (fun doc' (hook_stage, fn) ->
      if hook_stage = stage then fn doc' else doc'
    ) acc plugin.doc_hooks
  ) doc (List.rev !_registry)

let apply_html_hooks html =
  List.fold_left (fun acc plugin ->
    List.fold_left (fun html' fn -> fn html') acc plugin.html_hooks
  ) html (List.rev !_registry)

let apply_transforms doc =
  List.fold_left (fun acc plugin ->
    List.fold_left (fun doc' (_, fn) -> fn doc') acc plugin.transforms
  ) doc (List.rev !_registry)

let transform_names () =
  List.flatten (List.map (fun p -> List.map fst p.transforms) !_registry)

let plugin_names () =
  List.map (fun p -> p.name) !_registry
