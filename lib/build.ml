(** Build engine for the static site generator.

    Orchestrates the pipeline: find markdown files, process through
    transforms (including dynamically loaded plugins), and write HTML output.
*)

let ensure_dir path =
  if not (Sys.file_exists path && Sys.is_directory path) then
    Sys.mkdir path 0o755

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let is_markdown path =
  Filename.check_suffix path ".md" ||
  Filename.check_suffix path ".markdown"

let rec collect_markdown acc dir =
  try
    let entries = Sys.readdir dir in
    Array.fold_left (fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then
        if entry <> "_build" && entry <> ".git" && entry <> "node_modules" then
          collect_markdown acc path
        else
          acc
      else if is_markdown path then
        path :: acc
      else
        acc
    ) acc entries
  with _ -> acc

let relative_path base path =
  let base_len = String.length base in
  let path_len = String.length path in
  if path_len > base_len && String.sub path 0 base_len = base then
    let start = if path.[base_len] = '/' then base_len + 1 else base_len in
    String.sub path start (path_len - start)
  else
    path

let output_path_for ~input_dir ~output_dir input_path =
  let rel = relative_path input_dir input_path in
  let html_name =
    let base = Filename.basename rel in
    if Filename.check_suffix base ".md" then
      Filename.chop_suffix base ".md" ^ ".html"
    else if Filename.check_suffix base ".markdown" then
      Filename.chop_suffix base ".markdown" ^ ".html"
    else
      base ^ ".html"
  in
  let dir_part = Filename.dirname rel in
  let out_dir = if dir_part = "." then output_dir else Filename.concat output_dir dir_part in
  ensure_dir out_dir;
  Filename.concat out_dir html_name

let get_mtime path =
  try
    let stats = Unix.stat path in
    stats.Unix.st_mtime
  with _ -> 0.0

let build_file ~cache ~input_path ~output_path ?(dev_mode = false) () =
  try
    let mtime = get_mtime input_path in
    let cache_key = "build:" ^ input_path in
    (* Check if we can use the cached output *)
    match Cache.get cache ~key:cache_key ~mtime with
    | Some cached_html ->
        write_file output_path cached_html;
        Printf.printf "  [build] %s -> %s (cached)\n%!" input_path output_path;
        Ok ()
    | None ->
        let content = read_file input_path in
        (* Parse markdown *)
        let doc = Transform.parse content in
        (* Apply post-parse plugin hooks *)
        let doc = Plugin.apply_doc_hooks ~stage:Post_parse doc in
        (* Built-in transforms *)
        let doc = Transform.toc doc in
        let doc = Transform.highlight doc in
        (* Apply plugin transforms *)
        let doc = Plugin.apply_transforms doc in
        (* Apply pre-render plugin hooks *)
        let doc = Plugin.apply_doc_hooks ~stage:Pre_render doc in
        let title =
          let rec find_h1 = function
            | [] -> None
            | Ast.Heading { level = 1; content } :: _ ->
                let rec text_of_inline = function
                  | Ast.Text s -> s
                  | Ast.Bold children | Ast.Italic children ->
                      String.concat "" (List.map text_of_inline children)
                  | Ast.Code s -> s
                  | Ast.Link { text; _ } -> String.concat "" (List.map text_of_inline text)
                  | Ast.Break -> " "
                in
                Some (String.concat "" (List.map text_of_inline content))
            | _ :: rest -> find_h1 rest
          in
          match find_h1 doc with
          | Some t -> t
          | None -> Filename.basename input_path
        in
        let html = Renderer.render_page ~title ~dev_mode doc in
        (* Apply post-render HTML hooks *)
        let html = Plugin.apply_html_hooks html in
        write_file output_path html;
        Cache.set cache ~key:cache_key ~mtime html;
        Printf.printf "  [build] %s -> %s\n%!" input_path output_path;
        Ok ()
  with exn ->
    Error (Printexc.to_string exn)

let load_plugins ?plugin_dir () =
  match plugin_dir with
  | None -> ()
  | Some dir ->
      if Sys.file_exists dir && Sys.is_directory dir then
        match Plugin.load_dir dir with
        | Ok count ->
            if count > 0 then
              Printf.printf "[build] Loaded %d plugin(s) from %s\n%!" count dir
        | Error msg ->
            Printf.eprintf "[build] Plugin loading error: %s\n%!" msg
      else
        Printf.printf "[build] Plugin directory not found: %s (skipping)\n%!" dir

let build ~input_dir ~output_dir ?(dev_mode = false) ?(cache_dir = ".crt_cache") ?plugin_dir () =
  Printf.printf "[build] Building from %s to %s\n%!" input_dir output_dir;
  ensure_dir output_dir;
  (* Load plugins before building *)
  load_plugins ?plugin_dir ();
  let cache = Cache.create ~dir:cache_dir in
  let files = collect_markdown [] input_dir in
  if files = [] then
    Printf.printf "[build] No markdown files found in %s\n%!" input_dir
  else
    Printf.printf "[build] Found %d markdown file(s)\n%!" (List.length files);
  let results = List.map (fun input_path ->
    let output_path = output_path_for ~input_dir ~output_dir input_path in
    build_file ~cache ~input_path ~output_path ~dev_mode ()
  ) files in
  let errors = List.filter_map (function Error e -> Some e | Ok () -> None) results in
  match errors with
  | [] ->
      Printf.printf "[build] Build complete.\n%!";
      Ok ()
  | _ ->
      Printf.printf "[build] Build completed with %d error(s).\n%!" (List.length errors);
      Error (String.concat "\n" errors)

let build_with_watch ~input_dir ~output_dir ~delay ?(dev_mode = false) ?(cache_dir = ".crt_cache") ?plugin_dir ?(on_build_complete = fun () -> ()) () =
  let result = build ~input_dir ~output_dir ~dev_mode ~cache_dir ?plugin_dir () in
  (match result with
  | Ok () -> on_build_complete ()
  | Error e -> Printf.printf "[build] Initial build failed: %s\n%!" e);

  let should_stop = ref false in
  let handle_signal _ = should_stop := true in
  Sys.set_signal Sys.sigint (Signal_handle handle_signal);
  Sys.set_signal Sys.sigterm (Signal_handle handle_signal);

  Watcher.watch
    ~dir:input_dir
    ~delay
    ~on_change:(fun _changed_files ->
      Printf.printf "[build] Rebuilding...\n%!";
      match build ~input_dir ~output_dir ~dev_mode ~cache_dir ?plugin_dir () with
      | Ok () -> on_build_complete ()
      | Error e -> Printf.printf "[build] Rebuild failed: %s\n%!" e
    )
    ~should_stop:(fun () -> !should_stop);

  Ok ()

let serve ~input_dir ~output_dir ~port ~delay ?(cache_dir = ".crt_cache") ?plugin_dir () =
  let server = Server.create ~port () in
  let should_stop = ref false in
  let handle_signal _ = should_stop := true; Server.stop server in
  Sys.set_signal Sys.sigint (Signal_handle handle_signal);
  Sys.set_signal Sys.sigterm (Signal_handle handle_signal);

  (* Run server in a background thread *)
  let server_thread = Thread.create (fun () ->
    Server.run server ~doc_root:output_dir ()
  ) () in

  (* Give server time to start *)
  Thread.delay 0.1;

  (* Start build + watch with reload broadcasting *)
  let result = build_with_watch
    ~input_dir
    ~output_dir
    ~delay
    ~dev_mode:true
    ~cache_dir
    ?plugin_dir
    ~on_build_complete:(fun () -> Server.broadcast_reload server)
    ()
  in

  (match result with
  | Ok () -> ()
  | Error e -> Printf.eprintf "Error: %s\n" e);

  Server.stop server;
  Thread.join server_thread;
  Ok ()
