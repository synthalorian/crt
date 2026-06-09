(** CLI entry point for crt static site generator.

    Usage:
      crt build [-i DIR] [-o DIR]
      crt build --watch [-i DIR] [-o DIR] [--delay SECONDS]
      crt serve [-i DIR] [-o DIR] [--port PORT] [--delay SECONDS]
*)

open Crt_lib

let print_usage () =
  print_endline "crt -- static site generator";
  print_endline "";
  print_endline "Usage:";
  print_endline "  crt build [options]     Build the static site";
  print_endline "  crt serve [options]     Build, watch, and serve with hot reload";
  print_endline "  crt --help              Show this help message";
  print_endline "";
  print_endline "Build options:";
  print_endline "  -i, --input DIR         Input directory (default: content)";
  print_endline "  -o, --output DIR        Output directory (default: _site)";
  print_endline "  -w, --watch             Watch for changes and rebuild";
  print_endline "  --delay SECONDS         Polling delay when watching (default: 1.0)";
  print_endline "";
  print_endline "Serve options:";
  print_endline "  --port PORT             Server port (default: 8080)";
  print_endline "";
  print_endline "Examples:";
  print_endline "  crt build -i content -o _site";
  print_endline "  crt build --watch -i content -o _site";
  print_endline "  crt serve -i content -o _site --port 3000"

let parse_args args =
  let rec loop acc = function
    | [] -> List.rev acc
    | "-i" :: dir :: rest | "--input" :: dir :: rest ->
        loop ((`Input dir) :: acc) rest
    | "-o" :: dir :: rest | "--output" :: dir :: rest ->
        loop ((`Output dir) :: acc) rest
    | "-w" :: rest | "--watch" :: rest ->
        loop ((`Watch) :: acc) rest
    | "--delay" :: secs :: rest ->
        loop ((`Delay (float_of_string secs)) :: acc) rest
    | "--port" :: port :: rest ->
        loop ((`Port (int_of_string port)) :: acc) rest
    | "--help" :: _ | "-h" :: _ ->
        print_usage ();
        exit 0
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
        Printf.eprintf "Unknown option: %s\n" arg;
        print_usage ();
        exit 1
    | arg :: rest ->
        loop ((`Pos arg) :: acc) rest
  in
  loop [] args

let run_build args =
  let options = parse_args args in
  let input_dir = ref "content" in
  let output_dir = ref "_site" in
  let watch = ref false in
  let delay = ref 1.0 in
  
  List.iter (function
    | `Input dir -> input_dir := dir
    | `Output dir -> output_dir := dir
    | `Watch -> watch := true
    | `Delay secs -> delay := secs
    | `Port _ -> ()
    | `Pos _ -> ()
  ) options;
  
  if !watch then
    match Build.build_with_watch ~input_dir:!input_dir ~output_dir:!output_dir ~delay:!delay () with
    | Ok () -> ()
    | Error e -> (Printf.eprintf "Error: %s\n" e; exit 1)
  else
    match Build.build ~input_dir:!input_dir ~output_dir:!output_dir () with
    | Ok () -> ()
    | Error e -> (Printf.eprintf "Error: %s\n" e; exit 1)

let run_serve args =
  let options = parse_args args in
  let input_dir = ref "content" in
  let output_dir = ref "_site" in
  let port = ref 8080 in
  let delay = ref 1.0 in
  
  List.iter (function
    | `Input dir -> input_dir := dir
    | `Output dir -> output_dir := dir
    | `Port p -> port := p
    | `Delay secs -> delay := secs
    | `Watch -> ()
    | `Pos _ -> ()
  ) options;
  
  match Build.serve ~input_dir:!input_dir ~output_dir:!output_dir ~port:!port ~delay:!delay () with
  | Ok () -> ()
  | Error e -> (Printf.eprintf "Error: %s\n" e; exit 1)

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  match args with
  | "build" :: build_args -> run_build build_args
  | "serve" :: serve_args -> run_serve serve_args
  | [] -> print_usage ()
  | _ ->
      Printf.eprintf "Unknown command. Use --help for usage.\n";
      exit 1
