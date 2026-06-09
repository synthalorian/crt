(** Template engine with inheritance.

    Supports:
    - Variable substitution: [{{ variable }}]
    - Block definitions: [{% block name %}content{% endblock %}]
    - Template inheritance: [{% extends "base.html" %}]
    - Include partials: [{% include "partial.html" %}]
    - Conditionals: [{% if var %}...{% endif %}]
    - For loops: [{% for item in items %}...{% endfor %}]
*)

type node =
  | Text of string
  | Var of string
  | Block of string * node list
  | Include of string
  | Extends of string
  | If of string * node list * node list
  | For of string * string * node list

type t = node list

let strip_quotes s =
  let len = String.length s in
  if len >= 2 && ((s.[0] = '"' && s.[len - 1] = '"') || (s.[0] = '\'' && s.[len - 1] = '\'')) then
    String.sub s 1 (len - 2)
  else
    s

type context = {
  vars : (string, string) Hashtbl.t;
  lists : (string, string list) Hashtbl.t;
}

let create_context () = {
  vars = Hashtbl.create 16;
  lists = Hashtbl.create 8;
}

let set_var ctx key value = Hashtbl.replace ctx.vars key value
let get_var ctx key = try Some (Hashtbl.find ctx.vars key) with Not_found -> None

let set_list ctx key values = Hashtbl.replace ctx.lists key values
let get_list ctx key = try Some (Hashtbl.find ctx.lists key) with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* Parsing                                                                    *)
(* -------------------------------------------------------------------------- *)

type token =
  | TText of string
  | TVar of string
  | TBlockStart of string
  | TBlockEnd
  | TExtends of string
  | TInclude of string
  | TIf of string
  | TElse
  | TEndIf
  | TFor of string * string
  | TEndFor

let tokenize s =
  let len = String.length s in
  let tokens = ref [] in
  let buf = Buffer.create 256 in
  let add_text () =
    if Buffer.length buf > 0 then (
      tokens := TText (Buffer.contents buf) :: !tokens;
      Buffer.clear buf)
  in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && s.[!i] = '{' && s.[!i + 1] = '{' then (
      add_text ();
      (* variable: {{ name }} *)
      let j = ref (!i + 2) in
      while !j < len && (s.[!j] = ' ' || s.[!j] = '\t') do incr j done;
      let start = !j in
      while !j < len && not (s.[!j] = '}' && !j + 1 < len && s.[!j + 1] = '}') do
        incr j
      done;
      let var_name = String.trim (String.sub s start (!j - start)) in
      tokens := TVar var_name :: !tokens;
      i := !j + 2
    ) else if !i + 1 < len && s.[!i] = '{' && s.[!i + 1] = '%' then (
      add_text ();
      let j = ref (!i + 2) in
      while !j < len && (s.[!j] = ' ' || s.[!j] = '\t') do incr j done;
      let start = !j in
      while !j < len && not (s.[!j] = '%' && !j + 1 < len && s.[!j + 1] = '}') do
        incr j
      done;
      let tag_content = String.trim (String.sub s start (!j - start)) in
      let parts =
        let rec split i acc current =
          if i >= String.length tag_content then
            List.rev (if current = "" then acc else (String.trim current) :: acc)
          else if tag_content.[i] = ' ' || tag_content.[i] = '\t' then
            if current = "" then
              split (i + 1) acc ""
            else
              split (i + 1) (String.trim current :: acc) ""
          else
            split (i + 1) acc (current ^ String.make 1 tag_content.[i])
        in
        split 0 [] ""
      in
      (match parts with
      | "block" :: name :: _ -> tokens := TBlockStart name :: !tokens
      | ["endblock"] | ["endblock"; _] -> tokens := TBlockEnd :: !tokens
      | "extends" :: path :: _ -> tokens := TExtends (strip_quotes path) :: !tokens
      | "include" :: path :: _ -> tokens := TInclude (strip_quotes path) :: !tokens
      | "if" :: cond :: _ -> tokens := TIf cond :: !tokens
      | ["else"] -> tokens := TElse :: !tokens
      | ["endif"] -> tokens := TEndIf :: !tokens
      | "for" :: var_name :: "in" :: collection :: _ ->
          tokens := TFor (var_name, strip_quotes collection) :: !tokens
      | ["endfor"] -> tokens := TEndFor :: !tokens
      | _ -> ()
      );
      i := !j + 2
    ) else (
      Buffer.add_char buf s.[!i];
      incr i
    )
  done;
  add_text ();
  List.rev !tokens

let parse tokens =
  let rec parse_nodes ~stop_at acc tokens =
    match tokens with
    | [] -> List.rev acc, []
    | tok :: _rest when List.mem tok stop_at -> List.rev acc, tokens
    | TText s :: rest -> parse_nodes ~stop_at (Text s :: acc) rest
    | TVar name :: rest -> parse_nodes ~stop_at (Var name :: acc) rest
    | TBlockStart name :: rest ->
        let nodes, rest = parse_nodes ~stop_at:[TBlockEnd] [] rest in
        let rest =
          match rest with
          | TBlockEnd :: rest -> rest
          | _ -> rest
        in
        parse_nodes ~stop_at (Block (name, nodes) :: acc) rest
    | TInclude path :: rest -> parse_nodes ~stop_at (Include path :: acc) rest
    | TExtends path :: rest -> parse_nodes ~stop_at (Extends path :: acc) rest
    | TIf cond :: rest ->
        let then_nodes, rest = parse_nodes ~stop_at:[TElse; TEndIf] [] rest in
        let else_nodes, rest =
          match rest with
          | TElse :: rest ->
              let nodes, rest = parse_nodes ~stop_at:[TEndIf] [] rest in
              nodes, rest
          | _ -> [], rest
        in
        let rest =
          match rest with
          | TEndIf :: rest -> rest
          | _ -> rest
        in
        parse_nodes ~stop_at (If (cond, then_nodes, else_nodes) :: acc) rest
    | TFor (var_name, collection) :: rest ->
        let body_nodes, rest = parse_nodes ~stop_at:[TEndFor] [] rest in
        let rest =
          match rest with
          | TEndFor :: rest -> rest
          | _ -> rest
        in
        parse_nodes ~stop_at (For (var_name, collection, body_nodes) :: acc) rest
    | _ :: rest -> parse_nodes ~stop_at acc rest
  in
  let nodes, rest = parse_nodes ~stop_at:[] [] tokens in
  if rest <> [] then
    Printf.eprintf "[template] Warning: unclosed tags in template\n%!";
  nodes

let of_string s = parse (tokenize s)

(* -------------------------------------------------------------------------- *)
(* Rendering                                                                  *)
(* -------------------------------------------------------------------------- *)

type loader = string -> t

let rec render_node ctx loader buf = function
  | Text s -> Buffer.add_string buf s
  | Var name ->
      (match get_var ctx name with
      | Some value -> Buffer.add_string buf value
      | None -> Printf.eprintf "[template] Warning: undefined variable '%s'\n%!" name)
  | Block (_, nodes) ->
      List.iter (render_node ctx loader buf) nodes
  | Include path ->
      (try
        let tmpl = loader path in
        List.iter (render_node ctx loader buf) tmpl
      with exn ->
        Printf.eprintf "[template] Error including '%s': %s\n%!" path (Printexc.to_string exn))
  | Extends _path ->
      (* Extends is handled at the template level, not individual node *)
      ()
  | If (cond, then_nodes, else_nodes) ->
      (match get_var ctx cond with
      | Some "" | None ->
          List.iter (render_node ctx loader buf) else_nodes
      | Some value ->
          let is_truthy =
            match String.lowercase_ascii (String.trim value) with
            | "false" | "0" | "" -> false
            | _ -> true
          in
          if is_truthy then
            List.iter (render_node ctx loader buf) then_nodes
          else
            List.iter (render_node ctx loader buf) else_nodes)
  | For (var_name, collection, body_nodes) ->
      (match get_list ctx collection with
      | Some items ->
          List.iter (fun item ->
            let old_val = try Some (Hashtbl.find ctx.vars var_name) with Not_found -> None in
            Hashtbl.replace ctx.vars var_name item;
            List.iter (render_node ctx loader buf) body_nodes;
            match old_val with
            | Some v -> Hashtbl.replace ctx.vars var_name v
            | None -> Hashtbl.remove ctx.vars var_name
          ) items
      | None ->
          Printf.eprintf "[template] Warning: undefined list '%s'\n%!" collection)

let collect_blocks nodes =
  let rec collect acc = function
    | [] -> acc
    | Block (name, content) :: rest ->
        let acc = (name, content) :: acc in
        collect acc rest
    | _ :: rest -> collect acc rest
  in
  collect [] nodes

let override_blocks base_blocks child_blocks =
  let child_map =
    let tbl = Hashtbl.create (List.length child_blocks) in
    List.iter (fun (name, content) -> Hashtbl.replace tbl name content) child_blocks;
    tbl
  in
  let rec apply = function
    | [] -> []
    | Block (name, _base_content) :: rest when Hashtbl.mem child_map name ->
        let child_content = Hashtbl.find child_map name in
        Block (name, child_content) :: apply rest
    | node :: rest ->
        (match node with
        | If (cond, then_nodes, else_nodes) ->
            If (cond, apply then_nodes, apply else_nodes) :: apply rest
        | For (var, coll, body) ->
            For (var, coll, apply body) :: apply rest
        | _ -> node :: apply rest)
  in
  apply base_blocks

let render ?(loader = (fun _ -> [])) ctx nodes =
  let buf = Buffer.create 4096 in
  (* Handle template inheritance *)
  let nodes =
    match List.find_opt (function Extends _ -> true | _ -> false) nodes with
    | Some (Extends path) ->
        (try
          let base_nodes = loader path in
          let child_blocks = collect_blocks nodes in
          override_blocks base_nodes child_blocks
        with exn ->
          Printf.eprintf "[template] Error extending '%s': %s\n%!" path (Printexc.to_string exn);
          nodes)
    | _ -> nodes
  in
  List.iter (render_node ctx loader buf) nodes;
  Buffer.contents buf

let render_string ?loader ctx s = render ?loader ctx (of_string s)

(* -------------------------------------------------------------------------- *)
(* File I/O                                                                   *)
(* -------------------------------------------------------------------------- *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let load_file path = of_string (read_file path)
