open Crt_lib

let pass_count = ref 0
let fail_count = ref 0

let assert_eq name expected actual =
  if expected = actual then (
    incr pass_count;
    Printf.printf "  PASS: %s\n%!" name)
  else (
    incr fail_count;
    Printf.printf "  FAIL: %s\n%!" name;
    Printf.printf "    expected: %s\n%!"
      (String.escaped (Marshal.to_string expected []));
    Printf.printf "    actual:   %s\n%!"
      (String.escaped (Marshal.to_string actual [])))

let test_pure () =
  let p = Pipeline.pure 42 in
  match Pipeline.run p with
  | Ok 42 -> assert_eq "pure" true true
  | _ -> assert_eq "pure" true false

let test_return () =
  let p = Pipeline.return "hello" in
  match Pipeline.run p with
  | Ok "hello" -> assert_eq "return" true true
  | _ -> assert_eq "return" true false

let test_arr () =
  let p = Pipeline.pure 10 |> Pipeline.arr (fun x -> x * 2) in
  match Pipeline.run p with
  | Ok 20 -> assert_eq "arr" true true
  | _ -> assert_eq "arr" true false

let test_map () =
  let p = Pipeline.pure "hello" |> Pipeline.map String.length in
  match Pipeline.run p with
  | Ok 5 -> assert_eq "map" true true
  | _ -> assert_eq "map" true false

let test_named_arr () =
  let p = Pipeline.pure 7 |> Pipeline.arr ~name:"inc" (fun x -> x + 1) in
  match Pipeline.run p with
  | Ok 8 -> assert_eq "named_arr" true true
  | _ -> assert_eq "named_arr" true false

let test_node () =
  let p = Pipeline.pure 5 |> Pipeline.node "double" (fun x -> x * 2) in
  match Pipeline.run p with
  | Ok 10 -> assert_eq "node" true true
  | _ -> assert_eq "node" true false

let test_bind () =
  let p = Pipeline.(>>=) (Pipeline.pure 10) (fun x -> Pipeline.pure (x * 2)) in
  match Pipeline.run p with
  | Ok 20 -> assert_eq "bind" true true
  | _ -> assert_eq "bind" true false

let test_bind_named () =
  let p = Pipeline.bind ~name:"expand" (Pipeline.pure 3)
    (fun x -> Pipeline.pure (List.init x Fun.id))
  in
  match Pipeline.run p with
  | Ok [0; 1; 2] -> assert_eq "bind_named" true true
  | _ -> assert_eq "bind_named" true false

let test_infix_map () =
  let p = Pipeline.(>>|) (Pipeline.pure 5) (fun x -> x + 3) in
  match Pipeline.run p with
  | Ok 8 -> assert_eq "infix_map" true true
  | _ -> assert_eq "infix_map" true false

let test_infix_bind () =
  let p = Pipeline.(>>=) (Pipeline.pure 4) (fun x -> Pipeline.pure (x * x)) in
  match Pipeline.run p with
  | Ok 16 -> assert_eq "infix_bind" true true
  | _ -> assert_eq "infix_bind" true false

let test_pair () =
  let p1 = Pipeline.pure 1 in
  let p2 = Pipeline.pure 2 in
  let p = Pipeline.pair p1 p2 in
  match Pipeline.run p with
  | Ok (1, 2) -> assert_eq "pair" true true
  | _ -> assert_eq "pair" true false

let test_both () =
  let p1 = Pipeline.pure "hello" in
  let p2 = Pipeline.pure 42 in
  let p = Pipeline.both p1 p2 in
  match Pipeline.run p with
  | Ok ("hello", 42) -> assert_eq "both" true true
  | _ -> assert_eq "both" true false

let test_pair_with_transforms () =
  let p1 = Pipeline.pure 10 |> Pipeline.arr (fun x -> x * 2) in
  let p2 = Pipeline.pure 10 |> Pipeline.arr (fun x -> x + 5) in
  let p = Pipeline.pair p1 p2 in
  match Pipeline.run p with
  | Ok (20, 15) -> assert_eq "pair_with_transforms" true true
  | _ -> assert_eq "pair_with_transforms" true false

let test_complex_dag () =
  (* Build a DAG:
     Input = 10
     Branch 1: double -> 20
     Branch 2: triple -> 30
     Join: add -> 50
  *)
  let input = Pipeline.pure 10 in
  let branch1 = Pipeline.arr (fun x -> x * 2) input in
  let branch2 = Pipeline.arr (fun x -> x * 3) input in
  let joined = Pipeline.pair branch1 branch2 in
  let result = Pipeline.arr (fun (a, b) -> a + b) joined in
  match Pipeline.run result with
  | Ok 50 -> assert_eq "complex_dag" true true
  | _ -> assert_eq "complex_dag" true false

let test_sequential_chain () =
  let p = Pipeline.pure 1
    |> Pipeline.arr (fun x -> x + 1)
    |> Pipeline.arr (fun x -> x * 3)
    |> Pipeline.arr (fun x -> x - 1)
  in
  match Pipeline.run p with
  | Ok 5 -> assert_eq "sequential_chain" true true
  | _ -> assert_eq "sequential_chain" true false

let test_bind_chain () =
  let p =
    Pipeline.bind (Pipeline.pure 2) (fun x ->
    Pipeline.bind (Pipeline.pure (x + 1)) (fun y ->
    Pipeline.bind (Pipeline.pure (y * 2)) (fun z ->
    Pipeline.pure (z - 3))))
  in
  match Pipeline.run p with
  | Ok 3 -> assert_eq "bind_chain" true true
  | _ -> assert_eq "bind_chain" true false

let test_error_handling () =
  let p = Pipeline.pure 10
    |> Pipeline.arr (fun _ -> failwith "intentional error")
  in
  match Pipeline.run p with
  | Error (Failure msg) when String.contains msg 'i' -> assert_eq "error_handling" true true
  | _ -> assert_eq "error_handling" true false

let test_error_in_bind () =
  let p = Pipeline.bind (Pipeline.pure 10)
    (fun _ -> failwith "bind error")
  in
  match Pipeline.run p with
  | Error (Failure msg) when String.contains msg 'b' -> assert_eq "error_in_bind" true true
  | _ -> assert_eq "error_in_bind" true false

let test_error_in_pair_left () =
  let p1 = Pipeline.pure 10 |> Pipeline.arr (fun _ -> failwith "left") in
  let p2 = Pipeline.pure 20 in
  let p = Pipeline.pair p1 p2 in
  match Pipeline.run p with
  | Error (Failure msg) when String.contains msg 'l' -> assert_eq "error_in_pair_left" true true
  | _ -> assert_eq "error_in_pair_left" true false

let test_error_in_pair_right () =
  let p1 = Pipeline.pure 10 in
  let p2 = Pipeline.pure 20 |> Pipeline.arr (fun _ -> failwith "right") in
  let p = Pipeline.pair p1 p2 in
  match Pipeline.run p with
  | Error (Failure msg) when String.contains msg 'r' -> assert_eq "error_in_pair_right" true true
  | _ -> assert_eq "error_in_pair_right" true false

let test_trace () =
  let trace_val = ref None in
  let p = Pipeline.pure 42
    |> Pipeline.trace (fun x -> trace_val := Some x)
  in
  match Pipeline.run p with
  | Ok 42 when !trace_val = Some 42 -> assert_eq "trace" true true
  | _ -> assert_eq "trace" true false

let test_trace_preserves_value () =
  let p = Pipeline.pure "hello"
    |> Pipeline.trace (fun _ -> ())
    |> Pipeline.arr (fun s -> s ^ " world")
  in
  match Pipeline.run p with
  | Ok "hello world" -> assert_eq "trace_preserves_value" true true
  | _ -> assert_eq "trace_preserves_value" true false

let test_to_string_pure () =
  let s = Pipeline.to_string (Pipeline.pure 42) in
  assert_eq "to_string_pure" "Pure" s

let test_to_string_map () =
  let s = Pipeline.to_string (Pipeline.pure 42 |> Pipeline.arr ~name:"inc" (fun x -> x + 1)) in
  assert_eq "to_string_map" "Map(inc, Pure)" s

let test_to_string_pair () =
  let s = Pipeline.to_string (Pipeline.pair (Pipeline.pure 1) (Pipeline.pure 2)) in
  assert_eq "to_string_pair" "Pair(Pure, Pure)" s

let test_to_string_bind () =
  let s = Pipeline.to_string (Pipeline.bind ~name:"expand" (Pipeline.pure 10) (fun _ -> Pipeline.pure [])) in
  assert_eq "to_string_bind" "Bind(expand, Pure, _)" s

let test_nested_dag () =
  (* Three-way fan-out from a single input, then merge:
     Input = 5
     A = input * 2 = 10
     B = input + 3 = 8
     C = input - 1 = 4
     Pair(A, B) -> (10, 8)
     Pair((10, 8), C) -> ((10, 8), 4)
     Final: sum all = 22
  *)
  let input = Pipeline.pure 5 in
  let a = Pipeline.arr (fun x -> x * 2) input in
  let b = Pipeline.arr (fun x -> x + 3) input in
  let c = Pipeline.arr (fun x -> x - 1) input in
  let ab = Pipeline.pair a b in
  let abc = Pipeline.pair ab c in
  let result = Pipeline.arr (fun ((x, y), z) -> x + y + z) abc in
  match Pipeline.run result with
  | Ok 22 -> assert_eq "nested_dag" true true
  | _ -> assert_eq "nested_dag" true false

let test_composition_with_ast () =
  (* Integration: pipeline works with Phase 1 AST types *)
  let doc = Ast.[Paragraph [Text "hello"]; Heading { level = 1; content = [Text "world"] }] in
  let p = Pipeline.pure doc
    |> Pipeline.arr (fun d -> List.length d)
  in
  match Pipeline.run p with
  | Ok 2 -> assert_eq "composition_with_ast" true true
  | _ -> assert_eq "composition_with_ast" true false

let run () =
  Printf.printf "Running pipeline tests...\n%!";
  test_pure ();
  test_return ();
  test_arr ();
  test_map ();
  test_named_arr ();
  test_node ();
  test_bind ();
  test_bind_named ();
  test_infix_map ();
  test_infix_bind ();
  test_pair ();
  test_both ();
  test_pair_with_transforms ();
  test_complex_dag ();
  test_sequential_chain ();
  test_bind_chain ();
  test_error_handling ();
  test_error_in_bind ();
  test_error_in_pair_left ();
  test_error_in_pair_right ();
  test_trace ();
  test_trace_preserves_value ();
  test_to_string_pure ();
  test_to_string_map ();
  test_to_string_pair ();
  test_to_string_bind ();
  test_nested_dag ();
  test_composition_with_ast ();
  Printf.printf "\nResults: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1

let () = run ()
