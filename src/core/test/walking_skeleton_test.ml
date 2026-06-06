open Causalis_core

let price value =
  Value.Price.of_decimal (Decimal.of_int value)

let price_strings values =
  List.map (Option.map Value.Price.to_string) values

let int_strings values =
  List.map (Option.map string_of_int) values

let run_output output inputs =
  let artifact =
    match Compiler.compile ~outputs:[Compiler.Output.pack output] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "compilation failed"
  in
  match Runtime.Reference_exec.run artifact inputs with
  | Ok run -> run
  | Error _ -> Alcotest.fail "execution failed"

let test_price_map_pipeline () =
  let close = Signal.Input.create ~name:"close" ~clock:Signal.Clock.logical () in
  let factor = Value.Price.of_decimal (Decimal.of_int 2) in
  let doubled = Signal.map (fun value -> Value.Price.(value * factor)) (Signal.input close) in
  let output = Compiler.Output.create ~name:"doubled" doubled in
  let inputs = Runtime.Input_trace.of_values close [price 1; price 2; price 3; price 4] in
  let run = run_output output inputs in
  let actual = price_strings (Runtime.values output run) in
  Alcotest.(check (list (option string))) "doubled prices"
    [Some "2"; Some "4"; Some "6"; Some "8"] actual;
  let second_run =
    let artifact =
      match Compiler.compile ~outputs:[Compiler.Output.pack output] with
      | Ok artifact -> artifact
      | Error _ -> Alcotest.fail "compilation failed"
    in
    match Runtime.Reference_exec.run artifact inputs with
    | Ok run -> run
    | Error _ -> Alcotest.fail "artifact reuse failed"
  in
  Alcotest.(check (list (option string))) "reused artifact" actual
    (price_strings (Runtime.values output second_run))

let test_pre_pipeline () =
  let close = Signal.Input.create ~name:"close" ~clock:Signal.Clock.logical () in
  let output = Compiler.Output.create ~name:"previous" (Signal.pre (Signal.input close)) in
  let inputs = Runtime.Input_trace.of_values close [price 1; price 2; price 3; price 4] in
  let actual = price_strings (Runtime.values output (run_output output inputs)) in
  Alcotest.(check (list (option string))) "previous prices"
    [None; Some "1"; Some "2"; Some "3"] actual

let test_init_pipeline () =
  let close = Signal.Input.create ~name:"close" ~clock:Signal.Clock.logical () in
  let previous = Signal.pre (Signal.input close) in
  let output = Compiler.Output.create ~name:"initialized" (Signal.init (price 0) previous) in
  let inputs = Runtime.Input_trace.of_values close [price 1; price 2; price 3; price 4] in
  let actual = price_strings (Runtime.values output (run_output output inputs)) in
  Alcotest.(check (list (option string))) "initialized prices"
    [Some "0"; Some "1"; Some "2"; Some "3"] actual

let test_scan_pipeline () =
  let input = Signal.Input.create ~name:"value" ~clock:Signal.Clock.logical () in
  let scanned =
    Signal.scan ~init:0
      ~step:(fun state value ->
        let next_state = state + value in
        next_state, state)
      (Signal.input input)
  in
  let output = Compiler.Output.create ~name:"state_before_input" scanned in
  let inputs = Runtime.Input_trace.of_values input [1; 2; 3; 4] in
  let artifact =
    match Compiler.compile ~outputs:[Compiler.Output.pack output] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "compilation failed"
  in
  let run_once () =
    match Runtime.Reference_exec.run artifact inputs with
    | Ok run -> int_strings (Runtime.values output run)
    | Error _ -> Alcotest.fail "execution failed"
  in
  let actual = run_once () in
  Alcotest.(check (list (option string))) "scan outputs"
    [Some "0"; Some "1"; Some "3"; Some "6"] actual;
  Alcotest.(check (list (option string))) "scan state is per run" actual (run_once ())

let test_compiled_plan () =
  let input = Signal.Input.create ~name:"anchor" ~clock:Signal.Clock.logical () in
  let source = Signal.input input in
  let output = Compiler.Output.create ~name:"mapped" (Signal.map succ source) in
  let artifact =
    match Compiler.compile ~outputs:[Compiler.Output.pack output] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "compilation failed"
  in
  let schedule =
    List.map Signal.Internal.id (Compiler.Artifact.schedule artifact)
  in
  let repeated_artifact =
    match Compiler.compile ~outputs:[Compiler.Output.pack output] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "repeated compilation failed"
  in
  let repeated_schedule =
    List.map Signal.Internal.id (Compiler.Artifact.schedule repeated_artifact)
  in
  Alcotest.(check (list int)) "deterministic schedule" schedule repeated_schedule;
  Alcotest.(check int) "one dependency" 1
    (List.length (Compiler.Artifact.dependencies artifact));
  Alcotest.(check int) "no state slots" 0
    (List.length (Compiler.Artifact.state_layout artifact))

let test_delayed_feedback () =
  let anchor = Signal.Input.create ~name:"anchor" ~clock:Signal.Clock.logical () in
  let feedback =
    Signal.feedback ~clock:Signal.Clock.logical (fun previous ->
        Signal.map (fun value -> value + 1)
          (Signal.init 0 (Signal.pre previous)))
  in
  let output = Compiler.Output.create ~name:"feedback" feedback in
  let artifact =
    match Compiler.compile ~outputs:[Compiler.Output.pack output] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "delayed feedback should compile"
  in
  let inputs = Runtime.Input_trace.of_values anchor [0; 0; 0; 0] in
  let run =
    match Runtime.Reference_exec.run artifact inputs with
    | Ok run -> run
    | Error _ -> Alcotest.fail "delayed feedback should execute"
  in
  Alcotest.(check (list (option string))) "delayed feedback values"
    [Some "1"; Some "2"; Some "3"; Some "4"]
    (int_strings (Runtime.values output run));
  Alcotest.(check int) "feedback has one delay slot" 1
    (List.length (Compiler.Artifact.state_layout artifact))

let test_instantaneous_feedback_rejected () =
  let feedback =
    Signal.feedback ~clock:Signal.Clock.logical (fun previous ->
        Signal.map (fun value -> value + 1) previous)
  in
  let output = Compiler.Output.create ~name:"invalid_feedback" feedback in
  match Compiler.compile ~outputs:[Compiler.Output.pack output] with
  | Error (Compiler.Instantaneous_cycle _) -> ()
  | Error _ -> Alcotest.fail "wrong compiler error for instantaneous feedback"
  | Ok _ -> Alcotest.fail "instantaneous feedback should be rejected"

let () =
  Alcotest.run "walking_skeleton"
    [ "signal construction",
      [ Alcotest.test_case "price map" `Quick test_price_map_pipeline ];
      "temporal semantics",
      [ Alcotest.test_case "pre" `Quick test_pre_pipeline;
        Alcotest.test_case "init" `Quick test_init_pipeline;
        Alcotest.test_case "scan" `Quick test_scan_pipeline ];
      "compiled semantic machine",
      [ Alcotest.test_case "compiled plan" `Quick test_compiled_plan;
        Alcotest.test_case "delayed feedback" `Quick test_delayed_feedback;
        Alcotest.test_case "instantaneous feedback" `Quick test_instantaneous_feedback_rejected ] ]
