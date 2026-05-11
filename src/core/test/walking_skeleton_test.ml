open Causalis_core

let price value =
  Value.Price.of_decimal (Decimal.of_int value)

let test_price_map_pipeline () =
  let close = Signal.Input.create ~name:"close" ~clock:Signal.Clock.logical () in
  let factor = Value.Price.of_decimal (Decimal.of_int 2) in
  let doubled = Signal.map (fun value -> Value.Price.(value * factor)) (Signal.input close) in
  let output = Compiler.Output.create ~name:"doubled" doubled in
  let artifact =
    match Compiler.compile ~outputs:[Compiler.Output.pack output] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "compilation failed"
  in
  let inputs = Runtime.Input_trace.of_values close [price 1; price 2; price 3; price 4] in
  let run =
    match Runtime.Reference_exec.run artifact inputs with
    | Ok run -> run
    | Error _ -> Alcotest.fail "execution failed"
  in
  let actual = List.map Value.Price.to_string (Runtime.values output run) in
  Alcotest.(check (list string)) "doubled prices" ["2"; "4"; "6"; "8"] actual;
  let second_run =
    match Runtime.Reference_exec.run artifact inputs with
    | Ok run -> run
    | Error _ -> Alcotest.fail "artifact reuse failed"
  in
  Alcotest.(check (list string)) "reused artifact" actual
    (List.map Value.Price.to_string (Runtime.values output second_run))

let () = Alcotest.run "walking_skeleton" ["stage 1", [Alcotest.test_case "price map" `Quick test_price_map_pipeline]]
