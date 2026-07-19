open Causalis_core

module Domain = Causalis_strategy.Domain

let price value = Value.Price.of_decimal (Decimal.of_int value)

let weight_of_percent value =
  match Domain.Weight.of_decimal Decimal.(of_int value / of_int 100) with
  | Ok weight -> weight
  | Error message -> Alcotest.fail message

let output_trace output input values =
  let artifact =
    match Compiler.compile ~outputs:[ Compiler.Output.pack output ] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "strategy compilation failed"
  in
  let input_trace = Runtime.Input_trace.of_values input values in
  let run =
    match Runtime.Reference_exec.run artifact input_trace with
    | Ok run -> run
    | Error _ -> Alcotest.fail "strategy execution failed"
  in
  Runtime.values output run

let weights values =
  List.map (Option.map Domain.Weight.to_string) values

let test_parser_error_location () =
  let source = "strategy Broken {\n  output exposure : weight = ;\n}" in
  match Causalis_syntax.Strategy_parser.parse source with
  | _ -> Alcotest.fail "invalid strategy unexpectedly parsed"
  | exception Causalis_syntax.Strategy_parser.Parse_error error ->
      Alcotest.(check int) "error line" 2 error.line;
      Alcotest.(check bool) "message identifies expected expression" true
        (String.length error.message > 0)

let test_ppx_matches_ocaml () =
  let prices = List.map price [ 10; 10; 10; 11; 12; 11; 10; 9; 10 ] in
  let generated_input = Threshold_generated.EmaTrend.input_close in
  Alcotest.(check (list (pair string int))) "declared input metadata"
    [ ("close", Signal.Input.id generated_input) ]
    Threshold_generated.EmaTrend.input_ports;
  Alcotest.(check (list string)) "declared output metadata" [ "exposure" ]
    Threshold_generated.EmaTrend.output_names;
  let generated_values =
    output_trace Threshold_generated.EmaTrend.output_exposure generated_input prices
  in
  let source = Signal.input generated_input in
  let fast = Signal.ema 2 source in
  let slow = Signal.ema 4 source in
  let rising =
    Signal.map2
      (fun fast slow ->
        Decimal.(Value.Price.to_decimal fast > Value.Price.to_decimal slow))
      fast slow
  in
  let direct_output = Compiler.Output.create ~name:"exposure"
      (Signal.select rising
         (Signal.const ~clock:Signal.Clock.logical (weight_of_percent 100))
         (Signal.const ~clock:Signal.Clock.logical (weight_of_percent (-50))))
  in
  let direct_values = output_trace direct_output generated_input prices in
  Alcotest.(check (list (option string))) "frontend/direct OCaml equivalence"
    (weights direct_values) (weights generated_values);
  Alcotest.(check bool) "trend strategy emits long exposure" true
    (List.mem (Some "1") (weights generated_values));
  Alcotest.(check bool) "trend strategy emits short exposure" true
    (List.mem (Some "-0.5") (weights generated_values))

let with_csv rows callback =
  let path = Filename.temp_file "causalis-strategy" ".csv" in
  let channel = open_out_bin path in
  output_string channel
    ("timestamp,open,high,low,close,volume,open_interest\n" ^ rows);
  close_out channel;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> callback path)

let test_backtest_binds_instrument_outside_strategy () =
  let source_rows =
    "2025-01-01T09:15:00+05:30,10,10,10,10,100,1\n"
    ^ "2025-01-01T09:16:00+05:30,10,10,10,10,100,1\n"
    ^ "2025-01-01T09:17:00+05:30,10,10,10,10,100,1\n"
  in
  with_csv source_rows (fun path ->
      let instrument = "RUNTIME_INSTRUMENT" in
      let source =
        match Causalis_data_source.Historical_csv.load_files ~instrument [ path ] with
        | Ok source -> source
        | Error error -> Alcotest.failf "%s: %s" error.path error.message
      in
      let input name = Signal.Input.create ~name ~clock:Signal.Clock.logical () in
      let inputs : Causalis_run.Backtest.inputs =
        { open_price = input "open";
          high_price = input "high";
          low_price = input "low";
          close_price = Threshold_generated.EmaTrend.input_close;
          volume = input "volume";
          open_interest = input "open_interest" }
      in
      let output = Threshold_generated.EmaTrend.output_exposure in
      let artifact =
        match Compiler.compile ~outputs:Threshold_generated.EmaTrend.outputs with
        | Ok artifact -> artifact
        | Error _ -> Alcotest.fail "strategy compilation failed"
      in
      let execution =
        Causalis_execution.Execution_model.create
          ~fees:Causalis_execution.Fee.zero
      in
      let result =
        match
          Causalis_run.Backtest.run
            ~initial_cash:(Domain.Money.of_decimal (Decimal.of_int 1000))
            ~source ~instrument ~inputs ~output ~artifact ~execution
        with
        | Ok result -> result
        | Error _ -> Alcotest.fail "instrument-neutral backtest failed"
      in
      let steps = Causalis_run.Result.steps result in
      List.iter
        (fun step ->
          Alcotest.(check (list string)) "runtime instrument allocation"
            [ instrument ]
            (Domain.allocation_instruments (Causalis_run.Result.allocation step)))
        steps;
      let final_portfolio =
        Causalis_run.Result.portfolio (List.hd (List.rev steps))
      in
      Alcotest.(check string) "backtest opened a short position" "-50.0"
        (Domain.Position.to_string
           (Domain.portfolio_position final_portfolio instrument)))

let test_weight_range () =
  Alcotest.(check bool) "accepts full long exposure" true
    (Result.is_ok (Domain.Weight.of_decimal Decimal.one));
  Alcotest.(check bool) "accepts full short exposure" true
    (Result.is_ok (Domain.Weight.of_decimal Decimal.(of_int (-1))));
  Alcotest.(check bool) "rejects exposure below -100%" true
    (Result.is_error (Domain.Weight.of_decimal Decimal.(of_int (-101))))

let () =
  Alcotest.run "strategy_frontend"
    [ ("parser", [ Alcotest.test_case "source location" `Quick test_parser_error_location ]);
      ("PPX and runtime",
         [ Alcotest.test_case "equivalence" `Quick test_ppx_matches_ocaml;
         Alcotest.test_case "runtime instrument binding" `Quick
           test_backtest_binds_instrument_outside_strategy;
         Alcotest.test_case "signed weight bounds" `Quick test_weight_range ]) ]