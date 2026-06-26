open Causalis_core

module Domain = Causalis_strategy.Domain

let money value = Domain.Money.of_decimal (Decimal.of_int value)

let with_csv rows callback =
  let path = Filename.temp_file "causalis-backtest" ".csv" in
  let channel = open_out_bin path in
  output_string channel
    ("timestamp,open,high,low,close,volume,open_interest\n" ^ rows);
  close_out channel;
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () -> callback path)

let make_strategy () =
  let input name =
    Signal.Input.create ~name ~clock:Signal.Clock.logical ()
  in
  let inputs : Causalis_run.Backtest.inputs =
    { open_price = input "open";
      high_price = input "high";
      low_price = input "low";
      close_price = input "close";
      volume = input "volume";
      open_interest = input "open_interest" }
  in
  let close = Signal.input inputs.close_price in
  let target =
    Signal.map
      (fun close ->
        if Decimal.(Value.Price.to_decimal close > Decimal.of_int 9) then
          Domain.Weight.one
        else Domain.Weight.zero)
      close
  in
  let output = Compiler.Output.create ~name:"target" target in
  let artifact =
    match Compiler.compile ~outputs:[ Compiler.Output.pack output ] with
    | Ok artifact -> artifact
    | Error _ -> Alcotest.fail "strategy compilation failed"
  in
  (inputs, output, artifact)

let load_source path =
  match
    Causalis_data_source.Historical_csv.load_files ~instrument:"ETF" [ path ]
  with
  | Ok source -> source
  | Error error -> Alcotest.failf "%s: %s" error.path error.message

let run_strategy path =
  let inputs, output, artifact = make_strategy () in
  let execution =
    Causalis_execution.Execution_model.create
      ~fees:Causalis_execution.Fee.zero
  in
  match
    Causalis_run.Backtest.run ~initial_cash:(money 100)
      ~source:(load_source path) ~instrument:"ETF" ~inputs ~output ~artifact
      ~execution
  with
  | Ok result -> result
  | Error _ -> Alcotest.fail "historical backtest failed"

let test_compiled_historical_replay () =
  let rows =
    "2025-01-01T09:15:00+05:30,10,10,10,10,100,1\n"
    ^ "2025-01-01T09:16:00+05:30,10,10,10,10,100,1\n"
    ^ "2025-01-01T09:17:00+05:30,8,8,8,8,100,1\n"
    ^ "2025-01-01T09:18:00+05:30,8,8,8,8,100,1\n"
  in
  with_csv rows (fun path ->
      let result = run_strategy path in
      let steps = Causalis_run.Result.steps result in
      Alcotest.(check int) "four historical instants" 4 (List.length steps);
      Alcotest.(check int) "no same-bar initial fill" 0
        (List.length (Causalis_run.Result.fills (List.nth steps 0)));
      let buy_step = List.nth steps 1 in
      Alcotest.(check int) "buy fills on next bar" 1
        (List.length (Causalis_run.Result.fills buy_step));
      let buy_fill = List.hd (Causalis_run.Result.fills buy_step) in
      Alcotest.(check int) "buy fill instant" 1
        (Causalis_execution.Fill.instant buy_fill);
      Alcotest.(check string) "buy fills at next close" "10"
        (Value.Price.to_string (Causalis_execution.Fill.price buy_fill));
      Alcotest.(check int) "sell fills on next bar" 1
        (List.length (Causalis_run.Result.fills (List.nth steps 3)));
      Alcotest.(check string) "final cash after close fill" "80"
        (Domain.Money.to_string
           (Domain.portfolio_cash
              (Causalis_run.Result.portfolio (List.nth steps 3))));
      Alcotest.(check int) "no terminal orders discarded" 0
        (List.length (Causalis_run.Result.discarded_intents result)))

let test_terminal_intent_is_reported () =
  let rows = "2025-01-01T09:15:00+05:30,10,10,10,10,100,1\n" in
  with_csv rows (fun path ->
      let result = run_strategy path in
      Alcotest.(check int) "terminal buy intent reported" 1
        (List.length (Causalis_run.Result.discarded_intents result));
      Alcotest.(check int) "terminal intent was not filled" 0
        (List.length
           (Causalis_run.Result.fills
              (List.hd (Causalis_run.Result.steps result))));
      Alcotest.(check string) "portfolio unchanged by discarded intent" "100"
        (Domain.Money.to_string
           (Domain.portfolio_cash
              (Causalis_run.Result.portfolio
                 (List.hd (Causalis_run.Result.steps result))))))

let () =
  Alcotest.run "backtest"
    [ ( "compiled historical replay",
        [ Alcotest.test_case "fills at next close" `Quick
            test_compiled_historical_replay;
          Alcotest.test_case "terminal intent is reported" `Quick
            test_terminal_intent_is_reported ] ) ]