open Causalis_core

module Domain = Causalis_strategy.Domain

let price value = Value.Price.of_decimal (Decimal.of_int value)
let money value = Domain.Money.of_decimal (Decimal.of_int value)

let market close = Domain.market ~prices:[ ("ABC", price close) ]

let test_threshold_closed_loop () =
  let observations = ref [] in
  let strategy market portfolio =
    observations := portfolio :: !observations;
    let close =
      match Domain.market_price market "ABC" with
      | Some value -> Value.Price.to_decimal value
      | None -> Alcotest.fail "missing close"
    in
    let weight = if Decimal.(close > Decimal.of_int 9) then Domain.Weight.one else Domain.Weight.zero in
    Domain.allocation [ ("ABC", weight) ]
  in
  let execution = Causalis_execution.Execution_model.create ~fees:Causalis_execution.Fee.zero in
  let result =
    match
      Causalis_run.Coordinator.run ~initial_cash:(money 100)
        ~markets:[ market 10; market 12; market 8 ] ~strategy ~execution
    with
    | Ok result -> result
    | Error _ -> Alcotest.fail "closed loop failed"
  in
  let steps = Causalis_run.Result.steps result in
  Alcotest.(check int) "three instants" 3 (List.length steps);
  let first = List.nth steps 0 in
  let second = List.nth steps 1 in
  let third = List.nth steps 2 in
  Alcotest.(check int) "initial buy" 1
    (List.length (Causalis_run.Result.fills first));
  Alcotest.(check int) "hold" 0
    (List.length (Causalis_run.Result.intents second));
  Alcotest.(check int) "final sell" 1
    (List.length (Causalis_run.Result.fills third));
  let second_portfolio = Causalis_run.Result.portfolio second in
  Alcotest.(check string) "marked cash" "0"
    (Domain.Money.to_string (Domain.portfolio_cash second_portfolio));
  Alcotest.(check string) "marked equity" "120"
    (Domain.Money.to_string (Domain.portfolio_equity second_portfolio));
  Alcotest.(check string) "next strategy sees equity" "120"
    (Domain.Money.to_string
       (Domain.portfolio_equity (List.nth (List.rev !observations) 1)));
  let final_portfolio = Causalis_run.Result.portfolio third in
  Alcotest.(check string) "final cash" "80"
    (Domain.Money.to_string (Domain.portfolio_cash final_portfolio));
  Alcotest.(check string) "final equity" "80"
    (Domain.Money.to_string (Domain.portfolio_equity final_portfolio))

let test_insufficient_cash_rejected () =
  let accounting =
    Causalis_accounting.Accounting_model.create ~initial_cash:(money 10)
  in
  let quantity =
    match Domain.Quantity.of_decimal (Decimal.of_int 2) with
    | Ok quantity -> quantity
    | Error _ -> Alcotest.fail "invalid quantity"
  in
  let fill =
    Causalis_execution.Fill.create ~instant:0 ~instrument:"ABC"
      ~side:Domain.Buy ~quantity ~price:(price 10)
      ~fee:Domain.Money.zero
  in
  match
    Causalis_accounting.Accounting_model.apply_fills accounting (market 10)
      [ fill ]
  with
  | Error Causalis_accounting.Accounting_model.Insufficient_cash -> ()
  | _ -> Alcotest.fail "insufficient cash should be rejected"

let test_short_position_lifecycle () =
  let accounting =
    Causalis_accounting.Accounting_model.create ~initial_cash:(money 100)
  in
  let quantity =
    match Domain.Quantity.of_decimal (Decimal.of_int 10) with
    | Ok quantity -> quantity
    | Error _ -> Alcotest.fail "invalid trade quantity"
  in
  let market = market 10 in
  let apply side instant =
    let fill =
      Causalis_execution.Fill.create ~instant ~instrument:"ABC" ~side ~quantity
        ~price:(price 10) ~fee:Domain.Money.zero
    in
    match
      Causalis_accounting.Accounting_model.apply_fills accounting market [ fill ]
    with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "short lifecycle fill failed"
  in
  apply Domain.Sell 0;
  let short_observation =
    match Causalis_accounting.Accounting_model.observe accounting market with
    | Ok observation -> observation
    | Error _ -> Alcotest.fail "short portfolio should be observable"
  in
  Alcotest.(check string) "signed short position" "-10"
    (Domain.Position.to_string (Domain.portfolio_position short_observation "ABC"));
  Alcotest.(check string) "short proceeds" "200"
    (Domain.Money.to_string (Domain.portfolio_cash short_observation));
  Alcotest.(check string) "short equity" "100"
    (Domain.Money.to_string (Domain.portfolio_equity short_observation));
  apply Domain.Buy 1;
  let covered_observation =
    match Causalis_accounting.Accounting_model.observe accounting market with
    | Ok observation -> observation
    | Error _ -> Alcotest.fail "covered portfolio should be observable"
  in
  Alcotest.(check string) "cover closes position" "0"
    (Domain.Position.to_string (Domain.portfolio_position covered_observation "ABC"));
  Alcotest.(check string) "cash after cover" "100"
    (Domain.Money.to_string (Domain.portfolio_cash covered_observation))

let () =
  Alcotest.run "closed_loop"
    [ ("trading runtime", [ Alcotest.test_case "threshold" `Quick test_threshold_closed_loop ]);
      ("accounting",
       [ Alcotest.test_case "cash rejection" `Quick test_insufficient_cash_rejected;
         Alcotest.test_case "short lifecycle" `Quick test_short_position_lifecycle ]) ]
