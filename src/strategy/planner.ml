type error =
  | Missing_price of string
  | Insufficient_cash

let unique_instruments allocation observation =
  List.fold_left
    (fun instruments instrument ->
      if List.mem instrument instruments then instruments
      else instruments @ [ instrument ])
    (Domain.allocation_instruments allocation)
    (List.map fst (Domain.portfolio_positions observation))

let rebalance allocation observation market =
  let instruments = unique_instruments allocation observation in
  let cash = Domain.Money.to_decimal (Domain.portfolio_cash observation) in
  let equity = Domain.Money.to_decimal (Domain.portfolio_equity observation) in
  let rec build remaining required_cash intents =
    match remaining with
    | [] ->
        if Decimal.(required_cash > cash) then Error Insufficient_cash
        else Ok (List.rev intents)
    | instrument :: rest ->
        (match Domain.market_price market instrument with
        | None -> Error (Missing_price instrument)
        | Some price ->
            let price_value = Causalis_core.Value.Price.to_decimal price in
            let target_weight =
              Domain.Weight.to_decimal
                (Domain.allocation_weight allocation instrument)
            in
            let target_quantity = Decimal.(equity * target_weight / price_value) in
            let current_quantity =
              Domain.Position.to_decimal
                (Domain.portfolio_position observation instrument)
            in
            let delta = Decimal.(target_quantity - current_quantity) in
            if Decimal.(delta = zero) then
              build rest required_cash intents
            else if Decimal.(delta > zero) then
              let quantity =
                match Domain.Quantity.of_decimal delta with
                | Ok quantity -> quantity
                | Error message -> invalid_arg message
              in
              let notional = Decimal.(delta * price_value) in
              build rest Decimal.(required_cash + notional)
                (Intent.create ~instrument ~side:Domain.Buy ~quantity :: intents)
            else
              let quantity =
                match Domain.Quantity.of_decimal Decimal.(-delta) with
                | Ok quantity -> quantity
                | Error message -> invalid_arg message
              in
              build rest required_cash
                (Intent.create ~instrument ~side:Domain.Sell ~quantity :: intents))
  in
  build instruments Decimal.zero []
