type t = {
  mutable cash : Decimal.t;
  mutable positions : (string * Decimal.t) list;
}

type error =
  | Insufficient_cash
  | Missing_price of string

let create ~initial_cash =
  { cash = Causalis_strategy.Domain.Money.to_decimal initial_cash; positions = [] }

let position positions instrument =
  match List.assoc_opt instrument positions with
  | Some quantity -> quantity
  | None -> Decimal.zero

let set_position positions instrument quantity =
  let filtered = List.remove_assoc instrument positions in
  if Decimal.(quantity = zero) then filtered
  else (instrument, quantity) :: filtered

let observe state market =
  let rec value positions total =
    match positions with
    | [] -> Ok total
    | (instrument, quantity) :: rest ->
        (match Causalis_strategy.Domain.market_price market instrument with
        | None -> Error (Missing_price instrument)
        | Some price ->
            let marked =
              Decimal.(quantity * Causalis_core.Value.Price.to_decimal price)
            in
            value rest Decimal.(total + marked))
  in
  match value state.positions state.cash with
  | Error error -> Error error
  | Ok equity ->
      let positions =
        List.map
          (fun (instrument, quantity) ->
            instrument, Causalis_strategy.Domain.Position.of_decimal quantity)
          state.positions
      in
      Ok
        (Causalis_strategy.Domain.portfolio_observation
           ~cash:(Causalis_strategy.Domain.Money.of_decimal state.cash)
           ~equity:(Causalis_strategy.Domain.Money.of_decimal equity)
           ~positions)

let apply_fills state market fills =
  let rec validate cash positions = function
    | [] -> Ok (cash, positions)
    | fill :: rest ->
        let instrument = Causalis_execution.Fill.instrument fill in
        let quantity =
          Causalis_strategy.Domain.Quantity.to_decimal
            (Causalis_execution.Fill.quantity fill)
        in
        let price = Causalis_execution.Fill.price fill in
        let fee =
          Causalis_strategy.Domain.Money.to_decimal
            (Causalis_execution.Fill.fee fill)
        in
        let notional =
          Decimal.(quantity * Causalis_core.Value.Price.to_decimal price)
        in
        let current = position positions instrument in
        let next_cash, next_position =
          match Causalis_execution.Fill.side fill with
          | Causalis_strategy.Domain.Buy ->
              Decimal.(cash - notional - fee), Decimal.(current + quantity)
          | Causalis_strategy.Domain.Sell ->
              Decimal.(cash + notional - fee), Decimal.(current - quantity)
        in
        if Decimal.(next_cash < zero) then Error Insufficient_cash
        else
          validate next_cash
            (set_position positions instrument next_position)
            rest
  in
  match validate state.cash state.positions fills with
  | Error error -> Error error
  | Ok (cash, positions) ->
      state.cash <- cash;
      state.positions <- positions;
      (match observe state market with
      | Ok _ -> Ok ()
      | Error error -> Error error)
