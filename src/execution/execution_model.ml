type t = { fees : Fee.t }

let create ~fees = { fees }

let execute execution ~instant market intents =
  List.map
    (fun intent ->
      let instrument = Causalis_strategy.Intent.instrument intent in
      let side = Causalis_strategy.Intent.side intent in
      let quantity = Causalis_strategy.Intent.quantity intent in
      match Causalis_strategy.Domain.market_price market instrument with
      | None -> invalid_arg "Execution_model.execute: missing market price"
      | Some price ->
          let fee =
            Fee.calculate execution.fees ~instrument ~side ~quantity ~price
          in
          Fill.create ~instant ~instrument ~side ~quantity ~price ~fee)
    intents
