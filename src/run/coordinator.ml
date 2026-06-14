type strategy =
  Causalis_strategy.Domain.market ->
  Causalis_strategy.Domain.portfolio_observation ->
  Causalis_strategy.Domain.allocation

type error =
  | Planning of Causalis_strategy.Planner.error
  | Accounting of Causalis_accounting.Accounting_model.error

let run ~initial_cash ~markets ~strategy ~execution =
  let accounting = Causalis_accounting.Accounting_model.create ~initial_cash in
  let rec loop instant markets reversed_steps =
    match markets with
    | [] -> Ok (Result.create (List.rev reversed_steps))
    | market :: rest ->
        match Causalis_accounting.Accounting_model.observe accounting market with
        | Error error -> Error (Accounting error)
        | Ok portfolio ->
            let allocation = strategy market portfolio in
            match
              Causalis_strategy.Planner.rebalance allocation portfolio market
            with
            | Error error -> Error (Planning error)
            | Ok intents ->
                let fills =
                  Causalis_execution.Execution_model.execute execution ~instant
                    market intents
                in
                match
                  Causalis_accounting.Accounting_model.apply_fills accounting
                    market fills
                with
                | Error error -> Error (Accounting error)
                | Ok () ->
                    match
                      Causalis_accounting.Accounting_model.observe accounting
                        market
                    with
                    | Error error -> Error (Accounting error)
                    | Ok portfolio ->
                        let step =
                          Result.create_step ~instant ~allocation ~intents ~fills
                            ~portfolio
                        in
                        loop (instant + 1) rest (step :: reversed_steps)
  in
  loop 0 markets []
