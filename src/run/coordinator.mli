type strategy =
  Causalis_strategy.Domain.market ->
  Causalis_strategy.Domain.portfolio_observation ->
  Causalis_strategy.Domain.allocation

type error =
  | Planning of Causalis_strategy.Planner.error
  | Accounting of Causalis_accounting.Accounting_model.error
  | Target_count_mismatch

val run :
  initial_cash:Causalis_strategy.Domain.Money.t ->
  markets:Causalis_strategy.Domain.market list ->
  strategy:strategy ->
  execution:Causalis_execution.Execution_model.t ->
  (Result.t, error) result

val run_targets :
  initial_cash:Causalis_strategy.Domain.Money.t ->
  markets:Causalis_strategy.Domain.market list ->
  targets:Causalis_strategy.Domain.allocation list ->
  execution:Causalis_execution.Execution_model.t ->
  (Result.t, error) result
