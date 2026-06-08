type t

type error =
  | Insufficient_cash
  | Short_position
  | Missing_price of string

val create : initial_cash:Causalis_strategy.Domain.Money.t -> t
val observe : t -> Causalis_strategy.Domain.market ->
  (Causalis_strategy.Domain.portfolio_observation, error) result
val apply_fills : t -> Causalis_strategy.Domain.market ->
  Causalis_execution.Fill.t list -> (unit, error) result
