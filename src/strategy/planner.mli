type error =
  | Missing_price of string
  | Insufficient_cash

val rebalance :
  Domain.allocation ->
  Domain.portfolio_observation ->
  Domain.market ->
  (Intent.t list, error) result
