type t

val zero : t
val calculate : t -> instrument:string -> side:Causalis_strategy.Domain.side ->
  quantity:Causalis_strategy.Domain.Quantity.t ->
  price:Causalis_core.Value.Price.t -> Causalis_strategy.Domain.Money.t
