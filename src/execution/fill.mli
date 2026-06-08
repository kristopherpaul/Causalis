type t

val create : instant:int -> instrument:string ->
  side:Causalis_strategy.Domain.side ->
  quantity:Causalis_strategy.Domain.Quantity.t ->
  price:Causalis_core.Value.Price.t -> fee:Causalis_strategy.Domain.Money.t -> t
val instant : t -> int
val instrument : t -> string
val side : t -> Causalis_strategy.Domain.side
val quantity : t -> Causalis_strategy.Domain.Quantity.t
val price : t -> Causalis_core.Value.Price.t
val fee : t -> Causalis_strategy.Domain.Money.t
