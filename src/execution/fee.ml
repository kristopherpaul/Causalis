type t = unit

let zero = ()

let calculate _ ~instrument:_ ~side:_ ~quantity:_ ~price:_ =
  Causalis_strategy.Domain.Money.zero
