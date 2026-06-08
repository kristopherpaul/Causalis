type t

val create : fees:Fee.t -> t
val execute : t -> instant:int -> Causalis_strategy.Domain.market ->
  Causalis_strategy.Intent.t list -> Fill.t list
