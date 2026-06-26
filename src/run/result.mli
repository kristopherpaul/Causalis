type step

type t

val create : step list -> t
val create_with_discarded_intents :
	Causalis_strategy.Intent.t list -> step list -> t
val create_step : instant:int ->
	allocation:Causalis_strategy.Domain.allocation ->
	intents:Causalis_strategy.Intent.t list ->
	fills:Causalis_execution.Fill.t list ->
	portfolio:Causalis_strategy.Domain.portfolio_observation -> step
val steps : t -> step list
val discarded_intents : t -> Causalis_strategy.Intent.t list
val instant : step -> int
val allocation : step -> Causalis_strategy.Domain.allocation
val intents : step -> Causalis_strategy.Intent.t list
val fills : step -> Causalis_execution.Fill.t list
val portfolio : step -> Causalis_strategy.Domain.portfolio_observation
