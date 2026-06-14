type step = {
  instant : int;
  allocation : Causalis_strategy.Domain.allocation;
  intents : Causalis_strategy.Intent.t list;
  fills : Causalis_execution.Fill.t list;
  portfolio : Causalis_strategy.Domain.portfolio_observation;
}

type t = step list

let create steps = steps
let create_step ~instant ~allocation ~intents ~fills ~portfolio =
  { instant; allocation; intents; fills; portfolio }
let steps result = result
let instant step = step.instant
let allocation step = step.allocation
let intents step = step.intents
let fills step = step.fills
let portfolio step = step.portfolio
