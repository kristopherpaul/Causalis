type step = {
  instant : int;
  allocation : Causalis_strategy.Domain.allocation;
  intents : Causalis_strategy.Intent.t list;
  fills : Causalis_execution.Fill.t list;
  portfolio : Causalis_strategy.Domain.portfolio_observation;
}

type t = { steps : step list; discarded_intents : Causalis_strategy.Intent.t list }

let create steps = { steps; discarded_intents = [] }
let create_with_discarded_intents discarded_intents steps =
  { steps; discarded_intents }
let create_step ~instant ~allocation ~intents ~fills ~portfolio =
  { instant; allocation; intents; fills; portfolio }
let steps result = result.steps
let discarded_intents result = result.discarded_intents
let instant step = step.instant
let allocation step = step.allocation
let intents step = step.intents
let fills step = step.fills
let portfolio step = step.portfolio
