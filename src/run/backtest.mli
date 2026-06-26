type inputs = {
  open_price : Causalis_core.Value.Price.t Causalis_core.Signal.Input.t;
  high_price : Causalis_core.Value.Price.t Causalis_core.Signal.Input.t;
  low_price : Causalis_core.Value.Price.t Causalis_core.Signal.Input.t;
  close_price : Causalis_core.Value.Price.t Causalis_core.Signal.Input.t;
  volume : Decimal.t option Causalis_core.Signal.Input.t;
  open_interest : Decimal.t option Causalis_core.Signal.Input.t;
}

type error =
  | Empty_history
  | Duplicate_input_descriptor
  | Output_not_in_artifact
  | Runtime of Causalis_core.Runtime.Reference_exec.error
  | Output_length_mismatch
  | Undefined_output of int
  | Coordinator of Coordinator.error

val run :
  initial_cash:Causalis_strategy.Domain.Money.t ->
  source:Causalis_data_source.Historical_csv.t ->
  instrument:string ->
  inputs:inputs ->
  output:Causalis_strategy.Domain.Weight.t Causalis_core.Compiler.Output.t ->
  artifact:Causalis_core.Compiler.Artifact.t ->
  execution:Causalis_execution.Execution_model.t ->
  (Result.t, error) result