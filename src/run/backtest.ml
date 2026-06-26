open Causalis_core

type inputs = {
  open_price : Value.Price.t Signal.Input.t;
  high_price : Value.Price.t Signal.Input.t;
  low_price : Value.Price.t Signal.Input.t;
  close_price : Value.Price.t Signal.Input.t;
  volume : Decimal.t option Signal.Input.t;
  open_interest : Decimal.t option Signal.Input.t;
}

type error =
  | Empty_history
  | Duplicate_input_descriptor
  | Output_not_in_artifact
  | Runtime of Runtime.Reference_exec.error
  | Output_length_mismatch
  | Undefined_output of int
  | Coordinator of Coordinator.error

let run ~initial_cash ~source ~instrument ~inputs ~output ~artifact ~execution =
  let bars = Causalis_data_source.Historical_csv.bars source in
  if bars = [] then Error Empty_history
  else
    let input_descriptors =
      [ Signal.Input.id inputs.open_price;
        Signal.Input.id inputs.high_price;
        Signal.Input.id inputs.low_price;
        Signal.Input.id inputs.close_price;
        Signal.Input.id inputs.volume;
        Signal.Input.id inputs.open_interest ]
    in
    if List.length (List.sort_uniq compare input_descriptors) <> 6 then
      Error Duplicate_input_descriptor
    else
      let output_node_id =
        Signal.Internal.id (Signal.Internal.node (Compiler.Output.signal output))
      in
      let output_is_compiled =
        List.exists
          (fun (Compiler.Output.Pack candidate) ->
            Signal.Internal.id
              (Signal.Internal.node (Compiler.Output.signal candidate))
            = output_node_id)
          (Compiler.Artifact.outputs artifact)
      in
      if not output_is_compiled then Error Output_not_in_artifact
      else
        let input_trace =
          Runtime.Input_trace.combine
            [ Runtime.Input_trace.of_values inputs.open_price
                (List.map Causalis_data_source.Historical_csv.open_price bars);
              Runtime.Input_trace.of_values inputs.high_price
                (List.map Causalis_data_source.Historical_csv.high_price bars);
              Runtime.Input_trace.of_values inputs.low_price
                (List.map Causalis_data_source.Historical_csv.low_price bars);
              Runtime.Input_trace.of_values inputs.close_price
                (List.map Causalis_data_source.Historical_csv.close_price bars);
              Runtime.Input_trace.of_values inputs.volume
                (List.map Causalis_data_source.Historical_csv.volume bars);
              Runtime.Input_trace.of_values inputs.open_interest
                (List.map Causalis_data_source.Historical_csv.open_interest bars) ]
        in
        (match Runtime.Reference_exec.run artifact input_trace with
        | Error error -> Error (Runtime error)
        | Ok run ->
            let target_weights = Runtime.values output run in
            if List.length target_weights <> List.length bars then
              Error Output_length_mismatch
            else
              let rec targets bars weights reversed =
                match (bars, weights) with
                | [], [] -> Ok (List.rev reversed)
                | _bar :: remaining_bars, Some weight :: remaining_weights ->
                    let allocation =
                      Causalis_strategy.Domain.allocation [ (instrument, weight) ]
                    in
                    targets remaining_bars remaining_weights (allocation :: reversed)
                | bar :: _, None :: _ ->
                    Error
                      (Undefined_output
                         (Causalis_data_source.Historical_csv.logical_instant bar))
                | _ -> Error Output_length_mismatch
              in
              (match targets bars target_weights [] with
              | Error error -> Error error
              | Ok targets ->
                  let markets =
                    List.map Causalis_data_source.Historical_csv.market bars
                  in
                  (match
                     Coordinator.run_targets ~initial_cash ~markets ~targets ~execution
                   with
                  | Ok result -> Ok result
                  | Error error -> Error (Coordinator error))))