module Output = struct
  type 'a t = { name : string; signal : 'a Signal.t }
  type packed = Pack : 'a t -> packed

  let create ~name signal = { name; signal }
  let name output = output.name
  let signal output = output.signal
  let pack output = Pack output
end

module Artifact = struct
  type t = { nodes : Signal.Internal.node list; outputs : Output.packed list }

  let create ~nodes ~outputs = { nodes; outputs }
  let nodes artifact = artifact.nodes
  let outputs artifact = artifact.outputs
end

type error =
  | No_outputs
  | Mixed_clocks

let compile ~outputs =
  match outputs with
  | [] -> Error No_outputs
  | Output.Pack first_output :: _ ->
      let first_clock = Signal.clock (Output.signal first_output) in
      let valid_clock clock = Signal.Clock.equal first_clock clock in
      let seen = Hashtbl.create 16 in
      let schedule = ref [] in
      let rec visit node =
        if not (Hashtbl.mem seen (Signal.Internal.id node)) then begin
          Hashtbl.add seen (Signal.Internal.id node) ();
          if not (valid_clock (Signal.Internal.clock node)) then raise_notrace Exit;
          (match Signal.Internal.operation node with
          | Signal.Internal.Const _ | Signal.Internal.Input _ -> ()
          | Signal.Internal.Map { source; _ } -> visit source);
          schedule := node :: !schedule
        end
      in
      try
        List.iter
          (fun (Output.Pack output) -> visit (Signal.Internal.node (Output.signal output)))
          outputs;
        Ok (Artifact.create ~nodes:(List.rev !schedule) ~outputs)
      with Exit -> Error Mixed_clocks
