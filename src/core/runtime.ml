module Input_trace = struct
  type t = (int * Obj.t array) list

  let of_values input values =
    (Signal.Input.id input, Array.of_list (List.map Obj.repr values)) :: []

  let combine traces = List.concat traces

  let length trace =
    match trace with
    | [] -> Some 0
    | (_, values) :: rest ->
        if List.for_all (fun (_, other) -> Array.length other = Array.length values) rest then
          Some (Array.length values)
        else
          None

  let value trace ~input_id ~instant =
    match List.assoc_opt input_id trace with
    | Some values when instant < Array.length values -> Some values.(instant)
    | _ -> None
end

module Run = struct
  type t = { outputs : (Signal.Internal.node_id * Obj.t option array) list }

  let create outputs = { outputs }

  let output_values run ~node_id =
    match List.assoc_opt node_id run.outputs with
    | Some values -> values
    | None -> invalid_arg "Runtime.Run.output_values: unknown output node"
end

module Reference_exec = struct
  type error =
    | Empty_input_trace
    | Input_length_mismatch
    | Missing_input of int

  let run artifact inputs =
    match Input_trace.length inputs with
    | None -> Error Input_length_mismatch
    | Some 0 -> Error Empty_input_trace
    | Some length ->
        let node_values = Hashtbl.create (List.length (Compiler.Artifact.nodes artifact)) in
        List.iter
          (fun node ->
            Hashtbl.add (node_values) (Signal.Internal.id node) (Array.make length None))
          (Compiler.Artifact.nodes artifact);
        let state_values = Array.make (List.length (Compiler.Artifact.state_layout artifact)) None in
        let state_slots = Hashtbl.create (Array.length state_values) in
        List.iter
          (fun slot -> Hashtbl.add state_slots slot.Compiler.node_id slot.Compiler.index)
          (Compiler.Artifact.state_layout artifact);
        let current_value node instant =
          (Hashtbl.find node_values (Signal.Internal.id node)).(instant)
        in
        let set_current_value node instant value =
          (Hashtbl.find node_values (Signal.Internal.id node)).(instant) <- value
        in
        let state_value node = state_values.(Hashtbl.find state_slots (Signal.Internal.id node)) in
        let set_state_value node value =
          state_values.(Hashtbl.find state_slots (Signal.Internal.id node)) <- value
        in
        let evaluate instant node =
          match Signal.Internal.operation node with
          | Signal.Internal.Const value -> Some value
          | Signal.Internal.Input input_id ->
              (match Input_trace.value inputs ~input_id ~instant with
              | Some value -> Some value
              | None -> raise (Invalid_argument (string_of_int input_id)))
          | Signal.Internal.Map { source; apply } ->
              Option.map apply (current_value source instant)
          | Signal.Internal.Pre _ -> state_value node
          | Signal.Internal.Init { initial; source } ->
              if instant = 0 then Some initial else current_value source instant
          | Signal.Internal.Scan { source; initial_state; step } ->
              let state =
                match state_value node with
                | Some state -> state
                | None ->
                    set_state_value node (Some initial_state);
                    initial_state
              in
              (match current_value source instant with
              | None -> None
              | Some input ->
                  let next_state, output = step state input in
                  set_state_value node (Some next_state);
                  Some output)
          | Signal.Internal.Feedback target ->
              (match !target with
              | Some target -> current_value target instant
              | None -> raise (Invalid_argument "uninitialized feedback"))
        in
        try
          for instant = 0 to length - 1 do
            List.iter
              (fun plan_node ->
                let value = evaluate instant plan_node.Compiler.node in
                set_current_value plan_node.Compiler.node instant value)
              (Compiler.Artifact.runtime_plan artifact);
            List.iter
              (fun node ->
                match Signal.Internal.operation node with
                | Signal.Internal.Pre source -> set_state_value node (current_value source instant)
                | _ -> ())
              (Compiler.Artifact.nodes artifact)
          done;
          let outputs =
            List.filter_map
              (fun (Compiler.Output.Pack output) ->
                let node_id = Signal.Internal.id (Signal.Internal.node (Compiler.Output.signal output)) in
                match Hashtbl.find_opt node_values node_id with
                | Some values -> Some (node_id, values)
                | None -> None)
              (Compiler.Artifact.outputs artifact)
          in
          Ok (Run.create outputs)
        with
        | Invalid_argument input_id -> Error (Missing_input (int_of_string input_id))
end

let values output run =
  Run.output_values run
    ~node_id:(Signal.Internal.id (Signal.Internal.node (Compiler.Output.signal output)))
  |> Array.to_list
  |> List.map (Option.map Obj.obj)
