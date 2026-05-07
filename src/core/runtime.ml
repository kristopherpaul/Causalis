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
  type t = { outputs : (Signal.Internal.node_id * Obj.t array) list }

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
    | Some length ->
        let node_values = Hashtbl.create (List.length (Compiler.Artifact.nodes artifact)) in
        let output_values = Hashtbl.create 8 in
        let evaluate instant node =
          match Signal.Internal.operation node with
          | Signal.Internal.Const value -> value
          | Signal.Internal.Input input_id ->
              (match Input_trace.value inputs ~input_id ~instant with
              | Some value -> value
              | None -> raise (Invalid_argument (string_of_int input_id)))
          | Signal.Internal.Map { source; apply } ->
              apply (Hashtbl.find node_values (Signal.Internal.id source)).(instant)
        in
        try
          List.iter
            (fun node ->
              let values = Array.init length (fun instant -> evaluate instant node) in
              Hashtbl.replace node_values (Signal.Internal.id node) values;
              Hashtbl.replace output_values (Signal.Internal.id node) values)
            (Compiler.Artifact.nodes artifact);
          let outputs =
            List.filter_map
              (fun (Compiler.Output.Pack output) ->
                let node_id = Signal.Internal.id (Signal.Internal.node (Compiler.Output.signal output)) in
                match Hashtbl.find_opt output_values node_id with
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
  |> List.map Obj.obj
