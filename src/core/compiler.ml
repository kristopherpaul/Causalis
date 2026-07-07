module Output = struct
  type 'a t = { name : string; signal : 'a Signal.t }
  type packed = Pack : 'a t -> packed

  let create ~name signal = { name; signal }
  let name output = output.name
  let signal output = output.signal
  let pack output = Pack output
end

type delay =
  | Instant
  | Delayed of int

type dependency = {
  source : Signal.Internal.node_id;
  target : Signal.Internal.node_id;
  delay : delay;
}

type state_kind =
  | Delay_state
  | Scan_state
  | Window_state

type state_slot = {
  node_id : Signal.Internal.node_id;
  index : int;
  kind : state_kind;
}

type plan_node = {
  node : Signal.Internal.node;
  dependencies : dependency list;
  state_slot : state_slot option;
}

module Artifact = struct
  type t = {
    nodes : Signal.Internal.node list;
    schedule : Signal.Internal.node list;
    dependencies : dependency list;
    state_layout : state_slot list;
    runtime_plan : plan_node list;
    outputs : Output.packed list;
  }

  let create ~nodes ~schedule ~dependencies ~state_layout ~runtime_plan ~outputs =
    { nodes; schedule; dependencies; state_layout; runtime_plan; outputs }

  let nodes artifact = artifact.nodes
  let schedule artifact = artifact.schedule
  let dependencies artifact = artifact.dependencies
  let state_layout artifact = artifact.state_layout
  let runtime_plan artifact = artifact.runtime_plan
  let outputs artifact = artifact.outputs
end

type error =
  | No_outputs
  | Mixed_clocks
  | Invalid_feedback of Signal.Internal.node_id
  | Instantaneous_cycle of Signal.Internal.node_id list

exception Compile_error of error

let compile ~outputs =
  match outputs with
  | [] -> Error No_outputs
  | Output.Pack first_output :: _ ->
      let first_clock = Signal.clock (Output.signal first_output) in
      let nodes_by_id = Hashtbl.create 16 in
      let visited = Hashtbl.create 16 in
      let discovered = ref [] in
      let edges = ref [] in
      let add_edge source target delay =
        edges :=
          { source = Signal.Internal.id source;
            target = Signal.Internal.id target;
            delay }
          :: !edges
      in
      let rec visit node =
        let node_id = Signal.Internal.id node in
        if not (Hashtbl.mem visited node_id) then begin
          Hashtbl.add visited node_id ();
          Hashtbl.replace nodes_by_id node_id node;
          if not (Signal.Clock.equal first_clock (Signal.Internal.clock node)) then
            raise_notrace (Compile_error Mixed_clocks);
          discovered := node :: !discovered;
          match Signal.Internal.operation node with
          | Signal.Internal.Const _ | Signal.Internal.Input _ -> ()
          | Signal.Internal.Map { source; _ } ->
              visit source;
              add_edge source node Instant
          | Signal.Internal.Pre source ->
              visit source;
              add_edge source node (Delayed 1)
          | Signal.Internal.Init { source; _ } ->
              visit source;
              add_edge source node Instant
          | Signal.Internal.Scan { source; _ } ->
              visit source;
              add_edge source node Instant
            | Signal.Internal.Window { source; _ } ->
              visit source;
              add_edge source node Instant
          | Signal.Internal.Feedback target ->
              (match !target with
              | Some target ->
                  visit target;
                  add_edge target node Instant
              | None -> raise_notrace (Compile_error (Invalid_feedback node_id)))
        end
      in
      let stable_sort_nodes nodes =
        List.sort
          (fun left right ->
            compare (Signal.Internal.id left) (Signal.Internal.id right))
          nodes
      in
      let stable_sort_ids ids = List.sort compare ids in
      let topological_schedule nodes dependencies =
        let node_ids = List.map Signal.Internal.id nodes in
        let indegree = Hashtbl.create (List.length nodes) in
        let adjacency = Hashtbl.create (List.length nodes) in
        List.iter (fun node_id -> Hashtbl.replace indegree node_id 0) node_ids;
        List.iter
          (fun dependency ->
            match dependency.delay with
            | Delayed _ -> ()
            | Instant ->
                let count = Hashtbl.find indegree dependency.target in
                Hashtbl.replace indegree dependency.target (count + 1);
                let targets =
                  match Hashtbl.find_opt adjacency dependency.source with
                  | Some targets -> targets
                  | None -> []
                in
                Hashtbl.replace adjacency dependency.source
                  (dependency.target :: targets))
          dependencies;
        let rec loop ready result remaining =
          match stable_sort_ids ready with
          | [] ->
              if remaining = [] then List.rev result
              else raise (Compile_error (Instantaneous_cycle (stable_sort_ids remaining)))
          | next :: rest ->
              let next_targets =
                match Hashtbl.find_opt adjacency next with
                | Some targets -> targets
                | None -> []
              in
              let newly_ready =
                List.fold_left
                  (fun ready target ->
                    let count = Hashtbl.find indegree target - 1 in
                    Hashtbl.replace indegree target count;
                    if count = 0 then target :: ready else ready)
                  rest next_targets
              in
              loop newly_ready (next :: result)
                (List.filter (fun node_id -> node_id <> next) remaining)
        in
        let initial_ready =
          List.filter (fun node_id -> Hashtbl.find indegree node_id = 0) node_ids
        in
        loop initial_ready [] node_ids
      in
      try
        List.iter
          (fun (Output.Pack output) -> visit (Signal.Internal.node (Output.signal output)))
          outputs;
        let nodes = stable_sort_nodes !discovered in
        let dependencies = List.rev !edges in
        let schedule_ids = topological_schedule nodes dependencies in
        let node node_id = Hashtbl.find nodes_by_id node_id in
        let schedule = List.map node schedule_ids in
        let state_layout =
          let _, slots =
            List.fold_left
              (fun (index, slots) node ->
                match Signal.Internal.operation node with
                | Signal.Internal.Pre _ ->
                    (index + 1,
                     { node_id = Signal.Internal.id node; index; kind = Delay_state } :: slots)
                | Signal.Internal.Scan _ ->
                    (index + 1,
                     { node_id = Signal.Internal.id node; index; kind = Scan_state } :: slots)
                | Signal.Internal.Window _ ->
                  (index + 1,
                   { node_id = Signal.Internal.id node; index; kind = Window_state } :: slots)
                | _ -> (index, slots))
              (0, []) nodes
          in
          List.rev slots
        in
        let runtime_plan =
          List.map
            (fun node ->
              let node_id = Signal.Internal.id node in
              let node_dependencies =
                List.filter (fun dependency -> dependency.target = node_id) dependencies
              in
              let state_slot =
                List.find_opt (fun slot -> slot.node_id = node_id) state_layout
              in
              { node; dependencies = node_dependencies; state_slot })
            schedule
        in
        Ok
          (Artifact.create ~nodes ~schedule ~dependencies ~state_layout ~runtime_plan
             ~outputs)
      with Compile_error error -> Error error
