module Clock = struct
  type t = { name : string }

  let logical = { name = "logical" }
  let name clock = clock.name
  let equal left right = String.equal left.name right.name
end

module Input = struct
  type 'a t = { id : int; name : string; clock : Clock.t }

  let next_id = ref 0

  let create ~name ~clock () =
    let id = !next_id in
    incr next_id;
    { id; name; clock }

  let name input = input.name
  let clock input = input.clock
  let id input = input.id
end

module Graph = struct
  type node_id = int

  type operation =
    | Const of Obj.t
    | Input of int
    | Map of { source : node; apply : Obj.t -> Obj.t }
    | Pre of node
    | Init of { initial : Obj.t; source : node }
    | Scan of {
        source : node;
        initial_state : Obj.t;
        step : Obj.t -> Obj.t -> Obj.t * Obj.t;
      }
    | Feedback of node option ref

  and node = { id : node_id; clock : Clock.t; operation : operation }

  let next_id = ref 0

  let make clock operation =
    let id = !next_id in
    incr next_id;
    { id; clock; operation }

  let const ~clock value = make clock (Const value)
  let input ~clock input_id = make clock (Input input_id)
  let map ~clock ~source ~apply = make clock (Map { source; apply })
  let id node = node.id
  let clock node = node.clock
  let operation node = node.operation
end

type 'a t = { node : Graph.node }

let const ~clock value = { node = Graph.const ~clock (Obj.repr value) }
let input descriptor = { node = Graph.input ~clock:(Input.clock descriptor) (Input.id descriptor) }

let map f signal =
  let apply value = Obj.repr (f (Obj.obj value)) in
  { node = Graph.map ~clock:(Graph.clock signal.node) ~source:signal.node ~apply }

let pre signal =
  { node = Graph.make (Graph.clock signal.node) (Graph.Pre signal.node) }

let init initial signal =
  {
    node =
      Graph.make (Graph.clock signal.node)
        (Graph.Init { initial = Obj.repr initial; source = signal.node });
  }

let scan ~init:initial_state ~step signal =
  let erased_step state input =
    let next_state, output = step (Obj.obj state) (Obj.obj input) in
    Obj.repr next_state, Obj.repr output
  in
  {
    node =
      Graph.make (Graph.clock signal.node)
        (Graph.Scan
           {
             source = signal.node;
             initial_state = Obj.repr initial_state;
             step = erased_step;
           });
  }

let feedback ~clock build =
  let target = ref None in
  let node = Graph.make clock (Graph.Feedback target) in
  let body = (build { node } : 'a t) in
  target := Some body.node;
  { node }

let clock signal = Graph.clock signal.node

module Internal = struct
  type node_id = Graph.node_id
  type node = Graph.node
  type operation = Graph.operation =
    | Const of Obj.t
    | Input of int
    | Map of { source : node; apply : Obj.t -> Obj.t }
    | Pre of node
    | Init of { initial : Obj.t; source : node }
    | Scan of {
        source : node;
        initial_state : Obj.t;
        step : Obj.t -> Obj.t -> Obj.t * Obj.t;
      }
    | Feedback of node option ref

  let node signal = signal.node
  let id = Graph.id
  let clock = Graph.clock
  let operation = Graph.operation
end
