module Output : sig
  type 'a t
  type packed = Pack : 'a t -> packed

  val create : name:string -> 'a Signal.t -> 'a t
  val name : 'a t -> string
  val signal : 'a t -> 'a Signal.t
  val pack : 'a t -> packed
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

module Artifact : sig
  type t

  val nodes : t -> Signal.Internal.node list
  val schedule : t -> Signal.Internal.node list
  val dependencies : t -> dependency list
  val state_layout : t -> state_slot list
  val runtime_plan : t -> plan_node list
  val outputs : t -> Output.packed list
end

type error =
  | No_outputs
  | Mixed_clocks
  | Invalid_feedback of Signal.Internal.node_id
  | Instantaneous_cycle of Signal.Internal.node_id list

val compile : outputs:Output.packed list -> (Artifact.t, error) result
