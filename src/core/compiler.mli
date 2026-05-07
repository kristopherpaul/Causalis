module Output : sig
  type 'a t
  type packed = Pack : 'a t -> packed

  val create : name:string -> 'a Signal.t -> 'a t
  val name : 'a t -> string
  val signal : 'a t -> 'a Signal.t
  val pack : 'a t -> packed
end

module Artifact : sig
  type t

  val nodes : t -> Signal.Internal.node list
  val outputs : t -> Output.packed list
end

type error =
  | No_outputs
  | Mixed_clocks

val compile : outputs:Output.packed list -> (Artifact.t, error) result
