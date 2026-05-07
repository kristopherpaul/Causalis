module Input_trace : sig
  type t

  val of_values : 'a Signal.Input.t -> 'a list -> t
  val combine : t list -> t
end

module Run : sig
  type t
end

module Reference_exec : sig
  type error =
    | Empty_input_trace
    | Input_length_mismatch
    | Missing_input of int

  val run : Compiler.Artifact.t -> Input_trace.t -> (Run.t, error) result
end

val values : 'a Compiler.Output.t -> Run.t -> 'a list
