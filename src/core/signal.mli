module Clock : sig
  type t

  val logical : t
  val name : t -> string
  val equal : t -> t -> bool
end

module Input : sig
  type 'a t

  val create : name:string -> clock:Clock.t -> unit -> 'a t
  val name : 'a t -> string
  val clock : 'a t -> Clock.t
  val id : 'a t -> int
end

type 'a t

val const : clock:Clock.t -> 'a -> 'a t
val input : 'a Input.t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val pre : 'a t -> 'a t
val init : 'a -> 'a t -> 'a t
val scan : init:'s -> step:('s -> 'a -> 's * 'b) -> 'a t -> 'b t
val window : int -> 'a t -> 'a list t
val sma : int -> Value.Price.t t -> Value.Price.t t
val ema : int -> Value.Price.t t -> Value.Price.t t
val feedback : clock:Clock.t -> ('a t -> 'a t) -> 'a t
val clock : 'a t -> Clock.t

module Internal : sig
  type node_id = int
  type node

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
    | Window of { source : node; size : int }
    | Feedback of node option ref

  val node : 'a t -> node
  val id : node -> node_id
  val clock : node -> Clock.t
  val operation : node -> operation
end
