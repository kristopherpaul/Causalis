type t

val create : instrument:string -> side:Domain.side ->
  quantity:Domain.Quantity.t -> t
val instrument : t -> string
val side : t -> Domain.side
val quantity : t -> Domain.Quantity.t
