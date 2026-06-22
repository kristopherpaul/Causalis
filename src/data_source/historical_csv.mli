type bar

val source_timestamp : bar -> string
val timestamp : bar -> Ptime.t
val logical_instant : bar -> int
val market : bar -> Causalis_strategy.Domain.market
val open_price : bar -> Causalis_core.Value.Price.t
val high_price : bar -> Causalis_core.Value.Price.t
val low_price : bar -> Causalis_core.Value.Price.t
val close_price : bar -> Causalis_core.Value.Price.t
val volume : bar -> Decimal.t option
val open_interest : bar -> Decimal.t option

type t

type error = {
  path : string;
  record : int option;
  message : string;
}

val load_files :
  ?from_timestamp:Ptime.t ->
  ?until_timestamp:Ptime.t ->
  instrument:string ->
  string list ->
  (t, error) result

val bars : t -> bar list
val next : t -> bar option
val reset : t -> unit
val exhausted : t -> bool