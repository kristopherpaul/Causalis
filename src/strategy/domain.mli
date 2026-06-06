type instrument = string

type side =
  | Buy
  | Sell

module Quantity : sig
  type t

  val zero : t
  val of_decimal : Decimal.t -> (t, string) result
  val to_decimal : t -> Decimal.t
  val to_string : t -> string
end

module Money : sig
  type t

  val zero : t
  val of_decimal : Decimal.t -> t
  val to_decimal : t -> Decimal.t
  val to_string : t -> string
end

module Weight : sig
  type t

  val zero : t
  val one : t
  val of_decimal : Decimal.t -> (t, string) result
  val to_decimal : t -> Decimal.t
  val to_string : t -> string
end

type allocation = (instrument * Weight.t) list

val allocation : (instrument * Weight.t) list -> allocation
val allocation_weight : allocation -> instrument -> Weight.t
val allocation_instruments : allocation -> instrument list

type market

val market : prices:(instrument * Causalis_core.Value.Price.t) list -> market
val market_price : market -> instrument -> Causalis_core.Value.Price.t option
val market_prices : market -> (instrument * Causalis_core.Value.Price.t) list

type portfolio_observation

val portfolio_observation :
  cash:Money.t -> equity:Money.t -> positions:(instrument * Quantity.t) list ->
  portfolio_observation

val portfolio_cash : portfolio_observation -> Money.t
val portfolio_equity : portfolio_observation -> Money.t
val portfolio_positions : portfolio_observation -> (instrument * Quantity.t) list
val portfolio_position : portfolio_observation -> instrument -> Quantity.t
