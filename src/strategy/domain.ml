type instrument = string

type side =
  | Buy
  | Sell

module Quantity = struct
  type t = Decimal.t

  let zero = Decimal.zero

  let of_decimal value =
    if Decimal.(value < zero) then Error "quantity must be non-negative"
    else Ok value

  let to_decimal value = value
  let to_string value = Decimal.to_string value
end

module Position = struct
  type t = Decimal.t

  let zero = Decimal.zero
  let of_decimal value = value
  let to_decimal value = value
  let to_string value = Decimal.to_string value
end

module Money = struct
  type t = Decimal.t

  let zero = Decimal.zero
  let of_decimal value = value
  let to_decimal value = value
  let to_string value = Decimal.to_string value
end

module Weight = struct
  type t = Decimal.t

  let zero = Decimal.zero
  let one = Decimal.one

  let of_decimal value =
    if Decimal.(value < of_int (-1) || value > one) then
      Error "weight must be between negative one and one"
    else
      Ok value

  let to_decimal value = value
  let to_string value = Decimal.to_string value
end

type allocation = (instrument * Weight.t) list

let allocation entries =
  let rec add seen = function
    | [] -> List.rev seen
    | (instrument, weight) :: rest ->
        if List.mem_assoc instrument seen then
          invalid_arg "Domain.allocation: duplicate instrument"
        else
          add ((instrument, weight) :: seen) rest
  in
  add [] entries

let allocation_weight target instrument =
  match List.assoc_opt instrument target with
  | Some weight -> weight
  | None -> Weight.zero

let allocation_instruments target = List.map fst target

type market = { prices : (instrument * Causalis_core.Value.Price.t) list }

let market ~prices =
  let rec validate seen = function
    | [] -> { prices }
    | (instrument, _) :: rest ->
        if List.mem instrument seen then
          invalid_arg "Domain.market: duplicate instrument"
        else
          validate (instrument :: seen) rest
  in
  validate [] prices

let market_price market instrument = List.assoc_opt instrument market.prices
let market_prices market = market.prices

type portfolio_observation = {
  cash : Money.t;
  equity : Money.t;
  positions : (instrument * Position.t) list;
}

let portfolio_observation ~cash ~equity ~positions =
  { cash; equity; positions }

let portfolio_cash observation = observation.cash
let portfolio_equity observation = observation.equity
let portfolio_positions observation = observation.positions

let portfolio_position observation instrument =
  match List.assoc_opt instrument observation.positions with
  | Some quantity -> quantity
  | None -> Position.zero
