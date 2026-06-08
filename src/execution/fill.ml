type t = {
  instant : int;
  instrument : string;
  side : Causalis_strategy.Domain.side;
  quantity : Causalis_strategy.Domain.Quantity.t;
  price : Causalis_core.Value.Price.t;
  fee : Causalis_strategy.Domain.Money.t;
}

let create ~instant ~instrument ~side ~quantity ~price ~fee =
  { instant; instrument; side; quantity; price; fee }

let instant fill = fill.instant
let instrument fill = fill.instrument
let side fill = fill.side
let quantity fill = fill.quantity
let price fill = fill.price
let fee fill = fill.fee
