type t = {
  instrument : string;
  side : Domain.side;
  quantity : Domain.Quantity.t;
}

let create ~instrument ~side ~quantity = { instrument; side; quantity }
let instrument intent = intent.instrument
let side intent = intent.side
let quantity intent = intent.quantity
