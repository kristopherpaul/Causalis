module Price = struct
  type t = { value : Decimal.t }

  let of_decimal d =
    if Decimal.(d < zero) then
      invalid_arg "Price.of_decimal: negative value"
    else
      { value = d }

  let to_decimal t = t.value

  let zero = { value = Decimal.zero }

  let ( + ) a b = { value = Decimal.(a.value + b.value) }
  let ( - ) a b = { value = Decimal.(a.value - b.value) }
  let ( * ) a b = { value = Decimal.(a.value * b.value) }
  let ( / ) a b = { value = Decimal.(a.value / b.value) }
  let min a b = if Decimal.(a.value <= b.value) then a else b
  let max a b = if Decimal.(a.value >= b.value) then a else b

  let to_string t = Decimal.to_string t.value
  let of_string s =
    try Ok (of_decimal (Decimal.of_string s)) with
    | Invalid_argument message -> Error message
end