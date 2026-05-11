open Alcotest

module Price = Causalis_core.Value.Price

let check_decimal actual expected message =
  check string message (Decimal.to_string expected) (Decimal.to_string actual)

let test_zero () =
  let p = Price.zero in
  check_decimal (Price.to_decimal p) Decimal.zero "Price.zero is 0"

let test_of_decimal () =
  let p = Price.of_decimal (Decimal.of_int 100) in
  check_decimal (Price.to_decimal p) (Decimal.of_int 100) "of_decimal roundtrip"

let test_negative_rejected () =
  check_raises "negative prices are rejected"
    (Invalid_argument "Price.of_decimal: negative value")
    (fun () -> ignore (Price.of_decimal (Decimal.(~-Decimal.one))))

let test_add () =
  let a = Price.of_decimal (Decimal.of_int 100) in
  let b = Price.of_decimal (Decimal.of_int 50) in
  let c = Price.(a + b) in
  check_decimal (Price.to_decimal c) (Decimal.of_int 150) "addition"

let test_sub () =
  let a = Price.of_decimal (Decimal.of_int 100) in
  let b = Price.of_decimal (Decimal.of_int 30) in
  let c = Price.(a - b) in
  check_decimal (Price.to_decimal c) (Decimal.of_int 70) "subtraction"

let test_mul () =
  let a = Price.of_decimal (Decimal.of_int 10) in
  let b = Price.of_decimal (Decimal.of_int 5) in
  let c = Price.(a * b) in
  check_decimal (Price.to_decimal c) (Decimal.of_int 50) "multiplication"

let test_div () =
  let a = Price.of_decimal (Decimal.of_int 100) in
  let b = Price.of_decimal (Decimal.of_int 4) in
  let c = Price.(a / b) in
  check_decimal (Price.to_decimal c) (Decimal.of_int 25) "division"

let test_min_max () =
  let a = Price.of_decimal (Decimal.of_int 50) in
  let b = Price.of_decimal (Decimal.of_int 100) in
  check_decimal (Price.to_decimal (Price.min a b)) (Decimal.of_int 50) "min";
  check_decimal (Price.to_decimal (Price.max a b)) (Decimal.of_int 100) "max"

let test_to_string () =
  let p = Price.of_decimal (Decimal.of_string "123.45") in
  check string "to_string" "123.45" (Price.to_string p)

let test_of_string () =
  match Price.of_string "123.45" with
  | Ok p -> check_decimal (Price.to_decimal p) (Decimal.of_string "123.45") "of_string ok"
  | Error _ -> fail "of_string should succeed"

let test_of_string_invalid () =
  match Price.of_string "not-a-number" with
  | Error _ -> ()
  | Ok _ -> fail "of_string should fail on invalid input"

let () =
  run "Price" [
    "Price", [
      test_case "zero" `Quick test_zero;
      test_case "of_decimal" `Quick test_of_decimal;
      test_case "negative_rejected" `Quick test_negative_rejected;
      test_case "add" `Quick test_add;
      test_case "sub" `Quick test_sub;
      test_case "mul" `Quick test_mul;
      test_case "div" `Quick test_div;
      test_case "min_max" `Quick test_min_max;
      test_case "to_string" `Quick test_to_string;
      test_case "of_string" `Quick test_of_string;
      test_case "of_string_invalid" `Quick test_of_string_invalid;
    ]
  ]