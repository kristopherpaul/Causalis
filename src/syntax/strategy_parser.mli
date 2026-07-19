type expression =
  | Variable of string
  | Price of string
  | Weight of string
  | Add of expression * expression
  | Subtract of expression * expression
  | Multiply of expression * expression
  | Divide of expression * expression
  | Greater of expression * expression
  | Greater_equal of expression * expression
  | Less of expression * expression
  | Less_equal of expression * expression
  | Pre of expression
  | Sma of int * expression
  | Ema of int * expression
  | If of expression * expression * expression

type declaration =
  | Input of string
  | Let of string * expression
  | Output of string * expression

type strategy = { name : string; declarations : declaration list }

type error = { line : int; column : int; message : string }

exception Parse_error of error

val parse : string -> strategy