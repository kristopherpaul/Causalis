module Price : sig
  type t = private { value : Decimal.t }
  val of_decimal : Decimal.t -> t
  val to_decimal : t -> Decimal.t
  val zero : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val to_string : t -> string
  val of_string : string -> (t, string) result
end