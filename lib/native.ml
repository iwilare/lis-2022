module type Native = sig
  type t

  val max_value : t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( land ) : t -> t -> t
  val ( lor ) : t -> t -> t
  val ( lxor ) : t -> t -> t
  val ( lsl ) : t -> int -> t
  val ( lsr ) : t -> int -> t
  val lnot : t -> t
  val show : t -> string
  val from_int : int -> t
  val to_int : t -> int

  module Overflow : sig
    val ( + ) : t -> t -> t * bool
    val ( - ) : t -> t -> t * bool
    val ( land ) : t -> t -> t * bool
  end
end
