(* Unsigned byte *)
module Byte = struct
  type t = int

  let max_value = 0xFF
  let zero = 0
  let one = 1
  let ( + ) x y = (x + y) land max_value
  let ( - ) x y = (x - y) land max_value
  let ( * ) x y = x * y land max_value
  let ( = ) = ( = )
  let ( <> ) = ( <> )
  let ( <= ) = ( <= )
  let ( >= ) = ( >= )
  let ( < ) = ( < )
  let ( > ) = ( > )
  let ( land ) = ( land )
  let ( lor ) = ( lor )
  let ( lxor ) = ( lxor )
  let ( lsl ) x y = (x lsl y) land max_value
  let ( lsr ) = ( lsr )
  let lnot x = lnot x land max_value
  let to_int = Fun.id
  let show = Printf.sprintf "%5d"
  let show_hex = Printf.sprintf "%#2X"

  module Overflow = struct
    let is_overflow v = not (0 <= v && v <= max_value)
    let ( + ) x y = ((x + y) land max_value, is_overflow (x + y))
    let ( - ) x y = ((x - y) land max_value, is_overflow (x - y))
    let ( land ) x y = (x land y, is_overflow (x land y))
  end

  let from_int v =
    assert (not (Overflow.is_overflow v));
    v

  let ( #> ) = from_int
end

(* Unsigned word *)
module Word = struct
  type t = int

  let max_value = 0xFFFF
  let zero = 0
  let one = 1
  let ( + ) x y = (x + y) land max_value
  let ( - ) x y = (x - y) land max_value
  let ( * ) x y = x * y land max_value
  let ( = ) = ( = )
  let ( <> ) = ( <> )
  let ( <= ) = ( <= )
  let ( >= ) = ( >= )
  let ( < ) = ( < )
  let ( > ) = ( > )
  let ( land ) = ( land )
  let ( lor ) = ( lor )
  let ( lxor ) = ( lxor )
  let ( lsl ) x y = (x lsl y) land max_value
  let ( lsr ) = ( lsr )
  let lnot x = lnot x land max_value
  let decompose_bytes w = ((w lsr 8) land Byte.max_value, w land Byte.max_value)
  let compose_bytes h l = (h lsl 8) lor l
  let show = Printf.sprintf "%5d"
  let to_int = Fun.id
  let show_address = Printf.sprintf "%#5X"

  module Overflow = struct
    let is_overflow v = not (0 <= v && v <= max_value)
    let ( + ) x y = ((x + y) land max_value, is_overflow (x + y))
    let ( - ) x y = ((x - y) land max_value, is_overflow (x - y))
    let ( land ) x y = (x land y, is_overflow (x land y))
  end

  let from_int v =
    assert (not (Overflow.is_overflow v));
    v

  let ( #> ) = from_int
end

type word = Word.t
type byte = Byte.t
type address = word
type immediate = word
