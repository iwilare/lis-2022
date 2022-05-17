open Native

module Byte : sig
  include Native
  
  val show_hex : t -> string
end

module Word : sig
  include Native

  val show_address : t -> string
  val compose_bytes : Byte.t -> Byte.t -> t
  val decompose_bytes : t -> Byte.t * Byte.t
end

type word = Word.t
type byte = Byte.t
type address = word
type immediate = word
