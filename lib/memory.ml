open Types

type memory = (address * byte) list

let w0xFFFF = Word.from_int 0xFFFF (* Last byte *)
let w0xFFFE = Word.from_int 0xFFFE (* Last word; also, parity bit mask *)
let w0xFFFC = Word.from_int 0xFFFC (* Last valid word address *)
let limit = 65536 (* Memory size *)
let memory_init () = []
let memory_get_byte (a : address) (m : memory) =
  if Word.zero <= a && a <= w0xFFFF then
    Option.value (List.assoc_opt a m) ~default:Byte.zero
  else
    failwith "Invalid memory access"

let memory_set_byte (a : address) (b : byte) (m : memory) =
  m |> List.remove_assoc a
    |> List.cons (a, b)

let memory_get (a : address) (m : memory) =
  Word.(
    compose_bytes
      (memory_get_byte (Word.inc a) m))
      (memory_get_byte a m)

let memory_set (a : address) (w : word) (m : memory) =
    Word.decompose_bytes w |> fun (h, l) ->
      m |> memory_set_byte a l
        |> memory_set_byte (Word.inc a) h

let rec memory_set_words (a : address) (ws : word list) (m : memory) =
  match ws with
  | [] -> m
  | w::ws -> memory_set_words (Word.inc2 a) ws (memory_set a w m)

let rec memory_get_words (a : address) (k : int) (m : memory) : word list =
  match k with
  | 0 -> []
  | k -> memory_get a m :: memory_get_words (Word.inc2 a) (k - 1) m

let string_of_memory m =
  m |> List.sort (fun (a,_) (b,_) -> Word.to_int a - Word.to_int b)
    |> List.map (fun (a,b) -> Word.show_address a ^ " -> " ^ Byte.show_hex b)
    |> String.concat "\n"

(* Memory logic *)

let align_even x = Word.(x land w0xFFFE)

let is_touching_last_word_address (addr : word) =
  addr = w0xFFFE || addr = w0xFFFF

type address_range = {
  range_start: address;
  range_end: address
}

let is_in_range range addr =
  range.range_start <= addr && addr < range.range_end

let string_of_range r =
  "["
  ^ Word.show_address r.range_start
  ^ ","
  ^ Word.show_address r.range_end
  ^ "]"
