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

type address_range = {range_start: address; range_end: address}

let is_in_range range addr =
  range.range_start <= addr && addr < range.range_end

let string_of_range r =
  "["
  ^ Word.show_address r.range_start
  ^ ","
  ^ Word.show_address r.range_end
  ^ "]"

type memory_layout = {
  (* Must be non-overlapping region *)
  (* isr must not be in the enclave. *)
  (* 0xFFFE must be outside the enclave sections and different from isr. *)
  enclave_data : address_range;
  enclave_code : address_range;
  attacker_range : address_range;
  isr_range : address_range;
}

let string_of_layout l =
  "data: " ^ string_of_range l.enclave_data ^ " code: " ^ string_of_range l.enclave_code
  ^ " isr: " ^ Word.show_address l.isr_range.range_start

(* Memory type *)

type memory_type =
  | EnclaveData
  | EnclaveCode of { is_entry_point : bool }
  | Unprotected

let is_enclave_code layout = is_in_range layout.enclave_code

let is_enclave_data layout = is_in_range layout.enclave_data

(*
  During memory access control checks, we check that the code at the enclave entry
  point is executable. Since all instructions are at least two bytes long, both
  bytes of the instruction must be marked as executable.
*)
let is_enclave_entry_point layout addr =
     addr = layout.enclave_code.range_start
  || addr = Word.inc layout.enclave_code.range_start

let get_memory_type enc addr =
  if is_enclave_code enc addr then
    EnclaveCode { is_entry_point = is_enclave_entry_point enc addr }
  else if is_enclave_data enc addr then EnclaveData
  else Unprotected

(* CPU mode *)

type cpu_mode = PM | UM

let cpu_mode_of_address enc pc =
  match get_memory_type enc pc with
  | EnclaveCode _ -> Some PM
  | EnclaveData -> None
  | Unprotected -> Some UM

(* Memory access control *)

type memory_right = X | R | W

let permissions enc f t =
  match (get_memory_type enc f, get_memory_type enc t) with
  | EnclaveCode _, EnclaveCode _ -> [ R; X ]
  | EnclaveCode _, EnclaveData -> [ R; W ]
  | EnclaveCode _, Unprotected -> [ X ]
  | _, EnclaveCode { is_entry_point = true } -> [ X ]
  | _, EnclaveData -> []
  | _, EnclaveCode _ -> []
  | _ -> [ R; W; X ]

let mac enc f right t = List.mem right (permissions enc f t)

let rec mac_region enc f right t bytes =
  match bytes with
  | 0 -> true
  | _ ->
      mac enc f right t
      && mac_region enc f right (Word.inc t) (bytes - 1)

let mac_word enc f right w = mac_region enc f right w 2
