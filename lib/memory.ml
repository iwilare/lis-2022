open Types

type memory = byte Array.t

let w0xFFFF = Word.from_int 0xFFFF (* Last byte *)
let w0xFFFE = Word.from_int 0xFFFE (* Last word; also, parity bit mask *)
let w0xFFFC = Word.from_int 0xFFFC (* Last valid word address *)
let limit = 65536 (* Memory size *)
let memory_init () = Array.make limit Byte.zero
let memory_get_byte m (a : address) = Array.get m (Word.to_int a)
let memory_set_byte m (a : address) (b : byte) = Array.set m (Word.to_int a) b

let memory_get m a =
  Word.(
    compose_bytes
      (Array.get m (to_int (a + from_int 1)))
      (Array.get m (to_int a)))

let memory_set m a w =
  Word.(
    decompose_bytes w |> fun (h, l) ->
    l |> Array.set m (to_int a);
    h |> Array.set m (to_int (a + from_int 1)))

let align_even x = Word.(x land w0xFFFE)

let cycles_per_access =
  3 (* TODO: check if this is consistent with interrupt logic in UM case *)

let is_touching_last_word_address (addr : word) =
  addr = w0xFFFE || addr = w0xFFFF

type enclave_range = { enclave_start : address; enclave_end : address }

let string_of_range r =
  "["
  ^ Word.show_address r.enclave_start
  ^ ","
  ^ Word.show_address r.enclave_end
  ^ "]"

type memory_layout = {
  (* Must be non-overlapping region *)
  (* isr must not be in the enclave. *)
  (* 0xFFFE must be outside the enclave sections and different from isr. *)
  data : enclave_range;
  code : enclave_range;
  isr : address;
}

let string_of_layout l =
  "data: " ^ string_of_range l.data ^ " code: " ^ string_of_range l.code
  ^ " isr: " ^ Word.show_address l.isr

(* Memory type *)

type memory_type =
  | EnclaveData
  | EnclaveCode of { is_entry_point : bool }
  | Unprotected

let is_enclave_code enc addr =
  enc.code.enclave_start <= addr && addr < enc.code.enclave_end

let is_enclave_data enc addr =
  enc.data.enclave_start <= addr && addr < enc.data.enclave_end

let is_enclave_entry_point enc addr = addr = enc.code.enclave_start

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
      && mac_region enc f right Word.(t + from_int 1) (bytes - 1)

let mac_word enc f right w = mac_region enc f right w 2
