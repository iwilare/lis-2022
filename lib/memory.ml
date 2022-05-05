open Ast

type memory = byte Array.t

let limit = 65536
let is_overflow v = v < 0 || limit <= v
let memory_init () = Array.make limit 0
let memory_get_byte = Array.get
let memory_get m a = Array.get m a + (Array.get m (a + 1) lsl 8)
let memory_set_byte = Array.set

let memory_set m a v =
  v land 255 |> Array.set m a;
  v lsr 8 |> Array.set m (a + 1)

let cycles_per_access =
  3 (* TODO: check if this is consistent with interrupt logic in UM case *)

let is_touching_last_word_address addr = addr == limit - 2 || addr == limit - 1

type enclave_range = { enclave_start : address; enclave_end : address }

type memory_layout = {
  (* Must be non-overlapping region *)
  (* isr must not be in the enclave. *)
  (* 0xFFFE must be outside the enclave sections and different from isr. *)
  data : enclave_range;
  code : enclave_range;
  isr : address;
}

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
let mac_word enc f right w = mac enc f right w && mac enc f right (w + 1)

let rec mac_bytes enc f right t bytes =
  match bytes with
  | 0 -> true
  | _ -> mac enc f right t && mac_bytes enc f right (t + 1) (bytes - 1)
