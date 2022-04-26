open Ast

type memory = byte Array.t

let memory_init () = Array.make 65536 0
let memory_get_byte = Array.get
let memory_get m a = Array.get m a + (Array.get m (a + 1) lsl 8)
let memory_set_byte = Array.set

let memory_set m a v =
  v lsr 8 |> Array.set m a;
  v lsl 8 |> Array.set m (a + 1)

type enclave_range = { enclave_start : address; enclave_end : address }

type enclave_layout = {
  (* Must be non-overlapping region *)
  (* isr must not be in the enclave. *)
  (* 0xFFFE must be outside the enclave sections and different from isr. *)
  data : enclave_range;
  code : enclave_range;
}

type cpu_mode = PM | UM

let is_in_enclave_code enc addr =
  enc.code.enclave_start <= addr && addr < enc.code.enclave_end

let is_in_enclave_data enc addr =
  enc.data.enclave_start <= addr && addr < enc.data.enclave_end

let is_enclave_entry_point enc addr = addr = enc.code.enclave_start

type memory_type =
  | EnclaveData
  | EnclaveCode of { is_entry_point : bool }
  | Unprotected

let cpu_mode_of_address enc pc =
  if is_in_enclave_code enc pc then Some PM
  else if is_in_enclave_data enc pc then None
  else Some UM

type memory_right = X | R | W

let get_memory_type enc addr =
  if is_in_enclave_code enc addr then
    EnclaveCode { is_entry_point = is_enclave_entry_point enc addr }
  else if is_in_enclave_data enc addr then EnclaveData
  else Unprotected

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
