open Memory
open Types

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
