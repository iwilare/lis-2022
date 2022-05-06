(*

Helper modules to generate relevant data structures and values for tests

NOTE: ALWAYS USE QCHECK2 instead of QCHECK when possible
 *)

module Memory = struct
  open QCheck2.Gen
  open Lis2022.Memory

  let address_in_range a b = a -- b >|= align_even
  let last_valid_address = (limit - 1) - 2
  let address = address_in_range 0 last_valid_address (* Exclude last address *)

  let byte = 0 -- 255
  let word = pair byte byte >|= fun (b1,b2) -> (b1 lsl 8) lor b2
  let memory = array_size (return limit) byte

  let enclave_range a b =
    a -- (b - 1) >>= fun enclave_start ->
    (enclave_start + 1) -- b >>= fun enclave_end ->
    pure { enclave_start; enclave_end }

  let address_out_of_enclave data code =
    oneof [
      address_in_range 0 (min data.enclave_start code.enclave_start);
      address_in_range (min data.enclave_end code.enclave_end) (max data.enclave_start code.enclave_start);
      address_in_range (max data.enclave_end code.enclave_end) (limit - 1)
    ]

  let layout =
    let low = enclave_range 0 (last_valid_address / 2) in
    (* Avoid the first address, = ISR *)
    let high = enclave_range (last_valid_address / 2 + 1) last_valid_address in
    (* Avoid the last address *)
    oneof [ pair low high; pair high low ] >>= fun (data, code) ->
    address_out_of_enclave data code >|= fun isr ->
    { data; code; isr } (* WARNING: ISR is set to always zero in these examples! *)

  let unprotected_address l =
    address_out_of_enclave l.data l.code >|= fun addr ->
    assert (addr != l.isr); (* TODO: introduce a proper generation mechanism *)
    addr

  let protected_code_address l = address_in_range l.code.enclave_start l.code.enclave_end

  let protected_data_address l = address_in_range l.data.enclave_start l.data.enclave_end

  let protected_address layout = oneof [protected_code_address layout; protected_data_address layout]

end

module Register = struct
  open QCheck2.Gen
  open Lis2022.Register_file
  open Lis2022.Ast

  let sr_mask = oneofl [ mask_c; mask_gie; mask_n; mask_v; mask_z ]

  (* Any register *)
  let any_register =
    oneofl
      [ PC; SP; SR; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15 ]

  (* Special registers *)
  let special_register = oneofl [ PC; SP; SR ]

  (* Address word-aligned registers *)
  let address_register = oneofl [ PC; SP ]

  (* General purpose registers *)
  let gp_register =
    oneofl [ R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15 ]

  let any_register_file =
    Memory.address >>= fun pc ->
    Memory.address >>= fun sp ->
    Memory.word >>= fun sr ->
    Memory.word >>= fun r3 ->
    Memory.word >>= fun r4 ->
    Memory.word >>= fun r5 ->
    Memory.word >>= fun r6 ->
    Memory.word >>= fun r7 ->
    Memory.word >>= fun r8 ->
    Memory.word >>= fun r9 ->
    Memory.word >>= fun r10 ->
    Memory.word >>= fun r11 ->
    Memory.word >>= fun r12 ->
    Memory.word >>= fun r13 ->
    Memory.word >>= fun r14 ->
    Memory.word >>= fun r15 ->
    pure {pc; sp; sr; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12; r13; r14; r15}

  let register_file_protected layout =
    Memory.protected_code_address layout >>= fun pc ->
    Memory.protected_code_address layout >>= fun sp ->
    any_register_file >|= fun r -> {r with pc}

  let register_file_unprotected layout =
    Memory.unprotected_address layout >>= fun pc ->
    Memory.unprotected_address layout >>= fun sp ->
    any_register_file >|= fun r -> {r with pc}
end

module Configuration = struct
  open QCheck2.Gen
  open Lis2022.Memory
  open Lis2022.Io_device
  open Lis2022.Configuration

  let default_memory = memory_init ()
  (* Careful! Global because of efficiency, should not be overwritten by tests *)

  let default_io_device = default_io_device
  (* Careful! Global because of efficiency, should not be overwritten by tests *)

  let configuration_unprotected_minimal =
    Memory.layout >>= fun layout ->
    Register.register_file_unprotected layout >|= fun r ->
    {(init_configuration true layout default_io_device default_memory ()) with r}

  let configuration_protected_minimal =
    Memory.layout >>= fun layout ->
    Register.register_file_protected layout >|= fun r ->
    {(init_configuration true layout default_io_device default_memory ()) with r}

  let configuration_minimal = oneof [configuration_unprotected_minimal; configuration_protected_minimal]
end

module Device = struct end
