(*

Helper modules to generate relevant data structures and values for tests

NOTE: ALWAYS USE QCHECK2 instead of QCHECK when possible
*)

module Memory = struct
  open QCheck2.Gen
  open Lis2022.Memory
  open Lis2022.Types
  open Lis2022.Types.Word

  let word = 0x0000 -- 0xFFFF >|= from_int
  let byte = 0x00 -- 0xFF >|= Byte.from_int
  let last_valid_address = w0xFFFC

  let address_in_range a b =
    to_int a -- to_int b >|= fun w -> align_even (from_int w)

  let address = address_in_range zero w0xFFFC (* Exclude last address *)
  let memory = array_size (pure limit) byte
  (* TODO: fix this. This causes QCheck to hang when generating a memory during
     a failing test, no idea why. *)

  let enclave_range a b =
    address_in_range a (b - from_int 1) >>= fun enclave_start ->
    address_in_range (enclave_start + from_int 1) b >|= fun enclave_end ->
    { enclave_start; enclave_end }

  let address_out_of_enclave data code =
    oneof
      [
        address_in_range zero (min data.enclave_start code.enclave_start);
        address_in_range
          (min data.enclave_end code.enclave_end)
          (max data.enclave_start code.enclave_start);
        address_in_range
          (max data.enclave_end code.enclave_end)
          last_valid_address;
      ]

  let layout =
    let low = enclave_range zero (last_valid_address lsr 1) in
    (* Avoid the first address, = ISR *)
    let high =
      enclave_range ((last_valid_address lsr 1) + from_int 1) last_valid_address
    in
    (* Avoid the last address *)
    oneof [ pair low high; pair high low ] >>= fun (data, code) ->
    address_out_of_enclave data code >|= fun isr -> { data; code; isr }
  (* WARNING: ISR is set to always zero in these examples! *)

  let unprotected_address l =
    address_out_of_enclave l.data l.code >|= fun addr ->
    (*assert (addr != l.isr);*)
    (* TODO: introduce a proper generation mechanism *)
    addr

  let protected_address l =
    address_in_range l.code.enclave_start l.code.enclave_end

  let protected_data_address l =
    address_in_range l.data.enclave_start l.data.enclave_end

  let any_protected_address layout =
    oneof [ protected_address layout; protected_data_address layout ]
end

module Register = struct
  open QCheck2.Gen
  open Lis2022.Register_file
  open Lis2022.Ast
  open Lis2022.Types.Word

  let sr_mask = oneofl [ mask_c; mask_gie; mask_n; mask_v; mask_z ]

  let sr_register_value =
    QCheck2.Gen.bool >>= fun c ->
    QCheck2.Gen.bool >>= fun gie ->
    QCheck2.Gen.bool >>= fun n ->
    QCheck2.Gen.bool >>= fun v ->
    QCheck2.Gen.bool >>= fun z ->
    pure ((if c then mask_c else zero)
      lor (if gie then mask_gie else zero)
      lor (if n then mask_n else zero)
      lor (if v then mask_v else zero)
      lor (if z then mask_z else zero))

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
    pure
      { pc; sp; sr; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12; r13; r14; r15 }

  let register_file_protected layout =
    Memory.protected_address layout >>= fun pc ->
    Memory.unprotected_address layout >>= fun sp -> (* SP always points to an unprotected location *)
    sr_register_value >>= fun sr ->
    any_register_file >|= fun r -> { r with pc; sp; sr }

  let register_file_unprotected layout =
    Memory.unprotected_address layout >>= fun pc ->
    Memory.unprotected_address layout >>= fun sp ->
    sr_register_value >>= fun sr ->
    any_register_file >|= fun r -> { r with pc; sp; sr }
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

  let configuration_minimal register_file_gen pc_old_gen =
    Memory.layout >>= fun layout ->
    register_file_gen layout >>= fun r ->
    pc_old_gen layout >|= fun pc_old ->
    {
      (init_configuration true layout default_io_device default_memory ()) with
      pc_old;
      r;
    }

  let configuration_unprotected_minimal =
    configuration_minimal Register.register_file_unprotected Memory.unprotected_address

  let configuration_protected_minimal =
      configuration_minimal Register.register_file_protected Memory.protected_address

  let any_configuration_minimal =
    oneof [ configuration_unprotected_minimal; configuration_protected_minimal ]
end

module Device = struct end
