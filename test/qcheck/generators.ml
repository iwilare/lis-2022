(*

Helper modules to generate relevant data structures and values for tests

NOTE: ALWAYS USE QCHECK2 instead of QCHECK when possible
 *)

module Memory = struct
  open QCheck2.Gen
  open Lis2022.Memory

  let address = int_range 0 (limit - 1)
  let byte = int_range 0 255
  let memory = array_size (return limit) byte

  let range a b =
    int_range a (b - 1) >>= fun enclave_start ->
    int_range (enclave_start + 1) b >>= fun enclave_end ->
    pure { enclave_start; enclave_end }

  (* Suppose that ISR always starts at zero! *)
  let layout =
    let low = range 2 (limit / 2) in
    (* Avoid the first address, = ISR *)
    let high = range (limit / 2) (limit - 2) in
    (* Avoid the last address *)
    oneof [ pair low high; pair high low ] >|= fun (data, code) ->
    { data; code; isr = 0 }
  (* WARNING: ISR is set to always zero in these examples! *)
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

  let configuration_minimal =
    Memory.layout >|= fun layout ->
    init_configuration true layout default_io_device default_memory ()
end

module Device = struct end
