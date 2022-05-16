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
    let* w = to_int a -- to_int b in
    pure (align_even (from_int w))

  let address = address_in_range zero w0xFFFC (* Exclude last address *)
  let memory = array_size (pure limit) byte
  (* TODO: fix this. This causes QCheck to hang when generating a memory during
     a failing test, no idea why. *)

  let enclave_range a b =
    let* enclave_start = address_in_range a (b - from_int 1) in
    let* enclave_end = address_in_range (enclave_start + from_int 1) b in
    pure { enclave_start; enclave_end }

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
    let* data, code = oneof [ pair low high; pair high low ] in
    let* isr = address_out_of_enclave data code in
    pure { data; code; isr }
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

  (* Avoid settings GIE without context on the mode *)
  let sr_register_value =
    let* c = QCheck2.Gen.bool in
    let* n = QCheck2.Gen.bool in
    let* v = QCheck2.Gen.bool in
    let* z = QCheck2.Gen.bool in
    pure
      ((if c then mask_c else zero)
      lor (if n then mask_n else zero)
      lor (if v then mask_v else zero)
      lor if z then mask_z else zero)

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
    let* pc = Memory.address in
    let* sp = Memory.address in
    let* sr = Memory.word in
    let* r3 = Memory.word in
    let* r4 = Memory.word in
    let* r5 = Memory.word in
    let* r6 = Memory.word in
    let* r7 = Memory.word in
    let* r8 = Memory.word in
    let* r9 = Memory.word in
    let* r10 = Memory.word in
    let* r11 = Memory.word in
    let* r12 = Memory.word in
    let* r13 = Memory.word in
    let* r14 = Memory.word in
    let* r15 = Memory.word in
    pure
      { pc; sp; sr; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12; r13; r14; r15 }

  let register_file_protected layout =
    let* pc = Memory.protected_address layout in
    (* SP always points to an unprotected location *)
    let* sp = Memory.unprotected_address layout in
    let* sr = sr_register_value in
    let* r = any_register_file in
    pure { r with pc; sp; sr }

  let register_file_unprotected layout =
    let* pc = Memory.unprotected_address layout in
    let* sp = Memory.unprotected_address layout in
    let* sr = sr_register_value in
    let* r = any_register_file in
    pure { r with pc; sp; sr }
end

module Configuration = struct
  open QCheck2.Gen
  open Lis2022.Memory
  open Lis2022.Io_device
  open Lis2022.Register_file
  open Lis2022.Configuration

  let default_memory = memory_init ()
  (* Careful! Global because of efficiency, should not be overwritten by tests *)

  let default_io_device = default_io_device
  (* Careful! Global because of efficiency, should not be overwritten by tests *)

  let t_pad = 0 -- Lis2022.Ast.max_cycles

  let backup layout =
    let* pc_old = Memory.protected_address layout in
    let* r = Register.register_file_protected layout in
    let* t_pad = t_pad in
    pure { r; pc_old; t_pad }

  let configuration_unprotected_minimal ?(io_device = default_io_device) () =
    let* layout = Memory.layout in
    let* r = Register.register_file_unprotected layout in
    let* pc_old = Memory.unprotected_address layout in
    let* b = opt (backup layout) in
    (* If the backup is Some(...) then set GIE to zero in the configuration *)
    let set_gie_zero_if_backup =
      Option.fold b ~none:Fun.id ~some:(Fun.const (set_bit mask_gie false))
    in
    pure
      {
        (init_configuration true layout io_device default_memory ()) with
        pc_old;
        r = { r with sr = set_gie_zero_if_backup r.sr };
        b;
      }

  let configuration_protected_minimal ?(io_device = default_io_device) () =
    let* layout = Memory.layout in
    let* r = Register.register_file_protected layout in
    let* pc_old = Memory.protected_address layout in
    pure
      {
        (init_configuration true layout io_device default_memory ()) with
        pc_old;
        r;
        b = None;
      }

  let any_configuration_minimal =
    oneof
      [
        configuration_unprotected_minimal (); configuration_protected_minimal ();
      ]
end

module Io_device = struct
  open QCheck2.Gen
  open Lis2022.Io_device
  open Lis2022.Types

  let io_state max = 1 -- max

  let transition_type max =
    let* s = io_state max in
    oneofl [ EpsilonTransition s; InterruptTransition s ]

  let io_possibilities states write_transitions =
    let* main_transition = transition_type states in
    let* read_transition = opt (pair Memory.word (io_state states)) in
    let* all_write_transitions =
      list_size (pure write_transitions) (io_state states)
    in
    let all_words = List.map Word.from_int (List.init write_transitions succ) in
    pure
      {
        main_transition;
        read_transition;
        write_transitions =
          (fun s ->
            List.assoc_opt s (List.combine all_words all_write_transitions));
      }

  let device states write_transitions =
    let* init_state = io_state states in
    let* all_transitions =
      list_size (pure states) (io_possibilities states write_transitions)
    in
    let states = List.init states succ in
    pure
      {
        states;
        init_state;
        delta = (fun s -> List.assoc s (List.combine states all_transitions));
      }

  let security_relevant_delta_transitions states when_interrupt =
    states |> List.map succ
    |> List.mapi (fun i next_state ->
           {
             main_transition =
               (if i == when_interrupt then InterruptTransition next_state
               else EpsilonTransition next_state);
             read_transition = Some (Word.from_int i, next_state);
             write_transitions = Fun.const None;
           })

  (*
    Linear automaton in which:
      - each transition is epsilon except for one at time `when_interrupt`
      - each transition points to the next state
      - always outputs the elapsed time from the start as read transition
  *)
  let security_relevant_device states when_interrupt =
    let states = List.init states succ in
    pure
      {
        states;
        init_state = 0;
        delta =
          (fun s ->
            List.assoc s
              (List.combine states
                 (security_relevant_delta_transitions states when_interrupt)));
      }
end

module Instructions = struct
  open Lis2022.Ast
  open QCheck2.Gen

  let reg1_instr =
    let* r = Register.gp_register in
    oneofl [ IN r; OUT r; JMP r; JZ r; NOT r ]

  let reg2_instr =
    let* r1 = Register.gp_register in
    let* r2 = Register.gp_register in
    oneofl
      [
        MOV (r1, r2);
        MOV_LOAD (r1, r2);
        MOV_STORE (r1, r2);
        AND (r1, r2);
        ADD (r1, r2);
        SUB (r1, r2);
        CMP (r1, r2);
      ]

  let move_immediate =
    map2 (fun i r -> MOV_IMM (i, r)) Memory.word Register.gp_register

  let random_inst =
    oneof
      [ pure HLT; pure NOP; pure RETI; reg1_instr; reg2_instr; move_immediate ]
end
