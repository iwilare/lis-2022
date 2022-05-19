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

  let address_in_range r =
    let* w = to_int r.range_start -- to_int r.range_end in
    pure (align_even (from_int w))

  let address = address_in_range {range_start = Word.zero; range_end = w0xFFFE}

  let (memory : memory QCheck2.Gen.t) = pure [] (*array_size (pure limit) byte*)
  (* TODO: fix this. This causes QCheck to hang when generating a memory during
     a failing test, no idea why. *)

  (* Global values for layout generation *)
  let layout =
    let sec1 = {range_start = from_int 0x0000; range_end = from_int 0x3FFF} in
    let sec2 = {range_start = from_int 0x4000; range_end = from_int 0x7FFF} in
    let sec3 = {range_start = from_int 0x8000; range_end = from_int 0xBFFF} in
    let sec4 = {range_start = from_int 0xC000; range_end = from_int 0xFFFC} in (* Avoid 0xFFFE *)
    let* shuf = QCheck2.Gen.shuffle_l [sec1; sec2; sec3; sec4] in
    match shuf with
    | [attacker_range; enclave_data; enclave_code; isr_range] ->
      pure { enclave_data; enclave_code; attacker_range; isr_range; }
    | _ -> failwith "<IMPOSSIBLE>"

  let attacker_range l = address_in_range l.attacker_range

  let unprotected_address l =
    oneof [address_in_range l.attacker_range; address_in_range l.isr_range]

  let protected_address l = address_in_range l.enclave_code

  let protected_data_address l = address_in_range l.enclave_data

  let any_protected_address l =
    oneof [address_in_range l.enclave_data; address_in_range l.enclave_code]
end

module Register = struct
  open QCheck2.Gen
  open Lis2022.Register_file
  open Lis2022.Ast
  open Lis2022.Memory
  open Lis2022.Types.Word

  let sr_mask = oneofl [ mask_c; mask_gie; mask_n; mask_v; mask_z ]

  (* GIE is always activated by default *)
  let sr_register_value =
    let* c = QCheck2.Gen.bool in
    let* n = QCheck2.Gen.bool in
    let* v = QCheck2.Gen.bool in
    let* z = QCheck2.Gen.bool in
    pure
      ((if c then mask_c else zero)
      lor (if n then mask_n else zero)
      lor (if v then mask_v else zero)
      lor (if z then mask_z else zero)
      lor mask_gie)

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

  let register_file l pc =
    let* pc in
    let sp = l.attacker_range.range_end in
    let* sr = sr_register_value in
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
    pure { pc; sp; sr; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12; r13; r14; r15 }

  let register_file_protected l = register_file l (Memory.protected_address l)

  let register_file_unprotected l = register_file l (Memory.unprotected_address l)
end

module Config = struct
  open QCheck2.Gen
  open Lis2022.Memory
  open Lis2022.Io_device
  open Lis2022.Config

  let default_memory = memory_init ()

  let default_io_device = default_io_device

  let t_pad = 0 -- Lis2022.Ast.max_cycles

  let backup layout =
    let* pc_old = Memory.protected_address layout in
    let* r = Register.register_file_protected layout in
    let* t_pad = t_pad in
    pure { r; pc_old; t_pad }

  let config_unprotected_minimal ?(io_device = default_io_device) () =
    let* layout = Memory.layout in
    let* pc_old = Memory.unprotected_address layout in
    let* r = Register.register_file_unprotected layout in
    let* b = opt (backup layout) in
    let m = default_memory in
    pure
      {
        io_state = io_device.init_state;
        current_clock = 0;
        arrival_time = None;
        io_device;
        layout;
        pc_old;
        m;
        b;
        r;
        exception_happened = false;
      }

  let config_protected_minimal ?(io_device = default_io_device) () =
    let* layout = Memory.layout in
    let* pc_old = Memory.protected_address layout in
    let* r = Register.register_file_protected layout in
    let b = None in (* Always none because we are in protected mode *)
    let m = default_memory in
    pure
      {
        io_state = io_device.init_state;
        current_clock = 0;
        arrival_time = None;
        io_device;
        layout;
        pc_old;
        m;
        b;
        r;
        exception_happened = false;
      }

  let any_config_minimal =
    oneof [ config_unprotected_minimal (); config_protected_minimal (); ]
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
        write_transitions = List.combine all_words all_write_transitions;
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
        delta = List.combine states all_transitions;
      }

  let security_relevant_delta_transitions states when_interrupt =
    states |> List.map succ
    |> List.mapi (fun i next_state ->
           {
             main_transition =
               (if i == when_interrupt then InterruptTransition next_state
               else EpsilonTransition next_state);
             read_transition = Some (Word.from_int i, next_state);
             write_transitions = [];
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
        delta = List.combine states (security_relevant_delta_transitions states when_interrupt);
      }
end

module Instructions = struct
  open Lis2022.Ast
  open QCheck2.Gen
  open Lis2022.Types

  let instr_generators =
    [ NOP, pure NOP
    ; RETI, pure RETI
    ; HLT, pure HLT
    ; IN(R3), Register.gp_register >|= (fun r -> IN r)
    ; OUT(R3), Register.gp_register >|= (fun r -> OUT r)
    ; JMP(R3), Register.gp_register >|= (fun r -> JMP r)
    ; JZ(R3), Register.gp_register >|= (fun r -> JZ r)
    ; MOV(R3,R3), Register.gp_register >>= (fun r1 -> Register.gp_register >|= fun r2 -> MOV(r1,r2))
    ; MOV_LOAD(R3,R3), Register.gp_register >>= (fun r1 -> Register.gp_register >|= fun r2 -> MOV_LOAD(r1,r2))
    ; MOV_STORE(R3,R3), Register.gp_register >>= (fun r1 -> Register.gp_register >|= fun r2 -> MOV_STORE(r1,r2))
    ; MOV_IMM(Word.zero,R3), Register.gp_register >>= (fun r ->  Memory.word >|= fun immediate -> MOV_IMM(immediate,r))
    ; NOT(R3), Register.gp_register >|= (fun r1 -> NOT(r1))
    ; ADD(R3,R3), Register.gp_register >>= (fun r1 -> Register.gp_register >|= fun r2 -> ADD(r1,r2))
    ; SUB(R3,R3), Register.gp_register >>= (fun r1 -> Register.gp_register >|= fun r2 -> SUB(r1,r2))
    ; AND(R3,R3), Register.gp_register >>= (fun r1 -> Register.gp_register >|= fun r2 -> AND(r1,r2))
    ; CMP(R3,R3), Register.gp_register >>= (fun r1 -> Register.gp_register >|= fun r2 -> CMP(r1,r2))
    ]

  let instr = oneof (List.map snd instr_generators)

  let simple_instructions = List.filter (fun (p,_) -> is_simple_instr p) instr_generators

  let n_cycles_simple_instruction n = oneof @@ List.filter_map (fun (p,i) -> if cycles p = n then Some i else None) simple_instructions

  let rec n_cycles_simple_program n =
    let gen max_cycles =
      let* i_1_cycles = n_cycles_simple_instruction 1 in
      let* i_2_cycles = n_cycles_simple_instruction 2 in
      let* (i, c) = oneofl ([i_1_cycles, 1] @ (if max_cycles = 2 then [i_2_cycles, 2] else [])) in
      let* rest = n_cycles_simple_program (n - c) in
      pure (i :: rest) in
    match n with
    | n when n > 2 -> gen 2
    | n when n = 1 -> gen 1
    | _ -> pure []
end