(*

Helper modules to generate relevant data structures and values for tests

NOTE: ALWAYS USE QCHECK2 instead of QCHECK when possible
*)

module Memory = struct
  open QCheck2.Gen
  open Lis2022.Memory
  open Lis2022.Types
  open Lis2022.Layout
  open Lis2022.Types.Word

  let word = 0x0000 -- 0xFFFF >|= from_int
  let byte = 0x00 -- 0xFF >|= Byte.from_int
  let last_valid_address = w0xFFFC

  let address_in_range ?(availability = 0) r =
    let* w = to_int r.range_start -- Stdlib.(to_int r.range_end - availability) in
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

  let protected_address ?(availability = 0) l = address_in_range l.enclave_code ~availability:availability

  let protected_data_address l = address_in_range l.enclave_data

  let unprotected_address ?(availability = 0) l =
    oneof [address_in_range l.attacker_range ~availability:availability; address_in_range l.isr_range ~availability:availability]

  let any_protected_address l =
    oneof [address_in_range l.enclave_data; address_in_range l.enclave_code]
end

module Register = struct
  open QCheck2.Gen
  open Lis2022.Register_file
  open Lis2022.Memory
  open Lis2022.Layout
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

  let register_file_protected ?(pc_availability = 0) l = register_file l (Memory.protected_address l ~availability:pc_availability)

  let register_file_unprotected ?(pc_availability = 0) l = register_file l (Memory.unprotected_address l ~availability:pc_availability)
end

module Config = struct
  open QCheck2.Gen
  open Lis2022.Memory
  open Lis2022.Io_device
  open Lis2022.Config
  open Lis2022.Instr
  open Lis2022.Register_file

  let default_memory = memory_init ()

  let default_io_device = default_io_device

  let t_pad = 0 -- Lis2022.Instr.max_cycles

  let backup layout =
    let* pc_old = Memory.protected_address layout in
    let* r = Register.register_file_protected layout in
    let* t_pad = t_pad in
    pure { r; pc_old; t_pad }

  (* Set at attacker position with backup = None *)
  let attacker_config ?(io_device = default_io_device) () =
    let* layout = Memory.layout in
    (* PC availability is overridden anyway *)
    let* r = Register.register_file_unprotected layout ~pc_availability:0 in
    let m = default_memory in
    pure
      {
        io_state = io_device.init_state;
        current_clock = 1;
        arrival_time = None;
        io_device;
        layout;
        pc_old = w0xFFFE;
        m;
        b = None; (* Start with no backup *)
        r = { r with pc = layout.attacker_range.range_start }; (* Start at the attacker position *)
      }

  (* One instruction with max length availability *)
  let any_config_unprotected_no_mem ?(io_device = default_io_device) ?(pc_availability = max_instruction_size - 1) () =
    let* layout = Memory.layout in
    let* pc_old = Memory.unprotected_address layout in
    let* r = Register.register_file_unprotected layout ~pc_availability:pc_availability in
    let* b = opt (backup layout) in
    let m = default_memory in
    (* If the backup is Some(...) then set GIE to zero in the config *)
    let set_gie_zero_if_backup =
      Option.fold b ~none:Fun.id ~some:(Fun.const (set_bit mask_gie false)) in
    pure
      {
        io_state = io_device.init_state;
        current_clock = 1;
        arrival_time = None;
        io_device;
        layout;
        pc_old;
        m;
        b;
        r = { r with sr = set_gie_zero_if_backup r.sr };
      }

  (* One instruction with max length availability *)
  let any_config_protected_no_mem ?(io_device = default_io_device) ?(pc_availability = max_instruction_size - 1) () =
    let* layout = Memory.layout in
    let* pc_old = Memory.protected_address layout in
    let* r = Register.register_file_protected layout ~pc_availability:pc_availability in
    let b = None in (* Always None because we are in protected mode *)
    let m = default_memory in
    pure
      {
        io_state = io_device.init_state;
        current_clock = 1;
        arrival_time = None;
        io_device;
        layout;
        pc_old;
        m;
        b;
        r;
      }

  let any_config_no_mem =
    oneof [ any_config_unprotected_no_mem (); any_config_protected_no_mem (); ]
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
end

module Instructions = struct
  open Lis2022.Instr
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

  let n_cycles_simple_instruction n =
    oneof @@ List.filter_map (fun (p,i) -> if cycles p = n then Some i else None) simple_instructions

  let rec n_cycles_simple_program n =
    let gen cycles_options =
      let* (i, c) = frequency (List.map (fun c ->
        (* The probability weight of the instruction is also its execution time :D *)
        ((if c = 1 then 1 else 3), n_cycles_simple_instruction c >|= fun i -> (i, c))) cycles_options) in
      let* rest = n_cycles_simple_program (n - c) in
      pure (i :: rest) in
    match n with
    | n when n >= 2 -> gen [1; 2]
    | n when n = 1 -> gen [1]
    | _ -> pure []
end
