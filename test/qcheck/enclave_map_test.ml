open Lis2022.Ast
open Lis2022.Config
open Lis2022.Serialization
open Lis2022.Interrupt_logic
open Lis2022.Io_device
open Lis2022.Memory
open Lis2022.Halt_error
open Lis2022.Semantics
open Lis2022.Types
open Generators

module M = Lis2022.Config_monad
module S = Semantics(Sancus_no_pad)

(*
  Attacker context
    - Memory
    - Io device
    - Program

Experiment 1
  begin attacker
    JMP ENCLAVE
  end attacker
  begin isr
    IN R3
    HALT
  end isr
Experiment 2
  begin attacker
    MOV R5 2
    MOV R4 ATTACKER_DATA
    JMP ENCLAVE
  ATTACKER_DATA:
    ...reserve 100 bytes...
  end attacker
  begin isr
    IN R3
    STORE R3 [R4]
    ADD R4 R5
    RETI
  end isr

  - Generate a program in attacker region that reads a lot.
  - Generate a 2 random programs, e1 and e2, in the enclave region N bytes with random timings, both of same total time
  - Generate one (1) security-relevant linear clock-based io_device
    - That always outputs the current clock
    - Sometimes runs an interrupt
  - Run with enclave1.
  - Run with enclave2.
  - Check if any of the attacker registers read with INs differ.

  ---------------------------------------------------------------------
*)

let show_step = function
  | `ok (), c -> string_of_config c
  | `halt e, _ -> "<" ^ string_of_halt_error e ^ ">"

let rec monad_iterate n m =
  match n with
  | 0 -> M.pure ()
  | n -> M.(m >> monad_iterate (n-1) m)

let test_no_pad_enclave_map_build_config enclave base_c =
  let attacker_out_address_in_memory = Word.(base_c.layout.attacker_range.range_end - from_int 1) in
  let attacker_program = [
    MOV_IMM(Word.from_int 42, R3);
    OUT R3; (* interrupt_delay starting *)
    NOP;NOP;NOP;NOP;
    NOP;NOP;NOP;NOP;              (* 8 cycles *)
    MOV_IMM(enclave_start base_c, R3); (* 2 cycles *)
    JMP R3                        (* 2 cycles *)
    (* Total cycles before first interrupt must be equal to interrupt_delay *)
  ] in
  let isr_program = [
    IN R4;                                       (* R4: instr_length *)
    MOV_IMM(Word.from_int 42, R3);               (* R3: 42 *)
    OUT(R8);                                     (* R3: 42, stop the clock to state 1 *)
    MOV_IMM(attacker_out_address_in_memory, R5); (* R4: instr_length R5: &counter *)
    MOV_LOAD(R5, R6);                            (* R4: instr_length R5: &counter, R6: counter *)
    MOV_STORE(R4, R6);                           (* R4: instr_length R5: &counter, R6: counter, instr_length stored at counter *)
    MOV_IMM(Word.from_int 2, R7);                (* R4: instr_length R5: &counter, R6: counter, R7: 2 *)
    ADD(R7, R6);                                 (* R4: instr_length R5: &counter, R6: counter + 2, R7: 2 *)
    MOV_STORE(R6, R5);                           (* R4: instr_length R5: &counter, R6: counter + 2, R7: 2, counter + 2 written at &counter*)
    MOV_IMM(Word.from_int 42, R3);
    OUT(R3);                                     (* Restart the clock *)
    RETI;
  ] in
  let attacker_size = List.fold_left (+) 0 (List.map size attacker_program) in
  let attacker_memory_start = Word.(attacker base_c + from_int attacker_size + from_int 2) in
  let m =
      base_c.m |> encode_and_put_program (isr base_c) isr_program
          |> encode_and_put_program (attacker base_c) attacker_program
          |> encode_and_put_program base_c.layout.enclave_code.range_start enclave
          |> memory_set attacker_out_address_in_memory attacker_memory_start (* Initialize the memory address *)
        in
  (attacker_memory_start,{base_c with m})

let test_no_pad_enclave_map name ~count =
  let property (enclave,c) =
    let (attacker_memory_start,c) = test_no_pad_enclave_map_build_config enclave c in
    let enclave_map = List.map Word.from_int (List.map cycles enclave) in
    let max_steps = 100 in
    let (_, c) = S.run max_steps c in
    memory_get_words attacker_memory_start (List.length enclave_map) c.m == enclave_map
  in
  let gen =
    QCheck2.Gen.(
      let interrupt_delay = 6 + 5 + 1 (* Time between RETI and first enclave instruction *) in
      let max_post_interrupt_clock_length = max_cycles + 20 in
      let n_cycles = 10 in
      let device = circular_on_demand_interrupt_io_device interrupt_delay max_post_interrupt_clock_length in
      let* enclave = Instructions.n_cycles_simple_program n_cycles >|= (fun p -> [NOP] @ p @ [HLT]) in
      let* base_c = Config.config_unprotected_minimal ~io_device:device () in
      pure (enclave,base_c)) in
  QCheck2.Test.make
    ~name:name
    ~count:count
    ~print:(fun (enclave,base_c) ->
        let (_, c) = test_no_pad_enclave_map_build_config enclave base_c in
        "---- Enclave instructions ------\n" ^ String.concat "," (List.map string_of_instr enclave) ^ "\n" ^
        "---- Initial configuration -----\n" ^ string_of_config c ^ "\n" ^
        String.concat "\n" (List.map (fun i -> "\n" ^ string_of_int i ^ "\n" ^ show_step (monad_iterate i (S.auto_step ()) c)) [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20]))
    gen property

let tests =
  [
     test_no_pad_enclave_map "Sancus no_pad allows attackers to fully map instructions timings of the enclave"
     ~count:3000000
  ]
