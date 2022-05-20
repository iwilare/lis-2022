open Lis2022.Ast
open Lis2022.Config
open Lis2022.Instructions
open Lis2022.Interrupt_logic
open Lis2022.Io_device
open Lis2022.Memory
open Lis2022.Halt_error
open Lis2022.Register_file
open Lis2022.Semantics
open Lis2022.Types
open Generators

module M = Lis2022.Config_monad

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

module Non_interference (I : Interrupt_logic) = struct
  module S = Semantics(I)
  let test_non_interference name ~count ~attacker_program ~isr_program ~enclave_epilogue ~n_cycles ~io_device_states =
    let property (enclave1,enclave2,c,_,_) =
      let max_steps = 100 in
      let context_memory =
          c.m |> encode_and_put_program (isr c) (isr_program c)
              |> encode_and_put_program (attacker c) (attacker_program c) in
      let full_memory1 = context_memory |> encode_and_put_program c.layout.enclave_code.range_start enclave1 in
      let full_memory2 = context_memory |> encode_and_put_program c.layout.enclave_code.range_start enclave2 in
      let config1 = {c with m = full_memory1} in
      let config2 = {c with m = full_memory2} in
      let (e, c) = S.run max_steps config1 in
      let (e', c') = S.run max_steps config2 in
      e = e' && c.r.r3 = c'.r.r3
    in
    let gen =
      QCheck2.Gen.(
      (* Computed depending on the attacker and enclave preamble *)
      let first_enclave_instruction = 6 in
      (* Fencepost problem must be solved carefully here: *)
      let* when_interrupt = first_enclave_instruction -- (first_enclave_instruction + n_cycles - 1) in
      (* The first instruction in the enclave can only be at most two bytes long. *)
      let security_relevant_device = Io_device.security_relevant_device io_device_states when_interrupt in
      let* c = Config.config_unprotected_minimal ~io_device:security_relevant_device () in
      let* enclave1 = Instructions.n_cycles_simple_program n_cycles >|= (fun p -> [NOP] @ p @ enclave_epilogue c) in
      let* enclave2 = Instructions.n_cycles_simple_program n_cycles >|= (fun p -> [NOP] @ p @ enclave_epilogue c) in
      pure (enclave1,enclave2,c,security_relevant_device,when_interrupt)) in
    QCheck2.Test.make ~name:name
      ~count:count
      ~print:(fun (enclave1,enclave2,c,dev,when_interrupt) ->
        let max_steps = 100 in
        let context_memory =
            c.m |> encode_and_put_program (isr c) (isr_program c)
                |> encode_and_put_program (attacker c) (attacker_program c) in
        let full_memory1 = context_memory |> encode_and_put_program c.layout.enclave_code.range_start enclave1 in
        let full_memory2 = context_memory |> encode_and_put_program c.layout.enclave_code.range_start enclave2 in
        let config1 = {c with m = full_memory1} in
        let config2 = {c with m = full_memory2} in
        let (e, c) = S.run max_steps config1 in
        let (e', c') = S.run max_steps config2 in
        "---- When interrupt ------------\n" ^ string_of_int when_interrupt ^ "\n" ^
        "---- Enclave 1 instructions ----\n" ^ String.concat "," (List.map string_of_instr enclave1) ^ "\n" ^
        "---- Enclave 2 instructions ----\n" ^ String.concat "," (List.map string_of_instr enclave2) ^ "\n" ^
        "---- Final configuration 1 -----\n" ^ string_of_config c ^ "\n" ^
        "---- Final configuration 2 -----\n" ^ string_of_config c' ^ "\n" ^
        "---- Reason for termination 1 --\n" ^ string_of_halt_error e ^ "\n" ^
        "---- Reason for termination 2 --\n" ^ string_of_halt_error e' ^ "\n" ^
        String.concat "\n" (List.map (fun i ->
          "\n" ^ string_of_int i ^ "\n" ^ show_step (monad_iterate i (S.auto_step ()) config1)
          )
         [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20])

        ^ "---- TRACE FOR SECOND CONFIG --\n" ^

        String.concat "\n" (List.map (fun i ->
          "\n" ^ string_of_int i ^ "\n" ^ show_step (monad_iterate i (S.auto_step ()) config2)
          )
         [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20])
        )
  gen property
end

let tests =
  [
    (* Tests MUST check *)
    (let open Non_interference(Sancus_high) in
     test_non_interference "Sancus high ALWAYS preserves the enclave abstraction"
       ~count:3000000
       ~n_cycles:4
       ~io_device_states:20
       ~isr_program:      (fun c -> [IN(R3); HLT])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
       ~enclave_epilogue: (fun c -> [MOV_IMM(isr c,R3);JMP R3]));
    (* Tests MUST check *)
    (let open Non_interference(Sancus_low) in
     test_non_interference "Sancus low ALWAYS preserves the enclave abstraction"
       ~count:3000000
       ~n_cycles:4
       ~io_device_states:20
       ~isr_program:      (fun c -> [IN(R3); HLT])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
       ~enclave_epilogue: (fun c -> [MOV_IMM(isr c,R3);JMP R3]));
    (* Tests MUST (EVENTUALLY) NOT check *)
    (let open Non_interference(Sancus_no_pad) in
     test_non_interference "Sancus no_pad SOMETIMES preserves the enclave abstraction"
       ~count:3000000
       ~n_cycles:4
       ~io_device_states:20
       ~isr_program:      (fun c -> [IN(R3); HLT])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
       ~enclave_epilogue: (fun c -> [MOV_IMM(isr c,R3);JMP R3]));
    (* Tests MUST check *)
    (let open Non_interference(Sancus_pre_pad) in
    test_non_interference "Sancus pre_pad with standard NI attack ALWAYS preserves the enclave abstraction "
       ~count:3000000
       ~n_cycles:4
       ~io_device_states:20
       ~isr_program:      (fun c -> [IN(R3); HLT])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
       ~enclave_epilogue: (fun c -> [MOV_IMM(isr c,R3);JMP R3]));
    (* Tests MUST (EVENTUALLY) NOT check *)
    (let open Non_interference(Sancus_pre_pad) in
     test_non_interference "Sancus pre_pad with resume to end attack SOMETIMES preserves the enclave abstraction"
       ~count:3000000
       ~n_cycles:4
       ~io_device_states:40
       ~isr_program:      (fun c -> [RETI])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)] @ [IN(R3); HLT])
       ~enclave_epilogue: (fun c -> [MOV_IMM(Word.(attacker c + from_int 6),R4);JMP R4]));
  ]
