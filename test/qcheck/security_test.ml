open QCheck2
open Lis2022.Register_file
open Lis2022.Config
open Lis2022.Memory
open Lis2022.Ast
open Lis2022.Io_device
open Lis2022.Types
open Lis2022.Semantics
open Lis2022.Config_monad
open Lis2022.Halt_error
open Lis2022.Interrupt_logic
open Lis2022.Instructions
open Generators


module S_low = Semantics(Sancus_low)
module S_high = Semantics(Sancus_high)
module S_unsafe = Semantics(Sancus_unsafe)
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

let test_non_interference =
  let property (enclave1,enclave2,c) =
    let context_memory =
        c.m |> encode_and_put_program (isr c) [IN(R3); HLT]
            |> encode_and_put_program (attacker c) [MOV_IMM(enclave_start c,R3); JMP(R3)] in
    let full_memory1 = context_memory |> encode_and_put_program c.layout.enclave_code.range_start enclave1 in
    let full_memory2 = context_memory |> encode_and_put_program c.layout.enclave_code.range_start enclave2 in
    true
  in
  let gen =
    QCheck2.Gen.(
    let n_cycles = 10 in
    let io_device_states = 25 in
    let* when_interrupt = 5 -- (n_cycles + 5) in
    let* enclave1 = Instructions.n_cycles_simple_program n_cycles in
    let* enclave2 = Instructions.n_cycles_simple_program n_cycles in
    let* security_relevant_device = Io_device.security_relevant_device io_device_states when_interrupt in
    let* configuration = Config.config_unprotected_minimal ~io_device:security_relevant_device () in
    pure (enclave1,enclave2,configuration)) in
  QCheck2.Test.make ~name:"Test that Sancus_unsafe breaks the enclave abstraction" ~count:100000 gen property

let tests =
  [
    test_non_interference
  ]