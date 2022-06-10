open Lis2022.Instr
open Lis2022.Config
open Lis2022.Serialization
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
  - Construct the attacker in the unprotected and isr regions.
  - Generate a 2 random programs, e1 and e2, in the enclave region N bytes with random timings, both of same total time
  - Generate one (1) security-relevant linear clock-based io_device
    - That always outputs the current clock
    - Sometimes runs an interrupt
  - Run with enclave1.
  - Run with enclave2.
  - Check if any of the attacker registers read with INs differ.
*)

let view_complete_trace = false

module Non_interference (I : Interrupt_logic) = struct
  module S = Semantics(I)
  let print_execution_trace c =
    let rec go c =
      match S.auto_step () c with
      | `ok (), c' -> string_of_config c' ^ "\n" ^ go c'
      | `halt e, _ -> "<" ^ string_of_halt_error e ^ ">" in
    string_of_config c ^ "\n" ^ go c

  let test_non_interference_build_config ~attacker_program ~isr_program enclave1 enclave2 base_c =
    let context_memory =
        base_c.m
          |> encode_and_put_program (isr base_c) (isr_program base_c)
          |> encode_and_put_program (attacker base_c) (attacker_program base_c) in
    let full_memory1 = context_memory |> encode_and_put_program base_c.layout.enclave_code.range_start enclave1 in
    let full_memory2 = context_memory |> encode_and_put_program base_c.layout.enclave_code.range_start enclave2 in
    let config1 = {base_c with m = full_memory1} in
    let config2 = {base_c with m = full_memory2} in
    (config1, config2)

  let test_non_interference ?(max_fail = 1) name ~count ~attacker_program ~isr_program ~enclave_epilogue ~n_cycles ~halt_reason ~io_device_states =
    let property (enclave1,enclave2,base_c,_) =
      let (config1, config2) = test_non_interference_build_config ~attacker_program ~isr_program enclave1 enclave2 base_c in
      let (e, c) = S.run config1 in
      let (e', c') = S.run config2 in
      e = halt_reason && e = e' && c.r.r3 = c'.r.r3
    in
    let gen =
      QCheck2.Gen.(
      (* Computed depending on the attacker and enclave preamble *)
      let first_enclave_instruction = 6 in
      (* Fencepost problem must be solved carefully here: *)
      let* when_interrupt = first_enclave_instruction -- (first_enclave_instruction + n_cycles - 1) in
      (* The first instruction in the enclave can only be at most two bytes long. *)
      let security_relevant_device = security_relevant_device io_device_states when_interrupt in
      (* Select as configuration the init config, where the attacker is ready. *)
      let* base_c = Config.attacker_config ~io_device:security_relevant_device () in
      (* Generate the two enclaves *)
      let* enclave1 = Instructions.n_cycles_simple_program n_cycles >|= (fun p -> [NOP] @ p @ enclave_epilogue base_c) in
      let* enclave2 = Instructions.n_cycles_simple_program n_cycles >|= (fun p -> [NOP] @ p @ enclave_epilogue base_c) in
      pure (enclave1,enclave2,base_c,when_interrupt)) in
    QCheck2.Test.make ~name:name
      ~count:count
      ~max_fail:max_fail
      ~print:(fun (enclave1,enclave2,base_c,when_interrupt) ->
        let (config1, config2) = test_non_interference_build_config ~attacker_program ~isr_program enclave1 enclave2 base_c in
        let (_, c1) = S.run config1 in
        let (_, c2) = S.run config2 in
        "---- When to interrupt ---------\n" ^ string_of_int when_interrupt ^ "\n" ^
        "---- Enclave 1 instructions ----\n" ^ String.concat "," (List.map string_of_instr enclave1) ^ "\n" ^
        "---- Enclave 2 instructions ----\n" ^ String.concat "," (List.map string_of_instr enclave2) ^ "\n" ^
        (if view_complete_trace then
            "---- Trace 1 -------------------\n" ^ print_execution_trace config1 ^ "\n" ^
            "---- Trace 2 -------------------\n" ^ print_execution_trace config2
         else
            "---- Values in R3 -------------------\n" ^
            "  Enclave 1: final R3 value =" ^ Word.show (c1.r.r3) ^ "\n" ^
            "  Enclave 2: final R3 value =" ^ Word.show (c2.r.r3)))
  gen property
end

(* Clear all registers *)
let clear_all = List.map (fun r -> MOV_IMM(Word.from_int 0, r)) all_gp_registers

let tests =
  [
    (* Tests must ALWAYS check *)
    (let open Non_interference(Sancus_high) in
     test_non_interference "Sancus high ALWAYS preserves the enclave abstraction"
       ~count:30000
       ~n_cycles:4
       ~io_device_states:50 (* Add more states, since ISR goes back to the *)
       ~halt_reason:      HaltUM
       ~isr_program:      (fun _ -> [IN(R3); HLT])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
       ~enclave_epilogue: (fun c -> clear_all @ [MOV_IMM(isr c,R3);JMP R3]));
        (* The ISR is never reached through interrupts.
           The enclave gives back control at its end, before clearing out all registers.
        *)
    (* Tests must ALWAYS check *)
    (let open Non_interference(Sancus_low) in
     test_non_interference "Sancus low ALWAYS preserves the enclave abstraction"
       ~count:30000
       ~n_cycles:4
       ~io_device_states:25
       ~halt_reason:      HaltUM
       ~isr_program:      (fun _ -> [IN(R3); HLT])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
       ~enclave_epilogue: (fun c -> clear_all @ [MOV_IMM(isr c,R3);JMP R3]));
    (* Tests must ALWAYS check *)
    (let open Non_interference(Sancus_pre_pad) in
    test_non_interference "Sancus pre_pad with standard NI attack ALWAYS preserves the enclave abstraction "
      ~count:30000
      ~n_cycles:4
      ~io_device_states:25
      ~halt_reason:      HaltUM
      ~isr_program:      (fun _ -> [IN(R3); HLT])
      ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
      ~enclave_epilogue: (fun c -> clear_all @ [MOV_IMM(isr c,R3);JMP R3]));
    (* Tests should SOMETIMES not check *)
    (let open Non_interference(Sancus_no_pad) in
     test_non_interference "Sancus no_pad SOMETIMES preserves the enclave abstraction"
       ~count:30000
       ~n_cycles:4
       ~io_device_states:25
       ~halt_reason:      HaltUM
       ~isr_program:      (fun _ -> [IN(R3); HLT])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)])
       ~enclave_epilogue: (fun c -> clear_all @ [MOV_IMM(isr c,R3);JMP R3]));
    (* Tests should SOMETIMES not check *)
    (let open Non_interference(Sancus_pre_pad) in
     test_non_interference "Sancus pre_pad with resume to end attack SOMETIMES preserves the enclave abstraction"
       ~count:30000
       ~n_cycles:4
       ~io_device_states:70
       ~halt_reason:      HaltUM
       ~isr_program:      (fun _ -> [RETI])
       ~attacker_program: (fun c -> [MOV_IMM(enclave_start c,R3); JMP(R3)] @ [IN(R3); HLT])
       ~enclave_epilogue: (fun c -> clear_all @ [MOV_IMM(Lis2022.Types.Word.(attacker c + from_int 6),R4);JMP R4]));
  ]
