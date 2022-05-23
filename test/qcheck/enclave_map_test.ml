open Lis2022.Instr
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

let print_execution_trace c =
  let rec go c =
    match S.auto_step () c with
    | `ok (), c' -> string_of_config c' ^ "\n" ^ go c'
    | `halt e, _ -> "<" ^ string_of_halt_error e ^ ">" in
  string_of_config c ^ "\n" ^ go c

let attacker_io_device =
  let interrupt_delay = 6 + 2 (* Time between OUT + RETI and first enclave instruction *) in
  let max_post_interrupt_clock_length = max_cycles + 20 in
  on_demand_interrupt_clock_io_device interrupt_delay max_post_interrupt_clock_length

let test_no_pad_enclave_map_build_config enclave base_c =
  let attacker_out_address_in_memory = Word.(base_c.layout.attacker_range.range_end - from_int 1) in
  let attacker_program = [
    MOV_IMM(Word.from_int 42, R3);     (* io_device special value*)
    OUT R3;                            (* 2 cycles *) (* interrupt_delay starting *)
    NOP;                               (* 1 cycles *)
    MOV_IMM(enclave_start base_c, R3); (* 2 cycles *)
    JMP R3                             (* 2 cycles *)
    (* Total cycles before first interrupt must be equal to interrupt_delay + 1 *)
  ] in
  let isr_program = [
    IN R3; (* R3: instr_length *)
    MOV_IMM(Word.from_int 42, R4);
    MOV_IMM(Word.from_int 2, R6);
    MOV_IMM(Word.from_int (cycles RETI + 1), R10);
    MOV_IMM(attacker_out_address_in_memory, R5);
    OUT(R4);           (* Stop the clock to state 1 *)
    MOV_LOAD(R5, R7);  (* R5: &counter, R7: counter *)
    SUB(R10, R3);      (* remove interrupt time of RETI *)
    MOV_STORE(R3, R7); (* instr_length stored at counter *)
    ADD(R6, R7);       (* counter = counter + 2 *)
    MOV_STORE(R7, R5); (* counter + 2 written at &counter*)
    OUT(R4);           (* restart the clock *)
    RETI;              (* return *)
  ] in
  let attacker_size = List.fold_left (+) 0 (List.map size attacker_program) in
  let attacker_memory_start = Word.(align_even (attacker base_c + from_int attacker_size)) in
  let m =
      base_c.m
        |> encode_and_put_program (isr base_c) isr_program
        |> encode_and_put_program (attacker base_c) attacker_program
        |> encode_and_put_program base_c.layout.enclave_code.range_start (enclave @ [HLT]) (* Do not consider the HLT in the check *)
        |> memory_set attacker_out_address_in_memory attacker_memory_start (* Initialize the memory address *)
        in
  (attacker_memory_start,{base_c with m})

let test_no_pad_enclave_map name ~count =
  let property (enclave,base_c) =
    let (attacker_memory_start,c) = test_no_pad_enclave_map_build_config enclave base_c in
    let enclave_map = List.map Word.from_int (List.map cycles enclave) in
    let (e, c') = S.run c in
    e = HaltPM && memory_get_words attacker_memory_start (List.length enclave_map) c'.m = enclave_map
  in
  let gen =
    QCheck2.Gen.(
      let* n_cycles = 1 -- 30 in
      let* enclave = Instructions.n_cycles_simple_program n_cycles >|= (fun p -> [NOP] @ p) in
      (* Select as configuration the init config, where the attacker is ready. *)
      let* base_c = Config.attacker_config ~io_device:attacker_io_device () in
      pure (enclave,base_c)) in
  QCheck2.Test.make
    ~name:name
    ~count:count
    ~print:(fun (enclave,base_c) ->
        let (attacker_memory_start,c) = test_no_pad_enclave_map_build_config enclave base_c in
        let enclave_map = List.map Word.from_int (List.map cycles enclave) in
        let (e, c') = S.run c in
        string_of_io_device base_c.io_device ^
        "---- Attacker memory start -----\n" ^ Word.show_address attacker_memory_start ^ "\n" ^
        "---- Enclave instructions ------\n" ^ String.concat "," (List.map string_of_instr enclave) ^ "\n" ^
        "---- Attacker memory values ----\n" ^ String.concat "," (List.map Word.show (memory_get_words attacker_memory_start (List.length enclave_map) c'.m)) ^ "\n" ^
        "---- Enclave map ---------------\n" ^ String.concat "," (List.map Word.show enclave_map) ^ "\n")
    gen property

let tests =
  [
     test_no_pad_enclave_map "Sancus no_pad allows attackers to fully map instructions timings of the enclave"
     ~count:300000
  ]
