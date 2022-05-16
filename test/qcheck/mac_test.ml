open QCheck2.Gen
open Lis2022.Register_file
open Lis2022.Configuration
open Lis2022.Memory
open Lis2022.Types
open Lis2022.Ast
open Lis2022.Semantics
open Generators
open Semantics (Lis2022.Interrupt_logic.Sancus_low)

let mac_valid_load =
  let property (c, r1, r2) =
    register_set c.layout c.r r1 (align_even (register_get c.r r1));
    let addr = register_get c.r r1 in

    let exec = mac c.layout c.pc_old X c.r.pc in

    let last_word = is_touching_last_word_address addr in
    let enc_memory =
      is_enclave_data c.layout addr || is_enclave_code c.layout addr
    in
    let enc_entry =
      is_enclave_entry_point c.layout addr && r2 == Lis2022.Ast.PC
    in
    enc_entry || not (last_word || enc_memory)
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register
      Register.gp_register
  in
  QCheck2.Test.make
    ~name:(name ^ "mac_valid validates LOAD (UM) correctly")
    ~count:50
    ~print:(fun (c, _, _) -> string_of_configuration c)
    gen property
