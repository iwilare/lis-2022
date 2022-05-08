open QCheck2.Gen
open Lis2022.Register_file
open Lis2022.Configuration
open Lis2022.Semantics
open Lis2022.Memory
open Generators

let test_add =
  let property (c, r1, r2) =

    let before_r1 = register_get c.r r1 in
    let before_r2 = register_get c.r r2 in

    let v = before_r1 + before_r2 >= 65536 in 

    let good = step c (ADD (r1, r2)) |> Result.is_ok in

    let vsr = get_bit mask_v (register_get c.r SR) in 

    let overflow = v && vsr in

    let unchanged_r1 = (r1 == r2 || register_get c.r r1 = before_r1) in 

    good && unchanged_r1 && (overflow || register_get c.r r2 = before_r1 + before_r2)
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:"ADD changes the first register with the correct value" ~count:50
    ~print:(fun (c,s,t) -> string_of_configuration c)
    gen
    property

let test_sub =
  let property (c, r1, r2) =

    let before_r1 = register_get c.r r1 in
    let before_r2 = register_get c.r r2 in

    let v = before_r1 - before_r2 >= 65536 in

    let good = step c (SUB (r1, r2)) |> Result.is_ok in

    let vsr = get_bit mask_v (register_get c.r SR) in 

    let overflow = v && vsr in

    let unchanged_r1 = (r1 == r2 || register_get c.r r1 = before_r1) in

    good && unchanged_r1 && (overflow || register_get c.r r2 = before_r1 - before_r2)
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:"SUB changes the first register with the correct value" ~count:50 gen
    property

let test_and =
  let property (c, r1, r2) =

    let before_r1 = register_get c.r r1 in
    let before_r2 = register_get c.r r2 in

    let good = step c (AND (r1, r2)) |> Result.is_ok in

    good && (r1 == r2 || register_get c.r r1 = before_r1) && register_get c.r r2 = before_r1 land before_r2
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:"AND changes the first register with the correct value" ~count:50 gen
    property

let test_not =
  let property (c, r) =

    let before_r = register_get c.r r in

    let good = step c (NOT (r)) |> Result.is_ok in

    good && (register_get c.r r == lnot before_r)
  in
  let gen =
    pair Configuration.configuration_unprotected_minimal Register.gp_register
  in
  QCheck2.Test.make
    ~name:"NOT changes the register with the correct value" ~count:50 gen
    property

let test_cmp =
  let property (c, r1, r2) =

    let before_r1 = register_get c.r r1 in
    let before_r2 = register_get c.r r2 in
    let diff = before_r1 - before_r2 in 

    let good = step c (CMP (r1, r2)) |> Result.is_ok in
    let unchanged = (before_r1 == register_get c.r r1) && (before_r2 == register_get c.r r2) in     
    let zsr = get_bit mask_z (register_get c.r SR) in  

    (*should we also check that bit zero of sr is unchanged when r1 != r2 ?*)

    good && unchanged && ((diff==0 && zsr) || diff!=0)
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:"CMP set bit zero of sr when the two registers are equal" ~count:50 gen
    property

let test_mov =
  let property (c, r1, r2) =

    let before_r1 = register_get c.r r1 in

    let good = step c (MOV (r1, r2)) |> Result.is_ok in

    good && (before_r1 == register_get c.r r1) && (before_r1 == register_get c.r r2)
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:"MOV changes the second register with the correct value" ~count:50 gen
    property

let test_movi =
  let property (c, w, r) =
    
    let good = step c (MOV_IMM (w, r)) |> Result.is_ok in 
    
    good && (register_get c.r r == w)
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Memory.reg_value Register.gp_register
  in
  QCheck2.Test.make
    ~name:"MOV_IMM changes the register with the correct value" ~count:50 gen
    property

let test_load_um =
  let property (c, r1, r2) = 

    register_set c.layout c.r r1 (align_even (register_get c.r r1)); 
    let before_r1 = register_get c.r r1 in

    let last_word = is_touching_last_word_address before_r1 in
    let enc_memory = is_enclave_data c.layout before_r1 || is_enclave_code c.layout before_r1 in
    let enc_entry = is_enclave_entry_point c.layout before_r1 && r2 == Lis2022.Ast.PC in 

    let good = step c (MOV_LOAD (r1, r2)) |> Result.is_ok in
    let unchanged_r1 = r1 == r2 || register_get c.r r1 = before_r1 in 
    let changed_r2 = register_get c.r r2 == memory_get c.m before_r1 in 
    let mac_ok = enc_entry || not (last_word || enc_memory) in 

    (*should we also test that if step fails we are in the EXC configuration?*)

    (*(mac_ok => good && changed_r2 && unchanged_r1)*)
    not mac_ok || (good && changed_r2 && unchanged_r1)
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:"MOV_LOAD (UM) changes the second register with the correct value" ~count:50 gen
    property

let tests = [test_add; test_sub; test_and; test_not; test_mov; test_movi; test_cmp; test_load_um]
