open QCheck2.Gen
open Lis2022.Register_file
open Lis2022.Configuration
open Lis2022.Semantics
open Generators

let test_add =
  let property (c, r1, r2) =
    let before_r1 = register_get c.r r1 in
    let before_r2 = register_get c.r r2 in

    let good = step c (ADD (r1, r2)) |> Result.is_ok in

    good && (r1 == r2 || register_get c.r r1 = before_r1) && register_get c.r r2 = before_r1 + before_r2
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

    let good = step c (SUB (r1, r2)) |> Result.is_ok in

    good && (r1 == r2 || register_get c.r r1 = before_r1) && register_get c.r r2 = before_r1 - before_r2
  in
  let gen =
    triple Configuration.configuration_unprotected_minimal Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:"SUB changes the first register with the correct value" ~count:50 gen
    property
(*
let test_jmp =
  let property (c, l, r) =
    let rget = register_get c.r in

    let mac_ok = mac l (rget PC) R (rget r) in

    let good = step c (JMP r) |> Result.is_ok in

    good && rget PC = rget r
  in
  let gen =
    pair Configuration.configuration_minimal Register.gp_register
  in
  QCheck2.Test.make
    ~name:"JMP changes pc with the correct value" ~count:50 gen
    property
*)
let tests = [ test_add; test_sub ]
