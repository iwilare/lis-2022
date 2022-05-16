open QCheck2.Gen
open Lis2022.Register_file
open Lis2022.Configuration
open Lis2022.Memory
open Lis2022.Types
open Lis2022.Ast
open Lis2022.Semantics
open Generators
open Semantics (Lis2022.Interrupt_logic.Sancus_low)

let is_ok x = x = `ok

(*
let test_not =
  let property (c, r) =
    let before_r = register_get c.r r in

    let good = step c (NOT r) |> is_ok in

    good && register_get c.r r == Word.lnot before_r
  in
  let gen =
    pair Configuration.configuration_unprotected_minimal Register.gp_register
  in
  QCheck2.Test.make ~name:"NOT changes the register with the correct value"
    ~count:50 gen property
*)

let tests = []
