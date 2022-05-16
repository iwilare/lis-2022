open Lis2022.Instructions
open Generators

let test_enc_dec_of_registers =
  let property i = reg_encode i |> reg_decode = Some i in
  let gen = Register.any_register in
  QCheck2.Test.make ~name:"register encode decode is involutive" ~count:50 gen
    property

let test_enc_dec_of_instructions =
  let property i = encode i |> decode = Some i in
  let gen = Generators.Instructions.random_inst in
  QCheck2.Test.make ~name:"Instruction encode decode is involutive" ~count:50
    gen property 

let tests = [ test_enc_dec_of_registers; test_enc_dec_of_instructions ]
