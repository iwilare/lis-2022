open Lis2022.Serialization
open Lis2022.Memory
open Generators

let test_enc_dec_of_registers =
  let property i = reg_encode i |> reg_decode = Some i in
  let gen = Register.any_register in
  QCheck2.Test.make ~name:"Register encode decode is involutive" ~count:20000 gen
    property

let test_enc_dec_of_instructions =
  let property i = encode i |> decode = Some i in
  let gen = Instructions.instr in
  QCheck2.Test.make ~name:"Instruction encode decode is involutive" ~count:20000
  ~print:Lis2022.Ast.string_of_instr
    gen property

let test_enc_dec_of_instructions_in_memory =
  let property (m, a, i) = m |> encode_and_put a i |> fetch_and_decode a = Some i in
  let gen = QCheck2.Gen.triple Memory.memory Memory.address Instructions.instr in
  QCheck2.Test.make ~name:"Instruction encode decode in memory is involutive" ~count:20000
  ~print:(fun (m, a, i) -> Lis2022.Ast.string_of_instr i ^ "\n" ^ string_of_memory (m |> encode_and_put a i))
  gen property

let tests = [ test_enc_dec_of_registers; test_enc_dec_of_instructions; test_enc_dec_of_instructions_in_memory]
