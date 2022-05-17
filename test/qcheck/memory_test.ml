open Lis2022.Memory
open Generators.Memory
open Lis2022.Types

let test_byte_set_is_involutive =
  let property (m, a, v) =
    (m |> memory_set_byte a v
       |> (fun m -> v = memory_get_byte a m))
  in
  let gen = QCheck2.Gen.triple memory address byte in
  QCheck2.Test.make
    ~name:"Memory set memory get of bytes returns the initial value" ~count:20000
    gen property

let test_word_set_is_involutive =
  let property (m, a, v) =
    m |> memory_set a v
      |> (fun m -> v = memory_get a m)
  in
  let gen = QCheck2.Gen.triple memory address word in
  QCheck2.Test.make
    ~name:"Memory set memory get of words returns the initial value" ~count:20000
    gen property

let tests = [ test_byte_set_is_involutive; test_word_set_is_involutive ]
