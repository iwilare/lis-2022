open Lis2022.Memory
open Generators.Memory
open Lis2022.Types

let test_byte_set_is_involutive =
  let property (m, a, v) =
    memory_set_byte m a v;
    v = memory_get_byte m a
  in
  let gen = QCheck2.Gen.triple memory address byte in
  QCheck2.Test.make
    ~name:"memory set memory get of bytes returns the initial value" ~count:50
    gen property

let test_word_set_is_involutive =
  let property (m, a, v) =
    memory_set m a v;
    v = memory_get m a
  in
  let gen = QCheck2.Gen.triple memory address word in
  QCheck2.Test.make
    ~name:"memory set memory get of words returns the initial value" ~count:50
    gen property

let tests = [ test_byte_set_is_involutive; test_word_set_is_involutive ]
