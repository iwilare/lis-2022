open Lis2022.Memory
open Generators.Memory

let test_byte_set_is_involutive =
  let property (m, addr, x) =
    let mset = memory_set_byte m in
    let mget = memory_get_byte m in
    mset addr x;
    x = mget addr
  in
  let gen = QCheck2.Gen.triple memory address byte in
  QCheck2.Test.make
    ~name:"memory set memory get of bytes returns the initial value" ~count:50
    gen property

let test_word_set_is_involutive =
  let property (m, addr, x) =
    let mset = memory_set m in
    let mget = memory_get m in
    mset addr x;
    x = mget addr
  in
  let gen = QCheck2.Gen.triple memory address byte in
  QCheck2.Test.make
    ~name:"memory set memory get of words returns the initial value" ~count:50
    gen property

let tests = [ test_byte_set_is_involutive; test_word_set_is_involutive ]
