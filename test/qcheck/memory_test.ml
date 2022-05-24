open Lis2022.Memory
open Lis2022.Types
open QCheck2.Gen
open Generators.Memory

let test_byte_set_get =
  let property (m, a, v) =
    (m |> memory_set_byte a v
       |> memory_get_byte a) = v
  in
  let gen = QCheck2.Gen.triple memory address byte in
  QCheck2.Test.make
    ~name:"Memory set memory get of bytes returns the initial value" ~count:20000
    gen property

let test_word_set_get =
  let property (m, a, v) =
    (m |> memory_set a v
       |> memory_get a) = v
  in
  let gen = QCheck2.Gen.triple memory address word in
  QCheck2.Test.make
    ~name:"Memory set memory get of words returns the initial value" ~count:20000
    gen property

let test_memory_set_words_get_words =
  let property (m, a, ws) =
    (m |> memory_set_words a ws
       |> memory_get_words a (List.length ws)) = ws
  in
  let gen =
    let* n = 0 -- 32 in
    let address = address_in_range {range_start = Word.zero; range_end = Word.(w0xFFFE - from_int n)} in
    QCheck2.Gen.triple memory address (QCheck2.Gen.list_size (pure n) word) in
  QCheck2.Test.make
    ~name:"Memory set memory get of multiple words returns the initial value" ~count:20000
    gen property

let test_decompose_compose =
  let property w =
    Word.decompose_bytes w |> fun (l, h) -> Word.compose_bytes l h = w
  in
  let gen = word in
  QCheck2.Test.make
    ~name:"Decompose words is the inverse of compose words" ~count:20000
    gen property

let test_compose_decompose =
  let property (l,h) =
    Word.compose_bytes l h |> Word.decompose_bytes = (l, h)
  in
  let gen = QCheck2.Gen.pair byte byte in
  QCheck2.Test.make
    ~name:"Compose words is the inverse of decompose words" ~count:20000
    gen property

let tests = [
  test_byte_set_get;
  test_word_set_get;
  test_memory_set_words_get_words;
  test_compose_decompose;
  test_decompose_compose
]
