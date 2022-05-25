open Lis2022.Register_file


let set_bit_works_as_expected =
  let property (mask, b, x) =
    let new_x = set_bit mask b x in
    if b then get_bit mask new_x else not (get_bit mask new_x)
  in
  let gen =
    QCheck2.Gen.triple Generators.Register.sr_mask QCheck2.Gen.bool
      Generators.Memory.word
  in
  QCheck2.Test.make ~name:"Set bit works correctly" ~count:20000 gen property

let tests = [set_bit_works_as_expected ]
