open Lis2022.Register_file
open Lis2022.Memory
open Lis2022.Types.Word

let test_align_even =
  let property x =
    if x land one = zero then align_even x = x else align_even x = x - one
  in
  let gen = Generators.Memory.address in
  QCheck2.Test.make ~name:"Align even always returns the correct even address" ~count:20000 gen property

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

let tests = [ test_align_even; set_bit_works_as_expected ]
