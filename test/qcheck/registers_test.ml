open Lis2022.Register_file

let test_align_even =
  let property x =
    if x mod 2 = 0 then align_even x = x else align_even x = x - 1
  in
  let gen = Generators.Memory.address in
  QCheck2.Test.make ~name:"align even always returns the correct even address"
    ~count:100 gen property

let set_bit_works_as_expected =
  let property (mask, b, x) =
    let new_x = set_bit mask b x in
    if b then get_bit mask new_x else not (get_bit mask new_x)
  in
  let gen =
    QCheck2.Gen.triple Generators.Register.sr_mask QCheck2.Gen.bool
      Generators.Memory.byte
  in
  QCheck2.Test.make ~name:"set bit works" ~count:50
    ~print:
      (QCheck2.Print.triple QCheck2.Print.int QCheck2.Print.bool
         QCheck2.Print.int)
    gen property

(*ways to property test mutations? *)
let tests = [ test_align_even; set_bit_works_as_expected ]
