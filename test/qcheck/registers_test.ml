open Lis2022.Register_file

let test_align_even =
  let is_even x =
    if x mod 2 = 0 then align_even x = x else align_even x = x - 1
  in
  let gen_address = Generators.Memory.gen_valid_address in
  QCheck2.Test.make ~name:"align even always returns the correct even address"
    ~count:50 gen_address is_even

(*ways to property test mutations? *)
let tests = [ test_align_even ]
