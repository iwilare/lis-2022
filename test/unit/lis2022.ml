open OUnit2

let tests =
  "lis2022 unit test"
  >::: [
         ("io_deviced" >:: fun _ -> Io_devices_test.init_io_devices_test);
         ( "configuration" >:: fun _ ->
           Configuration_test.init_configuration_test );
       ]

let _ = run_test_tt_main tests