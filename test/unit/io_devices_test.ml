open OUnit2
open Lis2022.Io_device

let init_io_devices_test =
  let io_dev = default_io_device in
  let _ = assert_equal io_dev.states [] in
  assert_equal io_dev.init_state 0

let io_device_advance_one =
  let io_dev = default_io_device in
  let next_conf = advance io_dev 0 (0, 0, None) in
  let _ = assert_equal next_conf (0, 0, None) in
  let next_conf = advance io_dev 2 (0, 0, None) in
  assert_equal next_conf (0, 2, None)
