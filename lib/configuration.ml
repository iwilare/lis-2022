open Memory
open Io_device
open Register_file
open Ast

type time = int

type backup =
  | Bottom
  | Padding of time
  | Backup of
      register_file (* Register file          *)
      * address (* Program counter        *)
      * time (* Remaining padding time *)

type configuration = {
  io_state : io_state;
  current_clock : time;
  arrival_time : time option;
  m : memory;
  r : register_file;
  pc_old : address;
  b : backup;
}

let init_configuration io_device memory =
  {
    io_state = io_device.init_state;
    current_clock = 0;
    arrival_time = None;
    m = memory;
    r = register_file_init memory;
    pc_old = 0xFFFE;
    b = Bottom;
  }

type mac = { read : bool; write : bool; execute : bool }
