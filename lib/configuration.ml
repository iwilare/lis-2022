open Types
open Memory
open Io_device
open Register_file
open Ast

type time = int

let string_of_time = string_of_int

type backup = {
  r : register_file; (* Register file *)
  pc_old : address; (* Program counter *)
  t_pad : time; (* Remaining padding time *)
}

type 'io_state configuration = {
  (* High vs low Sancus emulation *)
  manage_interrupts : bool;
  (* Memory layout *)
  layout : memory_layout;
  (* Global device *)
  io_device : 'io_state io_device;
  (* Timing state *)
  mutable io_state : 'io_state;
  mutable current_clock : time;
  mutable arrival_time : time option;
  (* Old program counter state *)
  mutable pc_old : address;
  (* Main state of the CPU *)
  m : memory;
  r : register_file;
  mutable b : backup option;
}

let string_of_configuration c =
  "Sancus: "
  ^ (if c.manage_interrupts then "high" else "low")
  ^ "\nLayout: " ^ string_of_layout c.layout ^ "\nClock: "
  ^ string_of_time c.current_clock
  ^ "\tIO state: " ^ Word.show c.io_state ^ "\tArrival time: "
  ^ Option.fold ~none:"-" ~some:string_of_time c.arrival_time
  ^ "\tBackup: "
  ^ Option.fold ~none:"-"
      ~some:(fun b -> "pad(" ^ string_of_time b.t_pad ^ ")")
      c.b
  ^ "\n[PC: " ^ Word.show_address c.r.pc ^ "]" ^ " [PCOLD: "
  ^ Word.show_address c.pc_old ^ "]" ^ " [SP: " ^ Word.show_address c.r.sp ^ "]"
  ^ " [SR: " ^ Word.show c.r.sr ^ " (" ^ string_of_sr_flags c.r.sr ^ ")" ^ "]"
  ^ "\n"
  ^ string_of_register_file_gp c.r

let init_configuration manage_interrupts layout io_device memory () =
  {
    manage_interrupts;
    io_device;
    layout;
    io_state = io_device.init_state;
    current_clock = 0;
    arrival_time = None;
    pc_old = w0xFFFE;
    m = memory;
    r = register_file_init memory ();
    b = None;
  }

let raise_exception extra_cycles c =
  c.current_clock <- c.current_clock + extra_cycles;
  c.arrival_time <- None;
  c.pc_old <- w0xFFFE;
  c.b <- None;

  copy_register_file c.r (register_file_0 ());
  c.r.pc <- memory_get c.m w0xFFFE

let cpu_mode c = cpu_mode_of_address c.layout c.r.pc
let io_device_choices c = c.io_device.delta c.io_state
let flag_gie c = get_bit mask_gie c.r.sr
let flag_z c = get_bit mask_z c.r.sr

let rec mac_valid c i =
  let rget = register_get c.r in
  let pc_old = c.pc_old in
  let pc = c.r.pc in
  let sp = c.r.sp in
  match c.b with
  | Some _ ->
      (match i with
      | RETI -> true
      | _ -> mac_valid { c with b = None } i && not (flag_gie c))
      && not (is_enclave_entry_point c.layout pc)
  | None -> (
      (* Check all current instruction bytes *)
      mac_region c.layout pc_old X pc (size i)
      &&
      match i with
      | IN _ | OUT _ -> cpu_mode c = Some UM
      | MOV_LOAD (r1, _) ->
          (not (is_touching_last_word_address (rget r1)))
          && mac_word c.layout pc R (rget r1)
      | MOV_STORE (_, r2) ->
          (not (is_touching_last_word_address (rget r2)))
          && mac_word c.layout pc W (rget r2)
      | RETI ->
          (not (is_touching_last_word_address sp))
          && (not (is_touching_last_word_address Word.(sp + from_int 1)))
          && mac_word c.layout pc R sp
          && mac_word c.layout pc R Word.(sp + from_int 1)
          (* Check that we can read PC and SP from the stack *)
      | _ -> true (* HALT should always be executable *))

let advance_device k c =
  let io_state, current_clock, arrival_time =
    advance c.io_device k (c.io_state, c.current_clock, c.arrival_time)
  in
  c.io_state <- io_state;
  c.current_clock <- current_clock;
  c.arrival_time <- arrival_time
