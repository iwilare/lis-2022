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

type 'io_state config = {
  (* Helper exception flag *)
  exception_happened : bool;
  (* Memory layout *)
  layout : memory_layout;
  (* Global device *)
  io_device : 'io_state io_device;
  (* Timing state *)
  io_state : 'io_state;
  current_clock : time;
  arrival_time : time option;
  (* Old program counter state *)
  pc_old : address;
  (* Main state of the CPU *)
  m : memory;
  r : register_file;
  b : backup option;
}

let cpu_mode c = cpu_mode_of_address c.layout c.r.pc
let flag_gie c = get_bit mask_gie c.r.sr
let flag_z c = get_bit mask_z c.r.sr
let flag_v c = get_bit mask_v c.r.sr
let write_transitions c = io_device_write_transitions c.io_device c.io_state
let read_transition c = io_device_read_transition c.io_device c.io_state
let current_clock c = c.current_clock
let arrival_time c = c.arrival_time
let backup c = c.b
let generate_backup t_pad c = { r = c.r; t_pad; pc_old = c.pc_old }
let isr c = c.layout.isr_range.range_start
let enclave_start c = c.layout.enclave_code.range_start
let attacker c = c.layout.attacker_range.range_start
let pc c = c.r.pc
let sr c = c.r.sr
let sp c = c.r.sp
let load_here c = fun a -> memory_get a c.m
let with_memory f c = f c.m

let string_of_config c =
  if c.exception_happened then
    "<EXCEPTION OCCURRED>"
  else
  "Layout: " ^ string_of_layout c.layout ^ "\nClock: "
  ^ string_of_time c.current_clock
  ^ "\tIO state: " ^ string_of_int c.io_state ^ "\tArrival time: "
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

let init_config layout io_device memory =
  {
    io_device;
    layout;
    io_state = io_device.init_state;
    current_clock = 0;
    arrival_time = None;
    pc_old = w0xFFFE;
    m = memory;
    r = register_file_init memory;
    b = None;
    exception_happened = false;
  }

let exception_config extra_cycles c =
  { c with
     exception_happened = true;
     current_clock = c.current_clock + extra_cycles;
     arrival_time = None;
     pc_old = w0xFFFE;
     b = None;
     r = {register_file_0 with pc = memory_get w0xFFFE c.m};
  }

let rec mac_valid i c =
  let rget r = register_get r c.r in
  let pc_old = c.pc_old in
  let pc = c.r.pc in
  let sp = c.r.sp in
  match c.b with
  | Some _ ->
      (match i with
      | RETI -> true
      | _ -> mac_valid i { c with b = None } && not (flag_gie c))
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