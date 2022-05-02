open Memory
open Io_device
open Register_file
open Ast

type time = int

type backup = {
  r : register_file; (* Register file *)
  pc_old : address; (* Program counter *)
  t_pad : time; (* Remaining padding time *)
}

type configuration = {
  (* High vs low Sancus emulation *)
  manage_interrupts : bool;
  (* Memory layout *)
  enclave : enclave_layout;
  isr : address;
  (* Global device *)
  io_device : io_device;
  (* Timing state *)
  mutable io_state : io_state;
  mutable current_clock : time;
  mutable arrival_time : time option;
  (* Old program counter state *)
  mutable pc_old : address;
  (* Main state of the CPU *)
  mutable m : memory;
  mutable r : register_file;
  mutable b : backup option;
}

let init_configuration manage_interrupts enclave io_device memory isr () =
  {
    manage_interrupts;
    isr;
    io_device;
    enclave;
    io_state = io_device.init_state;
    current_clock = 0;
    arrival_time = None;
    pc_old = 0xFFFE;
    m = memory;
    r = register_file_init memory ();
    b = None;
  }

let raise_exception extra_cycles c =
  c.current_clock <- c.current_clock + extra_cycles;
  c.arrival_time <- None;
  c.pc_old <- 0xFFFE;
  c.b <- None;

  c.r <- register_file_0 ();
  c.r.pc <- memory_get c.m 0xFFFE

let cpu_mode c = cpu_mode_of_address c.enclave c.r.pc
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
      && not (is_enclave_entry_point c.enclave pc)
  | None -> (
      match i with
      | NOP
      | AND (_, _)
      | ADD (_, _)
      | SUB (_, _)
      | CMP (_, _)
      | MOV (_, _)
      | JMP _ | JZ _ ->
          mac_word c.enclave pc_old X pc
      | MOV_IMM (_, _) | NOT _ -> mac_doubleword c.enclave pc_old X pc
      | IN _ | OUT _ -> cpu_mode c = Some UM && mac_word c.enclave pc_old X pc
      | MOV_LOAD (r1, _) ->
          (not (is_touching_last_word_address (rget r1)))
          && mac_word c.enclave pc R (rget r1)
          && mac_word c.enclave pc_old X (rget r1)
      | MOV_STORE (_, r2) ->
          (not (is_touching_last_word_address (rget r2)))
          && mac_doubleword c.enclave pc_old X (rget r2)
          && mac_word c.enclave pc W (rget r2)
      | RETI ->
          (* Check all doublewords *)
          (not (is_touching_last_word_address sp))
          && (not (is_touching_last_word_address (sp + 2)))
          && mac_word c.enclave pc_old X pc
          && mac_doubleword c.enclave pc R sp
      | HLT -> true (* Should always be executable *))
