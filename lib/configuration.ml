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
  io_device : io_device;
  enclave : enclave_layout;
  m : memory;
  mutable r : register_file;
  mutable io_state : io_state;
  mutable current_clock : time;
  mutable arrival_time : time option;
  mutable pc_old : address;
  mutable b : backup option;
}

let init_configuration enclave io_device memory () =
  {
    io_device;
    io_state = io_device.init_state;
    current_clock = 0;
    arrival_time = None;
    m = memory;
    r = register_file_init memory ();
    pc_old = 0xFFFE;
    b = None;
    enclave;
  }

let raise_exception extra_cycles c =
  c.current_clock <- c.current_clock + extra_cycles;
  c.arrival_time <- None;
  c.pc_old <- 0xFFFE;
  c.b <- None;

  c.r <- register_file_0 ();
  c.r.pc <- memory_get c.m 0xFFFE

let cpu_mode c = cpu_mode_of_address c.enclave (register_get c.r PC)
let get_io_device_possibilities c = c.io_device.delta c.io_state

let rec mac_valid c i =
  let rget = register_get c.r in
  match c.b with
  | Some _ -> (
    match i with
    | RETI -> true
    | _ ->
      mac_valid { c with b = None } i
      && (not (get_bit flag_gie (rget SR)))
      && not (is_enclave_entry_point c.enclave (rget PC)))
  | None -> (
    match i with
    | NOP
    | AND (_, _)
    | ADD (_, _)
    | SUB (_, _)
    | CMP (_, _)
    | MOV (_, _)
    | JMP _ | JZ _ ->
      mac_word c.enclave c.pc_old X (rget PC)
    | MOV_IMM (_, _) | NOT _ -> mac_doubleword c.enclave c.pc_old X (rget PC)
    | IN _ | OUT _ ->
      cpu_mode c = Some UM && mac_word c.enclave c.pc_old X (rget PC)
    | MOV_LOAD (r1, _) ->
      (not (is_touching_last_word_address (rget r1)))
      && mac_word c.enclave (rget PC) R (rget r1)
      && mac_word c.enclave c.pc_old X (rget r1)
    | MOV_STORE (_, r2) ->
      (not (is_touching_last_word_address (rget r2)))
      && mac_doubleword c.enclave c.pc_old X (rget r2)
      && mac_word c.enclave (rget PC) W (rget r2)
    | RETI ->
      (* Check all doublewords *)
      (not (is_touching_last_word_address (rget SP)))
      && (not (is_touching_last_word_address (rget SP + 2)))
      && mac_word c.enclave c.pc_old X (rget PC)
      && mac_doubleword c.enclave (rget PC) R (rget SP)
    | HLT -> true (* Should always be executable *))
