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

type configuration =
  {
    io_state : io_state;
    current_clock : time;
    arrival_time : time option;
    m : memory;
    r : register_file;
    pc_old : address;
    b : backup;
    enclave : enclave_layout;
  }

let init_configuration enclave io_device memory =
  {
    io_state = io_device.init_state;
    current_clock = 0;
    arrival_time = None;
    m = memory;
    r = register_file_init memory;
    pc_old = 0xFFFE;
    b = Bottom;
    enclave;
  }

let cpu_mode = function
  | None -> Some UM
  | Some(c) -> cpu_mode_of_address c.enclave (register_get c.r PC)

let config_get_gie c = get_bit gie (register_get c.r SR)

let rec mac_valid (c : configuration option) i =
  match c with
  | None -> false
  | Some(c) ->
    match c.b with
    | Backup _
    | Padding _ ->
      begin
      match i with
      | RETI -> true
      | _ -> mac_valid (Some {c with b = Bottom}) i
          && not (config_get_gie c)
          && not (is_enclave_entry_point c.enclave (register_get c.r PC))
      end
    | Bottom ->
      match i with
      | NOP
      | JMP(_)
      | JZ(_)
      | MOV(_,_)
      | SUB(_,_)
      | AND(_,_)
      | CMP(_,_) -> mac c.enclave c.pc_old X (register_get c.r PC)
                 && mac c.enclave c.pc_old X (register_get c.r PC + 1)

      | RETI -> failwith "Not implemented yet"
      | HLT -> failwith "Not implemented yet"
      | IN _ -> failwith "Not implemented yet"
      | OUT _ -> failwith "Not implemented yet"
      | MOV_LOAD (_, _) -> failwith "Not implemented yet"
      | MOV_STORE (_, _) -> failwith "Not implemented yet"
      | MOV_IMM (_, _) -> failwith "Not implemented yet"
      | NOT _ -> failwith "Not implemented yet"
      | ADD (_, _) -> failwith "Not implemented yet"