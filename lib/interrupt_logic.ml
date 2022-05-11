open Memory
open Register_file
open Ast
open Configuration
open Halt_error
open Types

module type Interrupt_logic = sig
  val interrupt_logic : 'io_state configuration -> [> `ok | `halt of halt_error ]
end

module Sancus_low = struct
  let interrupt_logic c =
    let rget = register_get c.r in
    let rset = register_set c.layout c.r in
    let mset = memory_set c.m in
    match c.arrival_time with
    | Some ta when flag_gie c -> (
        match cpu_mode c with
        | None -> `halt ExecutingEnclaveData
        | Some UM ->
            (* Push PC and SR in memory *)
            mset Word.(rget SP - from_int 2) @@ rget PC;
            mset Word.(rget SP - from_int 4) @@ rget SR;
            (* Jump to the ISR *)
            rset PC c.layout.isr;
            rset SR Word.zero;
            rset SP Word.(rget SP - from_int 4);
            advance_device (2 * cycles_per_access) c;
            `ok
        | Some PM ->
            let t_pad = c.current_clock - ta in
            let k = max_cycles - t_pad in
            c.b <- Some { r = c.r; t_pad; pc_old = c.pc_old };
            copy_register_file c.r (register_file_0 ());
            c.r.pc <- c.layout.isr;
            advance_device (6 + k) c;
            `ok)
    | _ -> `ok
end

module Sancus_high = struct
  let interrupt_logic _ = `ok
end