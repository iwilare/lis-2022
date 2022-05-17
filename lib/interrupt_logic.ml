open Memory
open Register_file
open Ast
open Config
open Halt_error
open Types
open Config_monad

module type Interrupt_logic = sig
  val interrupt_logic : unit -> (unit, 'io_device) m
end

module Sancus_low = struct
  let interrupt_logic () =
    let* c = get in
    match c.arrival_time with
    | Some ta when flag_gie c -> (
        match cpu_mode c with
        | None -> halt ExecutingEnclaveData
        | Some UM ->
            (* Push PC and SR in memory *)
            let* c = get in
            let* _ = mset Word.(c.r.sp - from_int 2) c.r.pc in
            let* _ = mset Word.(c.r.sp - from_int 4) c.r.sr in
            (* Jump to the ISR *)
            let* _ = rset PC c.layout.isr in
            let* _ = rset SR Word.zero in
            let* _ = rset SP Word.(c.r.sp - from_int 4) in
            advance_config 6
            (* 6 cycles is to access memory and update the registers *)
        | Some PM ->
            let t_pad = c.current_clock - ta in
            let k = max_cycles - t_pad in
            let* c = get in
            let* _ = set_backup @@ Some { r = c.r; t_pad; pc_old = c.pc_old } in
            let* _ = clear_registers () in
            let* _ = rset PC c.layout.isr in
            advance_config (6 + k))
    | _ -> ok
end

module Sancus_high = struct
  let interrupt_logic _ = ok
end
