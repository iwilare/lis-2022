open Memory
open Register_file
open Ast
open Config
open Halt_error
open Types
open Config_monad

module type Interrupt_logic = sig
  val interrupt_logic : (unit, 'io_device) m
end

let interrupt_logic ~activate_padding c =
  (let@ arrival_time in
  let@ flag_gie in
  match arrival_time with
  | Some ta when flag_gie -> (
      let@ cpu_mode in
      match cpu_mode with
      | None -> halt ExecutingEnclaveData
      | Some UM ->
          (* Push PC and SR in memory *)
          let@ sp in
          let@ sr in
          let@ pc in
          let@ isr in
          mset Word.(sp - from_int 2) pc >>
          mset Word.(sp - from_int 4) sr >>
          (* Jump to the ISR *)
          rset PC isr >>
          rset SR Word.zero >>
          rset SP Word.(sp - from_int 4) >>
          advance_config 6
          (* 6 cycles is to access memory and update the registers *)
      | Some PM ->
          let@ current_clock in
          let t_pad = current_clock - ta in
          let k = max_cycles - t_pad in
          let@ new_backup = generate_backup t_pad in
          let@ isr in
          set_backup @@ Some new_backup >>
          clear_registers >>
          rset PC isr >>
          (if activate_padding then
            advance_config (6 + k)
          else
            advance_config 6))
  | _ -> ok) c

module Sancus_unsafe = struct
  let interrupt_logic c = interrupt_logic ~activate_padding:false c
end

module Sancus_low = struct
  let interrupt_logic c = interrupt_logic ~activate_padding:true c
end

module Sancus_high = struct
  let interrupt_logic c = ok c
end
