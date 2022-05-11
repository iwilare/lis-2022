open Memory
open Register_file
open Ast
open Configuration
open Io_device
open Halt_error
open Interrupt_logic
open Types.Word

module Semantics (I : Interrupt_logic) = struct
  let execute_instruction_semantics i c =
    let mset = memory_set c.m in
    let mget = memory_get c.m in
    let rget = register_get c.r in
    let rset = register_set c.layout c.r in
    let rset_bit r mask v = rset r @@ set_bit mask v (rget r) in
    let set_status_register_flags v is_overflow =
      rset_bit SR mask_n (v < zero);
      rset_bit SR mask_z (v == zero);
      rset_bit SR mask_c (v <> zero);
      rset_bit SR mask_v is_overflow
    in
    let epilogue =
      advance_device (cycles i) c;
      I.interrupt_logic c
    in
    match i with
    | HLT -> (
        match cpu_mode c with
        | None -> `halt ExecutingEnclaveData
        | Some UM -> `halt HaltUM
        | Some PM ->
            raise_exception (cycles i) c;
            `ok)
    | IN r -> (
        match (io_device_choices c).read_transition with
        | None -> `halt NoIn
        | Some (w, d') ->
            rset r @@ w;
            c.io_state <- d';
            advance_device Stdlib.(cycles i - 1) c;
            (* Advance must do one cycle less *)
            I.interrupt_logic c)
    | OUT r -> (
        match (io_device_choices c).write_transitions (rget r) with
        | None -> `halt NoOut
        | Some d' ->
            c.io_state <- d';
            advance_device Stdlib.(cycles i - 1) c;
            (* Advance must do one cycle less *)
            I.interrupt_logic c)
    | RETI -> (
        match c.b with
        | Some b -> (
            advance_device (cycles i) c;
            match c.arrival_time with
            | Some _ when flag_gie c ->
                (* CPU-Reti-Chain *)
                I.interrupt_logic c
                (* Necessarily the pending protected case! Because ta'!=\bot && b!=\bot *)
            | _ ->
                (* CPU-Reti-PrePad *)
                c.b <- None;
                copy_register_file c.r b.r;
                c.pc_old <- b.pc_old;
                (* CPU-Reti-Pad *)
                advance_device b.t_pad c;
                I.interrupt_logic c)
        | None ->
            (* CPU-Reti *)
            (* No backup is found; we are in unprotected mode *)
            (* The point to return to is saved on the stack *)
            rset PC @@ mget (rget SP + from_int 2);
            (* Jump to the saved state *)
            (* PC can be safely set in the step function since we overwrite it here. *)
            rset SR @@ mget (rget SP);
            (* Restore SR *)
            rset SP @@ (rget SP + from_int 4);
            (* Clean up the stack *)
            advance_device (cycles i) c;
            (* NO INTERRUPT LOGIC HERE! *)
            `ok)
    | MOV_LOAD (r1, r2) ->
        rset r2 @@ mget (rget r1);
        epilogue
    | MOV_STORE (r1, r2) ->
        mset (rget r2) (rget r1);
        epilogue
    | MOV (r1, r2) ->
        rset r2 (rget r1);
        epilogue
    | MOV_IMM (w, r) ->
        rset r w;
        epilogue
    | NOP -> epilogue
    | JZ r ->
        if flag_z c then rset PC @@ rget r
          (* Else: next instruction is the following one *);
        epilogue
    | JMP r ->
        rset PC @@ rget r;
        epilogue
    | NOT r ->
        rset r (lnot (rget r));
        epilogue
    | CMP (r1, r2) ->
        let value, overflow = Overflow.(rget r1 - rget r2) in
        set_status_register_flags value overflow;
        epilogue
    | AND (r1, r2) ->
        let value, overflow = Overflow.(rget r1 land rget r2) in
        rset r2 value;
        set_status_register_flags value overflow;
        epilogue
    | ADD (r1, r2) ->
        let value, overflow = Overflow.(rget r1 + rget r2) in
        rset r2 value;
        set_status_register_flags value overflow;
        epilogue
    | SUB (r1, r2) ->
        let value, overflow = Overflow.(rget r1 - rget r2) in
        rset r2 value;
        set_status_register_flags value overflow;
        epilogue

  let step (c : 'io_device configuration) (i : instr) =
    let rget = register_get c.r in
    let rset = register_set c.layout c.r in
    if not (mac_valid c i) then (
      (* CPU-Violation-PM *)
      raise_exception (cycles i) c;
      `ok)
    else (
      (* Set PC old to the current PC *)
      c.pc_old <- rget PC;
      (* Advance to next instruction *)
      rset PC @@ (rget PC + from_int (size i));
      (* Check that the instruction is valid *)
      execute_instruction_semantics i c)
end
