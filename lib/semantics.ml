open Memory
open Register_file
open Ast
open Configuration
open Io_device
open Halt_error

let advance_device k c =
  let io_state, current_clock, arrival_time =
    advance c.io_device k (c.io_state, c.current_clock, c.arrival_time)
  in
  c.io_state <- io_state;
  c.current_clock <- current_clock;
  c.arrival_time <- arrival_time

let interrupt_logic c =
  let pure = Result.ok () in
  let halt = Result.error in
  let rget = register_get c.r in
  let rset = register_set c.enclave c.r in
  let mset = memory_set c.m in
  if not c.manage_interrupts then pure
  else
    match c.arrival_time with
    | Some ta when flag_gie c -> (
        match cpu_mode c with
        | None -> halt ExecutingEnclaveData
        | Some UM ->
            (* Push PC and SR in memory *)
            mset (rget SP - 2) @@ rget PC;
            mset (rget SP - 4) @@ rget SR;
            (* Jump to the ISR *)
            rset PC c.isr;
            rset SR 0;
            rset SP (rget SP - 4);
            advance_device (2 * cycles_per_access) c;
            pure
        | Some PM ->
            let t_pad = c.current_clock - ta in
            let k = max_cycles - t_pad in
            c.b <- Some { r = c.r; t_pad; pc_old = c.pc_old };
            c.r <- register_file_0 ();
            c.r.pc <- c.isr;
            advance_device (6 + k) c;
            pure)
    | _ -> pure

let execute_instruction_semantics i c : (unit, halt_error) result =
  let pure = Result.ok () in
  let halt = Result.error in
  let epilogue =
    advance_device (cycles i) c;
    interrupt_logic c
  in
  let mset = memory_set c.m in
  let mget = memory_get c.m in
  let rget = register_get c.r in
  let rset = register_set c.enclave c.r in
  let rset_bit r mask v = rset r @@ set_bit mask v (rget r) in
  let set_status_register_flags v =
    rset_bit SR mask_n (v < 0);
    rset_bit SR mask_z (v == 0);
    rset_bit SR mask_c (v <> 0);
    rset_bit SR mask_v (is_overflow v)
  in
  match i with
  | HLT -> (
      match cpu_mode c with
      | None -> halt ExecutingEnclaveData
      | Some UM -> halt HaltUM
      | Some PM ->
          raise_exception (cycles i) c;
          pure)
  | IN r -> (
      match (io_device_choices c).read_transition with
      | None -> halt NoIn
      | Some (w, d') ->
          rset r @@ w;
          c.io_state <- d';
          advance_device (cycles i - 1) c;
          (* Advance must do one cycle less *)
          interrupt_logic c)
  | OUT r -> (
      match (io_device_choices c).write_transitions (rget r) with
      | None -> halt NoOut
      | Some d' ->
          c.io_state <- d';
          advance_device (cycles i - 1) c;
          (* Advance must do one cycle less *)
          interrupt_logic c)
  | RETI -> (
      match c.b with
      | Some b -> (
          advance_device (cycles i) c;
          match c.arrival_time with
          | Some _ when flag_gie c ->
              (* CPU-Reti-Chain *)
              interrupt_logic c
              (* Necessarily the pending protected case! Because ta'!=\bot && b!=\bot *)
          | _ ->
              (* CPU-Reti-PrePad *)
              c.b <- None;
              c.r <- b.r;
              c.pc_old <- b.pc_old;
              (* CPU-Reti-Pad *)
              advance_device b.t_pad c;
              interrupt_logic c)
      | None ->
          (* CPU-Reti *)
          (* No backup is found; we are in unprotected mode *)
          (* The point to return to is saved on the stack *)
          rset PC @@ mget (rget SP + 2);
          (* Jump to the saved state *)
          (* PC can be safely set in the step function since we overwrite it here. *)
          rset SR @@ mget (rget SP);
          (* Restore SR *)
          rset SP @@ (rget SP + 4);
          (* Clean up the stack *)
          advance_device (cycles i) c;
          (* NO INTERRUPT LOGIC HERE! *)
          pure)
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
  | CMP (r1, r2) ->
      let diff = rget r1 - rget r2 in
      rset r2 diff;
      set_status_register_flags diff;
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
  | AND (r1, r2) ->
      let and_v = rget r1 land rget r2 in
      rset r2 and_v;
      set_status_register_flags and_v;
      epilogue
  | ADD (r1, r2) ->
      let add = rget r1 + rget r2 in
      rset r2 add;
      set_status_register_flags add;
      epilogue
  | SUB (r1, r2) ->
      let sub = rget r1 - rget r2 in
      rset r2 sub;
      set_status_register_flags sub;
      epilogue

let step (c : configuration) (i : instr) : (unit, halt_error) result =
  let rget = register_get c.r in
  let rset = register_set c.enclave c.r in
  let pure = Result.ok () in
  if not (mac_valid c i) then (
    (* CPU-Violation-PM *)
    raise_exception (cycles i) c;
    pure)
  else (
    (* Set PC old to the current PC *)
    c.pc_old <- rget PC;
    (* Advance to next instruction *)
    rset PC @@ (rget PC + size i);
    (* Check that the instruction is valid *)
    execute_instruction_semantics i c)
