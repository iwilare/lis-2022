open Memory
open Register_file
open Ast
open Configuration
open Io_device
open Halt_error

let interrupt_logic _ = ()

let advance_configuration k c =
  let io_state, current_clock, arrival_time =
    advance c.io_device k (c.io_state, c.current_clock, c.arrival_time)
  in
  c.io_state <- io_state;
  c.current_clock <- current_clock;
  c.arrival_time <- arrival_time

let execute_instruction_semantics i c : (unit, halt_error) result =
  let pure = Result.ok () in
  let halt = Result.error in
  let epilogue =
    advance_configuration (cycles i) c;
    interrupt_logic c;
    pure
  in
  let mset = memory_set c.m in
  let mget = memory_get c.m in
  let rget = register_get c.r in
  let rset = register_set c.enclave c.r in
  let rget_bit r mask = get_bit mask (rget r) in
  let rset_bit r mask v = rset r @@ set_bit mask v (rget r) in
  let set_status_register_flags v =
    rset_bit SR flag_n (v < 0);
    rset_bit SR flag_z (v == 0);
    rset_bit SR flag_c (v <> 0);
    rset_bit SR flag_v (is_overflow v)
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
    match (get_io_device_possibilities c).read_transition with
    | None -> halt NoIn
    | Some (w, d') ->
      rset r @@ w;
      c.io_state <- d';
      advance_configuration (cycles i - 1) c;
      (* Advance must do one cycle less *)
      interrupt_logic c;
      pure)
  | OUT r -> (
    match (get_io_device_possibilities c).write_transitions (rget r) with
    | None -> halt NoOut
    | Some d' ->
      c.io_state <- d';
      advance_configuration (cycles i - 1) c;
      (* Advance must do one cycle less *)
      interrupt_logic c;
      pure)
  | RETI -> (
    match c.b with
    | Some b -> (
      advance_configuration (cycles i) c;
      match c.arrival_time with
      | Some _ when get_bit flag_gie c.r.sr ->
        (* CPU-Reti-Chain *)
        interrupt_logic c;
        (* Necessarily the pending protected case! Because ta'!=\bot && b!=\bot *)
        pure
      | _ ->
        (* CPU-Reti-PrePad *)
        c.r <- b.r;
        c.pc_old <- b.pc_old;
        (* CPU-Reti-Pad *)
        c.b <- None;
        advance_configuration b.t_pad c;
        interrupt_logic c;
        pure)
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
      advance_configuration (cycles i) c;
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
    if rget_bit r flag_z then rset PC @@ rget r
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
