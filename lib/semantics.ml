open Ast
open Config
open Config_monad
open Halt_error
open Interrupt_logic
open Mac
open Register_file
open Serialization
open Types.Word

module Semantics (I : Interrupt_logic) = struct
  let execute_instruction_semantics i =
    let rset_bit r mask v = rmodify r (set_bit mask v) in
    let set_status_register_flags v is_overflow =
      rset_bit SR mask_n (v < zero) >>
      rset_bit SR mask_z (v == zero) >>
      rset_bit SR mask_c (v <> zero) >>
      rset_bit SR mask_v is_overflow
    in
    let epilogue =
      advance_config (cycles i) >>
      I.interrupt_logic
    in
    let arithmetic_operation_semantics r1 r2 op ?(change_r2 = true) =
      let* v1 = rget r1 in
      let* v2 = rget r2 in
      let v, overflow = op v1 v2 in
      (if change_r2 then rset r2 v else ok) >>
      set_status_register_flags v overflow >>
      epilogue
    in
    match i with
    | HLT -> (
        let@ cpu_mode in
        match cpu_mode with
        | None -> halt ExecutingEnclaveData
        | Some UM -> halt HaltUM
        | Some PM -> raise_exception HaltPM (cycles i))
    | IN r -> (
        let@ read_transition in
        match read_transition with
        | None -> halt NoIn
        | Some (w, d') ->
            rset r w >>
            set_io_state d' >>
            increment_current_clock >>
            advance_config Stdlib.(cycles i - 1) >>
            (* Advance must do one cycle less *)
            I.interrupt_logic)
    | OUT r -> (
        let* word = rget r in
        let@ write_transitions in
        match write_transitions word with
        | None -> halt NoOut
        | Some d' ->
            set_io_state d' >>
            increment_current_clock >>
            advance_config Stdlib.(cycles i - 1) >>
            (* Advance must do one cycle less *)
            I.interrupt_logic)
    | RETI -> (
          let@ backup in
          match backup with
          | Some b -> (
            advance_config (cycles i) >>
            let@ arrival_time in
            let@ flag_gie in
            match arrival_time with
            | Some _ when flag_gie ->
                (* CPU-Reti-Chain *)
                I.interrupt_logic
                (* Necessarily the pending protected case! Because ta'!=\bot && b!=\bot *)
            | _ ->
                (* CPU-Reti-PrePad *)
                set_backup None >>
                set_register_file b.r >>
                set_pc_old b.pc_old >>
                (* CPU-Reti-Pad *)
                advance_config b.t_pad >>
                I.interrupt_logic)
        | None ->
            (* CPU-Reti *)
            (* No backup is found; we are in unprotected mode *)
            (* The point to return to is saved on the stack *)
            let@ load_here in
            let@ sp in
            rset PC @@ load_here (sp + from_int 2) >>
            (* Jump to the saved state *)
            (* PC can be safely set in the step function since we overwrite it here. *)
            rset SR @@ load_here sp >>
            (* Restore SR *)
            rset SP @@ (sp + from_int 4) >>
            (* Clean up the stack *)
            advance_config (cycles i)
            (* NO INTERRUPT LOGIC HERE! *))
    | MOV_LOAD (r1, r2) ->
        rget r1 >>= mget >>= rset r2 >>
        epilogue
    | MOV_STORE (r1, r2) ->
        let* v2 = rget r2 in
        rget r1 >>= mset v2 >>
        epilogue
    | MOV (r1, r2) ->
        rget r1 >>= rset r2 >>
        epilogue
    | MOV_IMM (w, r) ->
        rset r w >>
        epilogue
    | NOP -> epilogue
    | JZ r ->
        let@ flag_z in
        (if flag_z then rget r >>= rset PC else ok) >>
          (* Else: next instruction is the following one *)
        epilogue
    | JMP r ->
        rget r >>= rset PC >>
        epilogue
    | NOT r ->
        let* v = rget r in
        rset r (lnot v) >>
        epilogue
    | CMP (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(-) ~change_r2:false
    | AND (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(land) ~change_r2:true
    | ADD (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(+) ~change_r2:true
    | SUB (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(-) ~change_r2:true

  let step (i : instr) =
    let* c = get in
    if not (mac_valid i c) then
      (* CPU-Violation-PM *)
      raise_exception MemoryViolation (cycles i)
    else (
      let* pc = rget PC in
      (* Set PC old to the current PC *)
      set_pc_old pc >>
      (* Advance to next instruction *)
      rset PC @@ pc + from_int (size i) >>
      (* Check that the instruction is valid *)
      execute_instruction_semantics i)

  let auto_step () =
    let@ pc in
    let@ i = with_memory (fetch_and_decode pc) in
    match i with
    | None -> raise_exception DecodeFail 0
    | Some i -> step i

  let rec run max_steps c =
    match max_steps with
    | 0 -> (TooMuchTime, c)
    | max_steps ->
      match auto_step () c with
      | (`ok (), c') -> run Stdlib.(max_steps - 1) c'
      | (`halt e, c') -> (e, c')
end
