open Memory
open Register_file
open Ast
open Config
open Io_device
open Halt_error
open Interrupt_logic
open Types.Word
open Config_monad

module Semantics (I : Interrupt_logic) = struct
  let execute_instruction_semantics i =
    let rset_bit r mask v = rmodify r (set_bit mask v) in
    let set_status_register_flags v is_overflow =
      let* _ = rset_bit SR mask_n (v < zero) in
      let* _ = rset_bit SR mask_z (v == zero) in
      let* _ = rset_bit SR mask_c (v <> zero) in
      let* _ = rset_bit SR mask_v is_overflow in
      ok
    in
    let epilogue () =
      let* _ = advance_config (cycles i) in
      I.interrupt_logic ()
    in
    let arithmetic_operation_semantics r1 r2 op ?(change_r2 = true) =
      let* v1 = rget r1 in
      let* v2 = rget r2 in
      let v, overflow = op v1 v2 in
      (if change_r2 then rset r2 v else ok) >>
      set_status_register_flags v overflow >>
      epilogue ()
    in
    match i with
    | HLT -> (
        let* c = get in
        match cpu_mode c with
        | None -> halt ExecutingEnclaveData
        | Some UM -> halt HaltUM
        | Some PM -> raise_exception (cycles i))
    | IN r -> (
        let* c = get in
        match (io_device_choices c).read_transition with
        | None -> halt NoIn
        | Some (w, d') ->
            let* _ = rset r w in
            let* _ = set_io_state d' in
            let* _ = increment_current_clock () in
            let* _ = advance_config Stdlib.(cycles i - 1) in
            (* Advance must do one cycle less *)
            I.interrupt_logic ())
    | OUT r -> (
        let* c = get in
        let* word = rget r in
        match List.assoc_opt word (io_device_choices c).write_transitions with
        | None -> halt NoOut
        | Some d' ->
            let* _ = set_io_state d' in
            let* _ = increment_current_clock () in
            let* _ = advance_config Stdlib.(cycles i - 1) in
            (* Advance must do one cycle less *)
            I.interrupt_logic ())
    | RETI -> (
        let* c = get in
        match c.b with
        | Some b -> (
            let* _ = advance_config (cycles i) in
            let* c = get in
            match c.arrival_time with
            | Some _ when flag_gie c ->
                (* CPU-Reti-Chain *)
                I.interrupt_logic ()
                (* Necessarily the pending protected case! Because ta'!=\bot && b!=\bot *)
            | _ ->
                (* CPU-Reti-PrePad *)
                let* _ = set_backup None in
                let* _ = modify (fun c -> {c with r = b.r}) in
                let* _ = set_pc_old b.pc_old in
                (* CPU-Reti-Pad *)
                let* _ = advance_config b.t_pad in
                I.interrupt_logic ())
        | None ->
            (* CPU-Reti *)
            (* No backup is found; we are in unprotected mode *)
            (* The point to return to is saved on the stack *)
            let* c = get in
            let load a = memory_get a c.m in
            let sp = c.r.sr in
            let* _ = rset PC @@ load (sp + from_int 2) in
            (* Jump to the saved state *)
            (* PC can be safely set in the step function since we overwrite it here. *)
            let* _ = rset SR @@ load sp in
            (* Restore SR *)
            let* _ = rset SP @@ (sp + from_int 4) in
            (* Clean up the stack *)
            let* _ = advance_config (cycles i) in
            (* NO INTERRUPT LOGIC HERE! *)
            ok)
    | MOV_LOAD (r1, r2) ->
        rget r1 >>= mget >>= rset r2 >>
        epilogue ()
    | MOV_STORE (r1, r2) ->
        mset <$> rget r2 <*> rget r1 >>
        epilogue ()
    | MOV (r1, r2) ->
        rget r1 >>= rset r2 >>
        epilogue ()
    | MOV_IMM (w, r) ->
        rset r w >>
        epilogue ()
    | NOP -> epilogue ()
    | JZ r ->
        let* c = get in
        (if flag_z c then (rget r >>= rset PC) else ok) >>
          (* Else: next instruction is the following one *)
        epilogue ()
    | JMP r ->
        rget r >>= rset PC >>
        epilogue ()
    | NOT r ->
        let* v = rget r in
        rset r (lnot v) >>
        epilogue ()
    | CMP (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(-) ~change_r2:false
    | AND (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(land) ~change_r2:true
    | ADD (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(+) ~change_r2:true
    | SUB (r1, r2) -> arithmetic_operation_semantics r1 r2 Overflow.(-) ~change_r2:true

  let step (i : instr) =
    let* c = get in
    if not (mac_valid i c) then (
      (* CPU-Violation-PM *)
      raise_exception (cycles i) >> ok)
    else (
      let* pc = rget PC in
      (* Set PC old to the current PC *)
      set_pc_old pc >>
      (* Advance to next instruction *)
      rset PC @@ pc + from_int (size i) >>
      (* Check that the instruction is valid *)
      execute_instruction_semantics i)
end
