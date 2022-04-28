open Memory
open Register_file
open Ast
open Configuration
open Io_device
open Error

let execute_instruction_semantics i c =
  let mset = memory_set c.m in
  let mget = memory_get c.m in
  let rget = register_get c.r in
  let rset = register_set c.enclave c.r in
  let rget_bit r mask = get_bit mask (rget r) in
  let rset_bit r mask v = rset r @@ set_bit mask v (rget r) in
  let set_status_register_flags v = begin
    rset_bit SR flag_n (v <  0);
    rset_bit SR flag_z (v == 0);
    rset_bit SR flag_c (v <> 0);
    rset_bit SR flag_v (is_overflow v) end in
    match i with
    | HLT -> failwith "Not implemented yet"
    | IN _ -> failwith "Not implemented yet"
    | OUT _ -> failwith "Not implemented yet"
    | MOV_LOAD (r1, r2) -> rset r2 @@ mget (rget r1) |> Result.ok
    | MOV_STORE (r1, r2) -> mset (rget r2) (rget r1) |> Result.ok
    | MOV(r1, r2) -> rset r2 (rget r1) |> Result.ok
    | MOV_IMM(w, r) -> rset r w |> Result.ok
    | CMP(r1, r2) -> Result.ok begin
        let diff = rget r1 - rget r2 in
        rset r2 diff;
        set_status_register_flags diff;
    end
    | NOP -> Ok(())
    | RETI -> Result.ok begin
        rset PC @@ mget (rget SP + 2);
        rset SR @@ mget (rget SP + 2);
        rset SP @@ rget SP + 4
    end
    | JZ(r) -> Result.ok begin
        if rget_bit r flag_z then
            rset PC @@ rget r
        else
            ()
    end
    | JMP(r) -> rset PC @@ rget r |> Result.ok
    | NOT(r) -> rset r (lnot (rget r)) |> Result.ok
    | AND(r1, r2) -> Result.ok begin
        let and_v = rget r1 land rget r2 in
        rset r2 and_v;
        set_status_register_flags and_v
    end
    | ADD(r1, r2) -> Result.ok begin
        let add = rget r1 + rget r2 in
        rset r2 add;
        set_status_register_flags add
    end
    | SUB(r1, r2) -> Result.ok begin
        let sub = rget r1 - rget r2 in
        rset r2 sub;
        set_status_register_flags sub
    end

let advance_configuration k c = begin
  let (io_state, current_clock, arrival_time) = advance c.io_device k (c.io_state, c.current_clock, c.arrival_time) in
  c.io_state <- io_state;
  c.current_clock <- current_clock;
  c.arrival_time <- arrival_time;
end

let interrupt_logic _ = begin
end

let step (c : configuration) (i : instr) : (configuration, error) result =
  let rget = register_get c.r in
  let rset = register_set c.enclave c.r in
  let (>>=) = Result.bind in
  match c.b with
  | Backup _ -> failwith "Not implemented yet"
  | Padding _ -> failwith "Not implemented yet"
  | Bottom ->
    (* Set PC old to the current PC *)
    c.pc_old <- rget PC;
    (* Advance to next instruction *)
    rset PC @@ rget PC + size i;
    (* Check that the instruction is valid *)
    if not (mac_valid c i) then
      Ok(exc_configuration c)
    else begin
      execute_instruction_semantics i c >>= fun () ->
      advance_configuration (cycles i) c;
      interrupt_logic c;
      Ok(c)
    end