open QCheck2
open QCheck2.Gen
open Lis2022.Register_file
open Lis2022.Config
open Lis2022.Memory
open Lis2022.Ast
open Lis2022.Io_device
open Lis2022.Types
open Lis2022.Semantics
open Lis2022.Halt_error
open Generators

open Semantics (Lis2022.Interrupt_logic.Sancus_high)

let show_step = function
  | `ok (), c -> string_of_config c
  | `halt e, _ -> "halt " ^ string_of_halt_error e

let printer_step i c =
  "Executing " ^ string_of_instr i
  ^ "\n\n(before) " ^ string_of_config c
  ^ "\n\n(after) " ^ show_step (step i c)

let step_and_check_instruction i c ~predicate_ok ~predicate_halt =
  match step i c with
  | `ok (), c -> predicate_ok c
  | `halt _, _ -> predicate_halt

let test_operation name operation instruction =
  let property (c, r1, r2) =
    step_and_check_instruction (instruction (r1, r2)) c
      ~predicate_ok: (fun c' ->
        let after_r1 = register_get r1 c'.r in
        let after_r2 = register_get r2 c'.r in
        let before_r1 = register_get r1 c.r in
        let before_r2 = register_get r2 c.r in
        let result, op_overflow = operation before_r1 before_r2 in
        let unchanged_r1 = r1 = r2 || after_r1 = before_r1 in
        let changed_r2 = after_r2 = result in
        (op_overflow = flag_v c') && unchanged_r1 && changed_r2)
      ~predicate_halt: false
  in
  let gen =
    triple (Config.config_unprotected_minimal ()) Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:(name ^ " changes the first register with the correct value")
    ~count:20000
    ~print:(fun (c, r1, r2) -> printer_step (instruction (r1, r2)) c)
    gen property

let test_not =
  let property (c, r) =
    step_and_check_instruction (NOT r) c
      ~predicate_ok: (fun c' ->
        let before_r = register_get r c.r in
        let after_r = register_get r c'.r in
        after_r == Word.lnot before_r)
      ~predicate_halt: false
  in
  let gen =
    pair (Config.config_unprotected_minimal ()) Register.gp_register
  in
  QCheck2.Test.make ~name:"NOT changes the register with the correct value"
    ~print:(fun (c, r) -> printer_step (NOT r) c)
    ~count:20000 gen property

let test_mov =
  let property (c, r1, r2) =
    step_and_check_instruction (MOV (r1, r2)) c
      ~predicate_ok: (fun c' ->
        let before_r1 = register_get r1 c.r in
        let after_r1 = register_get r1 c'.r in
        let after_r2 = register_get r1 c'.r in
           before_r1 == after_r1 && before_r1 == after_r2)
      ~predicate_halt: false
  in
  let gen =
    triple
      (Config.config_unprotected_minimal ())
      Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:
      "MOV changes the second register with the correct value from the first"
    ~print:(fun (c, r1, r2) -> printer_step (MOV (r1, r2)) c)
    ~count:20000 gen property

let test_movi =
  let property (c, w, r) =
    step_and_check_instruction (MOV_IMM (w, r)) c
      ~predicate_ok: (fun c' ->
        let after_r = register_get r c'.r in
        after_r == w)
      ~predicate_halt: false
  in
  let gen =
    triple
      (Config.config_unprotected_minimal ())
      Memory.word Register.gp_register
  in
  QCheck2.Test.make
    ~name:"MOV_IMM changes the register with the immediate value provided"
    ~print:(fun (c, w, r) -> printer_step (MOV_IMM (w, r)) c)
    ~count:20000 gen property

let test_load_um =
  let property (c, r1, r2, unprotected_addr) =
    (* Set the address in r1 *)
    let c = {c with r = register_set c.layout r1 unprotected_addr c.r} in
    step_and_check_instruction (MOV_LOAD (r1, r2)) c
      ~predicate_ok: (fun c' ->
        let before_r1 = register_get r1 c'.r in
        let after_r1 = register_get r1 c'.r in
        let unchanged_r1 = r1 == r2 || after_r1 = before_r1 in
        let changed_r2 = register_get r2 c'.r == memory_get before_r1 c.m in
        changed_r2 && unchanged_r1)
      ~predicate_halt: false
  in
  let gen =
    QCheck2.Gen.((Config.config_unprotected_minimal ()) >>= fun config ->
    quad (pure config) Register.gp_register Register.gp_register
      (Memory.unprotected_address config.layout))
  in
  QCheck2.Test.make
    ~name:
      "MOV_LOAD (UM) changes the memory with the correct value from the register"
    ~print:(fun (c, r1, r2, _) -> printer_step (MOV_LOAD (r1, r2)) c)
    ~count:20000 gen property
let test_store_um =
  let property (c, r1, r2, unprotected_addr) =
    (* Set the address in r2 *)
    let c = {c with r = register_set c.layout r2 unprotected_addr c.r} in
    step_and_check_instruction (MOV_STORE (r1, r2)) c
      ~predicate_ok: (fun c' ->
        let before_r1 = register_get r1 c.r in
        let before_r2 = register_get r2 c.r in
        let after_r1 = register_get r1 c'.r in
        let after_r2 = register_get r2 c'.r in
        let unchanged_r1 = after_r1 = before_r1 in
        let unchanged_r2 = after_r2 = before_r2 in
        let changed_memory = memory_get before_r2 c'.m = before_r1 in
        changed_memory && unchanged_r1 && unchanged_r2)
      ~predicate_halt: false
  in
  let gen =
    QCheck2.Gen.(Config.config_unprotected_minimal () >>= fun config ->
    quad (pure config) Register.gp_register Register.gp_register
      (Memory.unprotected_address config.layout))
  in
  QCheck2.Test.make
    ~name:
      "MOV_STORE (UM) changes the memory with the correct value from the register"
    ~count:20000 gen property
    ~print:(fun (c, r1, r2, _) -> printer_step (MOV_STORE (r1, r2)) c)
let test_jmp_um =
  let property (c, r, unprotected_addr) =
    (* Set the address in r *)
    let c = {c with r = register_set c.layout r unprotected_addr c.r} in
    let i = JMP r in
    step_and_check_instruction i c
      ~predicate_ok: (fun c' ->
        let before_r = register_get r c.r in
        let after_r = register_get r c'.r in
        let unchanged_r = after_r = before_r in
        unchanged_r && Word.(c'.r.pc = before_r))
      ~predicate_halt: false
  in
  let gen =
    QCheck2.Gen.(Config.config_unprotected_minimal () >>= fun config ->
      triple (pure config) Register.gp_register (Memory.unprotected_address config.layout))
  in
  QCheck2.Test.make
    ~name:("JMP jumps to the instruction indicated by the register")
    ~count:20000 gen property

let test_jz_um z_case =
  let property (c, r, unprotected_addr) =
    (* Set the address in r *)
    let c = {c with r = register_set c.layout r unprotected_addr c.r} in
    let i = JZ r in
    step_and_check_instruction i c
      ~predicate_ok: (fun c' ->
        let before_r = register_get r c.r in
        let after_r = register_get r c'.r in
        let unchanged_r = after_r = before_r in
        unchanged_r &&
          (if z_case then
            flag_z c ==> Word.(c'.r.pc = before_r)
          else
            not (flag_z c) ==> Word.(c'.r.pc = c.r.pc + Word.from_int (size i))))
      ~predicate_halt: false
  in
  let gen =
    QCheck2.Gen.(Config.config_unprotected_minimal () >>= fun config ->
      triple (pure config) Register.gp_register (Memory.unprotected_address config.layout))
  in
  QCheck2.Test.make
    ~name:("J0 (UM) goes to the next instruction if the flag is " ^ (if z_case then "not set" else "set"))
    ~count:20000 gen property
    ~print:(fun (c, r, _) -> printer_step (JZ r) c)

let test_in_device =
  let property (c, r) =
    let i = IN r in
    let transition = (io_device_choices c).read_transition in
    step_and_check_instruction i c
    ~predicate_ok: (fun c' ->
      match transition with
      | None -> false
      | Some (w, d') ->
        let expected_state, _, _ = advance c.io_device (cycles i - 1) (d', 0, None) in
        let after_r = register_get r c'.r in
        c'.io_state = expected_state && after_r = w)
    ~predicate_halt:(Option.is_none transition)
  in
  let gen =
    QCheck2.Gen.(let* io_device = Io_device.device 5 5 in
                 pair (Config.config_unprotected_minimal () ~io_device) Register.gp_register)
  in
  QCheck2.Test.make ~name:"IN performs a read from the device" ~count:100000 gen property

let test_out_device =
  let property (c, r, word_to_write) =
    let i = OUT r in
    (* Set the word to be written in r *)
    let c = {c with r = register_set c.layout r word_to_write c.r} in
    let transition = List.assoc_opt (register_get r c.r) (io_device_choices c).write_transitions in
    step_and_check_instruction i c
    ~predicate_ok: (fun c' ->
      match transition with
      | None -> false
      | Some d' ->
        let expected_state, _, _ = advance c.io_device (cycles i - 1) (d', 0, None) in
        let after_r = register_get r c'.r in
        c'.io_state = expected_state && after_r = word_to_write)
    ~predicate_halt:(Option.is_none transition)
  in
  let gen =
    QCheck2.Gen.(let* io_device = Io_device.device 5 5 in
                 let word_to_write = 0 -- 16 >|= Word.from_int in
                 triple (Config.config_unprotected_minimal () ~io_device) Register.gp_register word_to_write)
  in
  QCheck2.Test.make ~name:"OUT performs a read from the device" ~count:100000 gen property

let tests =
  [
    test_operation "ADD" Word.Overflow.( + ) (fun (r1, r2) -> ADD (r1, r2));
    test_operation "SUB" Word.Overflow.( - ) (fun (r1, r2) -> SUB (r1, r2));
    test_operation "AND" Word.Overflow.( land ) (fun (r1, r2) -> AND (r1, r2));
    test_not;
    test_mov;
    test_movi;
    test_load_um;
    test_store_um;
    test_jmp_um;
    test_jz_um true;
    test_jz_um false;
    test_in_device;
    test_out_device;
  ]
