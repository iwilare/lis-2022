open QCheck2
open QCheck2.Gen
open Lis2022.Register_file
open Lis2022.Configuration
open Lis2022.Memory
open Lis2022.Ast
open Lis2022.Io_device
open Lis2022.Types
open Lis2022.Semantics
open Generators

open Semantics (Lis2022.Interrupt_logic.Sancus_high)

let is_ok x = x = `ok

(* TODO: parameterize this test over the constructor and the operation for SUB, AND, without duplicating *)
let test_operation name operation instruction =
  let property (c, r1, r2) =
    let before_r1 = register_get c.r r1 in
    let before_r2 = register_get c.r r2 in

    let result, v = operation before_r1 before_r2 in

    let i = instruction (r1, r2) in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in

    let vsr = get_bit mask_v (register_get c.r SR) in

    let overflow = v && vsr in

    let unchanged_r1 = r1 == r2 || register_get c.r r1 = before_r1 in

    valid
    ==> (good && unchanged_r1 && (overflow || register_get c.r r2 = result))
  in
  let gen =
    triple
      (Configuration.configuration_unprotected_minimal ())
      Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:(name ^ " changes the first register with the correct value")
    ~count:20000
    ~print:(fun (c, _, _) -> string_of_configuration c)
    gen property

let test_not =
  let property (c, r) =
    let before_r = register_get c.r r in

    let i = NOT r in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in

    valid ==> (good && register_get c.r r == Word.lnot before_r)
  in
  let gen =
    pair
      (Configuration.configuration_unprotected_minimal ())
      Register.gp_register
  in
  QCheck2.Test.make ~name:"NOT changes the register with the correct value"
    ~count:20000 gen property

let test_mov =
  let property (c, r1, r2) =
    let before_r1 = register_get c.r r1 in

    let i = MOV (r1, r2) in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in

    valid
    ==> (good
        && before_r1 == register_get c.r r1
        && before_r1 == register_get c.r r2)
  in
  let gen =
    triple
      (Configuration.configuration_unprotected_minimal ())
      Register.gp_register Register.gp_register
  in
  QCheck2.Test.make
    ~name:
      "MOV changes the second register with the correct value from the first"
    ~count:20000 gen property

let test_movi =
  let property (c, w, r) =
    let i = MOV_IMM (w, r) in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in

    valid ==> (good && register_get c.r r == w)
  in
  let gen =
    triple
      (Configuration.configuration_unprotected_minimal ())
      Memory.word Register.gp_register
  in
  QCheck2.Test.make
    ~name:"MOV_IMM changes the register with the immediate value provided"
    ~count:20000 gen property

let test_load_um =
  let property (c, r1, r2, unprotected_addr) =
    (* Set the address in r1 *)
    register_set c.layout c.r r1 unprotected_addr;

    let before_r1 = register_get c.r r1 in

    let i = MOV_LOAD (r1, r2) in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in
    let unchanged_r1 = r1 == r2 || register_get c.r r1 = before_r1 in
    let changed_r2 = register_get c.r r2 == memory_get c.m before_r1 in
    valid ==> (good && changed_r2 && unchanged_r1)
  in
  let gen =
    Configuration.configuration_unprotected_minimal () >>= fun config ->
    QCheck2.Gen.quad (pure config) Register.gp_register Register.gp_register
      (Memory.unprotected_address config.layout)
  in
  QCheck2.Test.make
    ~name:
      "MOV_LOAD (UM) changes the memory with the correct value from the \
       register"
    ~count:20000 gen property

let test_store_um =
  let property (c, r1, r2, unprotected_addr) =
    (* Set the address in r2 *)
    register_set c.layout c.r r2 unprotected_addr;

    let before_r1 = register_get c.r r1 in
    let before_r2 = register_get c.r r2 in

    let i = MOV_STORE (r1, r2) in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in
    let changed_memory = register_get c.r r1 == memory_get c.m before_r2 in
    let unchanged_r1 = register_get c.r r1 = before_r1 in
    let unchanged_r2 = register_get c.r r2 = before_r2 in

    valid ==> (good && changed_memory && unchanged_r1 && unchanged_r2)
  in
  let gen =
    Configuration.configuration_unprotected_minimal () >>= fun config ->
    QCheck2.Gen.quad (pure config) Register.gp_register Register.gp_register
      (Memory.unprotected_address config.layout)
  in
  QCheck2.Test.make
    ~name:
      "MOV_STORE (UM) changes the register with the correct value from the \
       memory"
    ~count:20000 gen property

let test_j0_um_yes =
  let property (c, r, unprotected_addr) =
    (* Set the address in r1 *)
    register_set c.layout c.r r unprotected_addr;

    let before_r = register_get c.r r in
    let before_pc = c.r.pc in

    let z = get_bit mask_z c.r.sr in

    let i = JZ r in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in
    let unchanged_r = register_get c.r r = before_r in

    valid
    ==> (good && unchanged_r
        && QCheck2.(
             (not z) ==> Word.(c.r.pc = before_pc + Word.from_int (size i))))
  in
  let gen =
    Configuration.configuration_unprotected_minimal () >>= fun config ->
    QCheck2.Gen.triple (pure config) Register.gp_register
      (Memory.unprotected_address config.layout)
  in
  QCheck2.Test.make
    ~name:"J0 (UM) goes to the next instruction if the flag is false" ~count:20000
    gen property

let test_j0_um_no =
  let property (c, r, unprotected_addr) =
    (* Set the address in r1 *)
    register_set c.layout c.r r unprotected_addr;

    let before_r = register_get c.r r in

    let z = get_bit mask_z c.r.sr in

    let i = JZ r in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in
    let unchanged_r = register_get c.r r = before_r in

    valid
    ==> (good && unchanged_r && QCheck2.(z ==> (c.r.pc = register_get c.r r)))
  in
  let gen =
    Configuration.configuration_unprotected_minimal () >>= fun config ->
    QCheck2.Gen.triple (pure config) Register.gp_register
      (Memory.unprotected_address config.layout)
  in
  QCheck2.Test.make ~name:"J0 (UM) jumps if the flags is true" ~count:20000 gen
    property

let test_jmp_um =
  let property (c, r, unprotected_addr) =
    (* Set the address in r *)
    register_set c.layout c.r r unprotected_addr;

    let before_r1 = register_get c.r r in

    let i = JMP r in
    let valid = mac_valid c i in
    let good = step c i |> is_ok in
    let unchanged_r1 = register_get c.r r = before_r1 in
    let changed_pc = c.r.pc == register_get c.r r in
    valid ==> (good && changed_pc && unchanged_r1)
  in
  let gen =
    Configuration.configuration_unprotected_minimal () >>= fun config ->
    QCheck2.Gen.triple (pure config) Register.gp_register
      (Memory.unprotected_address config.layout)
  in
  QCheck2.Test.make
    ~name:"JMP (UM) sets the PC with correct value from the register" ~count:20000
    gen property

let test_in_device =
  let property (c, r) =
    let i = IN r in
    match (io_device_choices c).read_transition with
    | None -> 
        let valid = mac_valid c i in 
        valid ==> (step c i  = `halt NoIn)
    | Some (w, d') ->
        let valid = mac_valid c i in
        let good = step c i |> is_ok in
        let correct_state, _, _ =
          advance c.io_device (cycles i - 1) (d', 0, None)
        in
        valid
        ==> (good
            && c.io_state == correct_state
            && register_get c.r r = w (* Changed *))
  in
  let gen =
    let* io_device = Io_device.device 5 5 in
    pair
      (Configuration.configuration_unprotected_minimal () ~io_device)
      Register.gp_register
  in
  QCheck2.Test.make ~name:"IN performs a read from the device" ~count:100000 gen
    property ~print:(fun (c, r) ->
      "------------CONFIG:------------\n" ^ string_of_configuration c
      ^ "\n------------REGISTER---------------\n"
      ^ string_of_register r
      ^ "\n------------VALUE---------------\n"
      ^ Word.show (register_get c.r r)
      ^ "\n------------IO DEVICE---------------\n"
      ^ string_of_io_device c.io_device)

let test_out_device =
  let property (c, r, w) =
    register_set c.layout c.r r w;
    let i = OUT r in
    match List.assoc_opt (register_get c.r r) (io_device_choices c).write_transitions with
    | None -> 
      let valid = mac_valid c i in 
        valid ==> (step c i  = `halt NoOut)
    | Some d' ->
        let valid = mac_valid c i in
        let good = step c i |> is_ok in
        let correct_state, _, _ =
          advance c.io_device (cycles i - 1) (d', 0, None)
        in

        valid
        ==> (good
            && c.io_state == correct_state
            && register_get c.r r = w (* Unchanged *))
  in
  let gen =
    let* write_transitions_max = 0 -- 16 in
    let* io_device = Io_device.device 5 write_transitions_max in
    triple
      (Configuration.configuration_unprotected_minimal () ~io_device)
      Register.gp_register
      (0 -- write_transitions_max >|= Word.from_int)
  in
  QCheck2.Test.make ~name:"OUT performs a read from the device" ~count:100000 gen
    property ~print:(fun (c, r, w) ->
      "CONFIG: \n" ^ string_of_configuration c ^ "\n REGISTER "
      ^ string_of_register r ^ " VALUE: "
      ^ Word.show (register_get c.r r)
      ^ "\nWORD: " ^ Word.show w)

let tests =
  [
    test_operation "ADD" Word.Overflow.( + ) (fun (r1, r2) -> ADD (r1, r2));
    test_operation "SUB" Word.Overflow.( - ) (fun (r1, r2) -> SUB (r1, r2));
    test_operation "AND" Word.Overflow.( land ) (fun (r1, r2) -> AND (r1, r2));
    test_not;
    test_mov;
    test_movi;
    test_store_um;
    test_load_um;
    test_j0_um_yes;
    test_j0_um_no;
    test_jmp_um;
    test_in_device;
    test_out_device;
  ]
