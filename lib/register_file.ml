open Ast
open Memory

let zero : word = 0b00000000
let flag_gie : word = 0b00001000
let flag_v : word = 0b00010000
let flag_n : word = 0b00100000
let flag_z : word = 0b01000000
let flag_c : word = 0b10000000

type register_file = {
  mutable pc : word;
  mutable sp : word;
  mutable sr : word;
  mutable r3 : word;
  mutable r4 : word;
  mutable r5 : word;
  mutable r6 : word;
  mutable r7 : word;
  mutable r8 : word;
  mutable r9 : word;
  mutable r10 : word;
  mutable r11 : word;
  mutable r12 : word;
  mutable r13 : word;
  mutable r14 : word;
  mutable r15 : word;
}

let register_get r = function
  | PC -> r.pc
  | SP -> r.sp
  | SR -> r.sr
  | R3 -> r.r3
  | R4 -> r.r4
  | R5 -> r.r5
  | R6 -> r.r6
  | R7 -> r.r7
  | R8 -> r.r8
  | R9 -> r.r9
  | R10 -> r.r10
  | R11 -> r.r11
  | R12 -> r.r12
  | R13 -> r.r13
  | R14 -> r.r14
  | R15 -> r.r15

let align_even x = x land 0xFFFE
let get_bit mask x = x land mask > 0
let set_bit mask b x = x lor if b then mask else zero

let register_set enclave regs r w =
  match r with
  | PC -> regs.pc <- align_even w
  | SP -> regs.sp <- align_even w
  | SR -> (
      match cpu_mode_of_address enclave (register_get regs PC) with
      | Some PM ->
          regs.sr <-
            w |> set_bit flag_gie (get_bit flag_gie (register_get regs SR))
      | _ ->
          (* Remember Some(UM) *)
          regs.sr <- w)
  | R3 -> regs.r3 <- w
  | R4 -> regs.r4 <- w
  | R5 -> regs.r5 <- w
  | R6 -> regs.r6 <- w
  | R7 -> regs.r7 <- w
  | R8 -> regs.r8 <- w
  | R9 -> regs.r9 <- w
  | R10 -> regs.r10 <- w
  | R11 -> regs.r11 <- w
  | R12 -> regs.r12 <- w
  | R13 -> regs.r13 <- w
  | R14 -> regs.r14 <- w
  | R15 -> regs.r15 <- w

let register_file_0 () =
  {
    pc = 0;
    sp = 0;
    sr = 0;
    r3 = 0;
    r4 = 0;
    r5 = 0;
    r6 = 0;
    r7 = 0;
    r8 = 0;
    r9 = 0;
    r10 = 0;
    r11 = 0;
    r12 = 0;
    r13 = 0;
    r14 = 0;
    r15 = 0;
  }

let register_file_init m () =
  {
    pc = memory_get m 0xFFFE;
    sp = 0;
    sr = flag_gie;
    r3 = 0;
    r4 = 0;
    r5 = 0;
    r6 = 0;
    r7 = 0;
    r8 = 0;
    r9 = 0;
    r10 = 0;
    r11 = 0;
    r12 = 0;
    r13 = 0;
    r14 = 0;
    r15 = 0;
  }
