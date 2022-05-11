open Types
open Ast
open Memory

let zero : word = Word.from_int 0b00000000
let mask_gie : word = Word.from_int 0b00001000
let mask_v : word = Word.from_int 0b00010000
let mask_n : word = Word.from_int 0b00100000
let mask_z : word = Word.from_int 0b01000000
let mask_c : word = Word.from_int 0b10000000

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

let string_of_register_file_gp r =
  "[R3: " ^ Word.show r.r3 ^ "]" ^ " [R4: " ^ Word.show r.r4 ^ "]" ^ " [R5: "
  ^ Word.show r.r5 ^ "]"
  (*" [R6: " ^ Word.show r.r6 ^ "]" ^
    " [R7: " ^ Word.show r.r7 ^ "]" ^
    " [R8: " ^ Word.show r.r8 ^ "]" ^
    " [R9: " ^ Word.show r.r9 ^ "]" ^
    " [R10: " ^ Word.show r.r10 ^ "]" ^
    " [R11: " ^ Word.show r.r11 ^ "]" ^
    " [R12: " ^ Word.show r.r12 ^ "]" ^
    " [R13: " ^ Word.show r.r13 ^ "]" ^
    " [R14: " ^ Word.show r.r14 ^ "]" ^*) ^ " ..."
  ^ " [R15: " ^ Word.show r.r15 ^ "]"

let string_of_register = function
  | PC -> "PC"
  | SP -> "SP"
  | SR -> "SR"
  | R3 -> "R3"
  | R4 -> "R4"
  | R5 -> "R5"
  | R6 -> "R6"
  | R7 -> "R7"
  | R8 -> "R8"
  | R9 -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"
  | R12 -> "R12"
  | R13 -> "R13"
  | R14 -> "R14"
  | R15 -> "R15"

let register_get regs = function
  | PC -> regs.pc
  | SP -> regs.sp
  | SR -> regs.sr
  | R3 -> regs.r3
  | R4 -> regs.r4
  | R5 -> regs.r5
  | R6 -> regs.r6
  | R7 -> regs.r7
  | R8 -> regs.r8
  | R9 -> regs.r9
  | R10 -> regs.r10
  | R11 -> regs.r11
  | R12 -> regs.r12
  | R13 -> regs.r13
  | R14 -> regs.r14
  | R15 -> regs.r15

let get_bit mask x = Word.(x land mask > zero)

let set_bit mask b (x : word) =
  Word.(if b then x lor mask else x land lnot mask)

let string_of_sr_flags sr =
  let string_of_bool b = if b then "1" else "0" in
  "GIE="
  ^ string_of_bool (get_bit mask_gie sr)
  ^ " V="
  ^ string_of_bool (get_bit mask_v sr)
  ^ " N="
  ^ string_of_bool (get_bit mask_n sr)
  ^ " Z="
  ^ string_of_bool (get_bit mask_z sr)
  ^ " C="
  ^ string_of_bool (get_bit mask_c sr)

let register_set layout regs r w =
  match r with
  | PC -> regs.pc <- align_even w
  | SP -> regs.sp <- align_even w
  | SR -> (
      match cpu_mode_of_address layout regs.pc with
      | Some PM -> regs.sr <- w |> set_bit mask_gie (get_bit mask_gie regs.sr)
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

let copy_register_file r1 r2 =
  r1.pc <- r2.pc;
  r1.sp <- r2.sp;
  r1.sr <- r2.sr;
  r1.r3 <- r2.r3;
  r1.r4 <- r2.r4;
  r1.r5 <- r2.r5;
  r1.r6 <- r2.r6;
  r1.r7 <- r2.r7;
  r1.r8 <- r2.r8;
  r1.r9 <- r2.r9;
  r1.r10 <- r2.r10;
  r1.r11 <- r2.r11;
  r1.r12 <- r2.r12;
  r1.r13 <- r2.r13;
  r1.r14 <- r2.r14;
  r1.r15 <- r2.r15

let register_file_0 () =
  Word.
    {
      pc = zero;
      sp = zero;
      sr = zero;
      r3 = zero;
      r4 = zero;
      r5 = zero;
      r6 = zero;
      r7 = zero;
      r8 = zero;
      r9 = zero;
      r10 = zero;
      r11 = zero;
      r12 = zero;
      r13 = zero;
      r14 = zero;
      r15 = zero;
    }

let register_file_init m () =
  {
    pc = memory_get m (Word.from_int 0xFFFE);
    sp = zero;
    sr = mask_gie;
    r3 = zero;
    r4 = zero;
    r5 = zero;
    r6 = zero;
    r7 = zero;
    r8 = zero;
    r9 = zero;
    r10 = zero;
    r11 = zero;
    r12 = zero;
    r13 = zero;
    r14 = zero;
    r15 = zero;
  }
