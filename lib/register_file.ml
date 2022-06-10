open Types
open Memory
open Cpu_mode

type register =
  | PC
  | SP
  | SR
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

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

type register_file = {
  pc : word;
  sp : word;
  sr : word;
  r3 : word;
  r4 : word;
  r5 : word;
  r6 : word;
  r7 : word;
  r8 : word;
  r9 : word;
  r10 : word;
  r11 : word;
  r12 : word;
  r13 : word;
  r14 : word;
  r15 : word;
}

let string_of_register_file_gp r =
  "[R3: " ^ Word.show r.r3 ^ "] " ^ "[R4: " ^ Word.show r.r4 ^ "] " ^ "[R5: "
  ^ Word.show r.r5 ^ "] " ^ "[R6: " ^ Word.show r.r6 ^ "] " ^ "[R7: "
  ^ Word.show r.r7 ^ "] " ^ "[R8: " ^ Word.show r.r8 ^ "] " ^ "[R9: "
  ^ Word.show r.r9 ^ "] " ^ "[R10: " ^ Word.show r.r10 ^ "] "
  (* "[R11: " ^ Word.show r.r11 ^ "] " ^ *)
  (* "[R12: " ^ Word.show r.r12 ^ "] " ^ *)
  (* "[R13: " ^ Word.show r.r13 ^ "] " ^ *)
  (* "[R14: " ^ Word.show r.r14 ^ "] " ^ *)
  ^ " ... "
  ^ "[R15: " ^ Word.show r.r15 ^ "]"

let register_get reg r =
  match reg with
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

let get_bit mask x = Word.(x land mask > zero)

let set_bit mask b (x : word) =
  Word.(if b then x lor mask else x land lnot mask)

let zero : word = Word.from_int 0b00000000
let mask_gie : word = Word.from_int 0b00001000
let mask_v : word = Word.from_int 0b00010000
let mask_n : word = Word.from_int 0b00100000
let mask_z : word = Word.from_int 0b01000000
let mask_c : word = Word.from_int 0b10000000

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

let register_set layout reg w r =
  match reg with
  | PC -> { r with pc = align_even w }
  | SP -> { r with sp = align_even w }
  | SR -> (
      match cpu_mode_of_address layout r.pc with
      | Some PM -> { r with sr = w |> set_bit mask_gie (get_bit mask_gie r.sr) }
      | _ ->
          (* Remember Some(UM) *)
          { r with sr = w })
  | R3 -> { r with r3 = w }
  | R4 -> { r with r4 = w }
  | R5 -> { r with r5 = w }
  | R6 -> { r with r6 = w }
  | R7 -> { r with r7 = w }
  | R8 -> { r with r8 = w }
  | R9 -> { r with r9 = w }
  | R10 -> { r with r10 = w }
  | R11 -> { r with r11 = w }
  | R12 -> { r with r12 = w }
  | R13 -> { r with r13 = w }
  | R14 -> { r with r14 = w }
  | R15 -> { r with r15 = w }

let register_file_0 =
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

let register_file_init m =
  {
    pc = memory_get w0xFFFE m;
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
