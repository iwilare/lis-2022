open Ast
open Memory

let gie : word = 0b00001000

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
}

let register_file_get reg_file = function
  | PC -> reg_file.pc
  | SP -> reg_file.sp
  | SR -> reg_file.sr
  | R3 -> reg_file.r3
  | R4 -> reg_file.r4
  | R5 -> reg_file.r5
  | R6 -> reg_file.r6
  | R7 -> reg_file.r7
  | R8 -> reg_file.r8
  | R9 -> reg_file.r9
  | R10 -> reg_file.r10
  | R11 -> reg_file.r11

let register_file_set reg_file v = function
  | PC -> { reg_file with pc = v }
  | SP -> { reg_file with sp = v }
  | SR -> { reg_file with sr = v }
  | R3 -> { reg_file with r3 = v }
  | R4 -> { reg_file with r4 = v }
  | R5 -> { reg_file with r5 = v }
  | R6 -> { reg_file with r6 = v }
  | R7 -> { reg_file with r7 = v }
  | R8 -> { reg_file with r8 = v }
  | R9 -> { reg_file with r9 = v }
  | R10 -> { reg_file with r10 = v }
  | R11 -> { reg_file with r11 = v }

let register_file_0 =
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
  }

let register_file_init m =
  {
    pc = memory_get m 0xFFFE;
    sp = 0;
    sr = gie;
    r3 = 0;
    r4 = 0;
    r5 = 0;
    r6 = 0;
    r7 = 0;
    r8 = 0;
    r9 = 0;
    r10 = 0;
    r11 = 0;
  }
