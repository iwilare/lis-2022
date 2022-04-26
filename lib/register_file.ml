open Ast
open Memory

let zero : word = 0b00000000
let gie  : word = 0b00001000

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

let align_even x = x land 0xFFFE

let get_bit mask   x = x land mask > 0
let set_bit mask v x = x lor (if v then mask else zero)

let register_set enc r w = function
  | PC  -> { r with pc  = align_even w }
  | SP  -> { r with sp  = align_even w }
  | SR  ->
    begin
      match cpu_mode_of_address enc (register_get r PC) with
      | Some(PM) ->
        { r with sr = w |> set_bit gie (get_bit gie (register_get r SR)) }
      | _ -> (* Remember Some(UM) *)
        { r with sr = w }
    end
  | R3  -> { r with r3  = w }
  | R4  -> { r with r4  = w }
  | R5  -> { r with r5  = w }
  | R6  -> { r with r6  = w }
  | R7  -> { r with r7  = w }
  | R8  -> { r with r8  = w }
  | R9  -> { r with r9  = w }
  | R10 -> { r with r10 = w }
  | R11 -> { r with r11 = w }

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
