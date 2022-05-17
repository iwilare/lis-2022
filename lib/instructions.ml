open Ast
open Types
open Memory
let reg_encode r =
  let rval =
    match r with
    | PC -> 0x0
    | SP -> 0x1
    | SR -> 0x2
    | R3 -> 0x3
    | R4 -> 0x4
    | R5 -> 0x5
    | R6 -> 0x6
    | R7 -> 0x7
    | R8 -> 0x8
    | R9 -> 0x9
    | R10 -> 0xA
    | R11 -> 0xB
    | R12 -> 0xC
    | R13 -> 0xD
    | R14 -> 0xE
    | R15 -> 0xF
  in
  rval |> Word.from_int

let reg_decode w =
  match w |> Word.to_int with
  | 0x0 -> Some PC
  | 0x1 -> Some SP
  | 0x2 -> Some SR
  | 0x3 -> Some R3
  | 0x4 -> Some R4
  | 0x5 -> Some R5
  | 0x6 -> Some R6
  | 0x7 -> Some R7
  | 0x8 -> Some R8
  | 0x9 -> Some R9
  | 0xA -> Some R10
  | 0xB -> Some R11
  | 0xC -> Some R12
  | 0xD -> Some R13
  | 0xE -> Some R14
  | 0xF -> Some R15
  | _ -> None

let opcode = function
  | NOP -> 0x1340
  | RETI -> 0x1300
  | HLT -> 0x1380
  | IN _ -> 0x1400
  | OUT _ -> 0x1800
  | JMP _ -> 0x3C00
  | JZ _ -> 0x2400
  | MOV (_, _) -> 0x4000
  | MOV_LOAD (_, _) -> 0x4020
  | MOV_STORE (_, _) -> 0x4090
  | MOV_IMM (_, _) -> 0x4030
  | NOT _ -> 0xE030
  | ADD (_, _) -> 0X6000
  | SUB (_, _) -> 0X7000
  | AND (_, _) -> 0XF000
  | CMP (_, _) -> 0X9000

let encode (i : instr) =
  let reg_encode_shift r = Word.(reg_encode r lsl 8) in
  let encode1 i r = Word.((opcode i |> from_int) lor reg_encode r) in
  let encode2 i r1 r2 =
    Word.((opcode i |> from_int) lor reg_encode_shift r1 lor reg_encode r2)
  in
  match i with
  | NOP
  | RETI
  | HLT -> (opcode i |> Word.from_int, None)
  | IN r
  | OUT r
  | JMP r
  | JZ r -> (encode1 i r, None)
  | MOV (r1, r2)
  | MOV_LOAD (r1, r2)
  | ADD (r1, r2)
  | SUB (r1, r2)
  | AND (r1, r2)
  | CMP (r1, r2)
  | MOV_STORE (r1, r2) ->
      (encode2 i r1 r2, None)
  | MOV_IMM (v, r) -> (encode1 i r, Some v)
  | NOT r -> (encode1 i r, Word.from_int 0xFFFF |> Option.some)

let decode v =
  let open Word in
  let get_reg w = w land from_int 0x000F |> reg_decode |> Option.get in
  let get_s_reg w =
    (w land from_int 0x0F00) lsr 8 |> reg_decode |> Option.get
  in
  match v with
  | w1, None when w1 land from_int 0xFFF0 = from_int 0x1300 -> Some RETI
  | w1, None when w1 land from_int 0xFFF0 = from_int 0x1340 -> Some NOP
  | w1, None when w1 land from_int 0xFFF0 = from_int 0x1380 -> Some HLT
  | w1, None when w1 land from_int 0xFF00 = from_int 0x1400 ->
      Some (IN (get_reg w1))
  | w1, None when w1 land from_int 0xFF00 = from_int 0x1800 ->
      Some (OUT (get_reg w1))
  | w1, None when w1 land from_int 0xFF00 = from_int 0x3C00 ->
      Some (JMP (get_reg w1))
  | w1, None when w1 land from_int 0xFF00 = from_int 0x2400 ->
      Some (JZ (get_reg w1))
  | w1, None when w1 land from_int 0xF000 = from_int 0xF000 ->
      Some (AND (get_s_reg w1, get_reg w1))
  | w1, None when w1 land from_int 0xF000 = from_int 0x6000 ->
      Some (ADD (get_s_reg w1, get_reg w1))
  | w1, None when w1 land from_int 0xF000 = from_int 0x7000 ->
      Some (SUB (get_s_reg w1, get_reg w1))
  | w1, None when w1 land from_int 0xF000 = from_int 0x9000 ->
      Some (CMP (get_s_reg w1, get_reg w1))
  | w1, None when w1 land from_int 0xF0F0 = from_int 0x4000 ->
      Some (MOV (get_s_reg w1, get_reg w1))
  | w1, None when w1 land from_int 0xF0F0 = from_int 0x4020 ->
      Some (MOV_LOAD (get_s_reg w1, get_reg w1))
  | w1, None when w1 land from_int 0xF0F0 = from_int 0x4090 ->
      Some (MOV_STORE (get_s_reg w1, get_reg w1))
  | w1, Some _ when w1 land from_int 0xF0F0 = from_int 0xE030 ->
      Some (NOT (get_reg w1))
  | w1, Some w2 when w1 land from_int 0xF0F0 = from_int 0x4030 ->
      Some (MOV_IMM (w2, get_reg w1))
  | _ -> None

let fetch_and_decode addr mem = 
  match memory_get mem addr with 
  | w when Word.(w land from_int 0xF0F0 = from_int 0xE030) ||  Word.(w land from_int 0xF0F0 = from_int 0x4030) -> 
    Word.(decode (w, memory_get mem addr+from_int 1 |> Option.some))
  | w  -> decode (w, None)



let put_instructions_in_memory start_point mem instructions= 
instructions
|> List.map encode
|> List.map fst 
|> List.iteri (fun i inst -> memory_set mem Word.(start_point + from_int i) inst )