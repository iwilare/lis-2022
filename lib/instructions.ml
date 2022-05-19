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
  | NOP -> Word.from_int 0x1340
  | RETI -> Word.from_int 0x1300
  | HLT -> Word.from_int 0x1380
  | IN _ -> Word.from_int 0x1400
  | OUT _ -> Word.from_int 0x1800
  | JMP _ -> Word.from_int 0x3C00
  | JZ _ -> Word.from_int 0x2400
  | MOV (_, _) -> Word.from_int 0x4000
  | MOV_LOAD (_, _) -> Word.from_int 0x4020
  | MOV_STORE (_, _) -> Word.from_int 0x4090
  | MOV_IMM (_, _) -> Word.from_int 0x4030
  | NOT _ -> Word.from_int 0xE030
  | ADD (_, _) -> Word.from_int 0X6000
  | SUB (_, _) -> Word.from_int 0X7000
  | AND (_, _) -> Word.from_int 0XF000
  | CMP (_, _) -> Word.from_int 0X9000

let identify code =
  let pick (mask, opcode, ist) = if Word.(code land mask = opcode) then Some(ist) else None in
  List.find_map pick
    [ Word.from_int 0xFFF0, Word.from_int 0x1300, RETI
    ; Word.from_int 0xFFF0, Word.from_int 0x1340, NOP
    ; Word.from_int 0xFFF0, Word.from_int 0x1380, HLT
    ; Word.from_int 0xFF00, Word.from_int 0x1400, IN R3
    ; Word.from_int 0xFF00, Word.from_int 0x1800, OUT R3
    ; Word.from_int 0xFF00, Word.from_int 0x3C00, JMP R3
    ; Word.from_int 0xFF00, Word.from_int 0x2400, JZ R3
    ; Word.from_int 0xF000, Word.from_int 0xF000, AND (R3,R3)
    ; Word.from_int 0xF000, Word.from_int 0x6000, ADD (R3,R3)
    ; Word.from_int 0xF000, Word.from_int 0x7000, SUB (R3,R3)
    ; Word.from_int 0xF000, Word.from_int 0x9000, CMP (R3,R3)
    ; Word.from_int 0xF0F0, Word.from_int 0x4000, MOV (R3,R3)
    ; Word.from_int 0xF0F0, Word.from_int 0x4020, MOV_LOAD (R3,R3)
    ; Word.from_int 0xF0F0, Word.from_int 0x4090, MOV_STORE (R3,R3)
    ; Word.from_int 0xF0F0, Word.from_int 0xE030, NOT R3
    ; Word.from_int 0xF0F0, Word.from_int 0x4030, MOV_IMM (Word.zero,R3)
    ]

let encode (i : instr) =
  let reg_encode_shift r = Word.(reg_encode r lsl 8) in
  let encode1 i r = Word.(opcode i lor reg_encode r) in
  let encode2 i r1 r2 =
    Word.(opcode i lor reg_encode_shift r1 lor reg_encode r2)
  in
  match i with
  | NOP
  | RETI
  | HLT -> [opcode i]
  | IN r
  | OUT r
  | JMP r
  | JZ r -> [encode1 i r]
  | MOV (r1, r2)
  | MOV_LOAD (r1, r2)
  | ADD (r1, r2)
  | SUB (r1, r2)
  | AND (r1, r2)
  | CMP (r1, r2) -> [encode2 i r1 r2]
  | MOV_STORE (r1, r2) -> [encode2 i r1 r2; Word.zero]
  | MOV_IMM (v, r) -> [encode1 i r; v]
  | NOT r -> [encode1 i r; w0xFFFF]

let decode (v : word list) =
  let get_reg w = Word.(w land from_int 0x000F |> reg_decode |> Option.get) in
  let get_s_reg w = Word.((w land from_int 0x0F00) lsr 8 |> reg_decode |> Option.get)
  in
  match v with
  | [w1] when identify w1 = Some RETI -> Some RETI
  | [w1] when identify w1 = Some NOP -> Some NOP
  | [w1] when identify w1 = Some HLT -> Some HLT
  | [w1] when identify w1 = Some(IN(R3)) -> Some(IN(get_reg w1))
  | [w1] when identify w1 = Some(OUT(R3)) -> Some(OUT(get_reg w1))
  | [w1] when identify w1 = Some(JMP(R3)) -> Some(JMP(get_reg w1))
  | [w1] when identify w1 = Some(JZ(R3)) -> Some(JZ(get_reg w1))
  | [w1] when identify w1 = Some(AND(R3,R3)) -> Some(AND(get_s_reg w1, get_reg w1))
  | [w1] when identify w1 = Some(ADD(R3,R3)) -> Some(ADD(get_s_reg w1, get_reg w1))
  | [w1] when identify w1 = Some(SUB(R3,R3)) -> Some(SUB(get_s_reg w1, get_reg w1))
  | [w1] when identify w1 = Some(CMP(R3,R3)) -> Some(CMP(get_s_reg w1, get_reg w1))
  | [w1] when identify w1 = Some(MOV(R3,R3)) -> Some(MOV(get_s_reg w1, get_reg w1))
  | [w1] when identify w1 = Some(MOV_LOAD(R3,R3)) -> Some(MOV_LOAD(get_s_reg w1, get_reg w1))
  | [w1;_] when identify w1 = Some(MOV_STORE(R3,R3)) -> Some(MOV_STORE(get_s_reg w1, get_reg w1))
  | [w1;_] when identify w1 = Some(NOT(R3)) -> Some(NOT(get_reg w1))
  | [w1;w2] when identify w1 = Some(MOV_IMM(Word.zero,R3)) -> Some(MOV_IMM (w2, get_reg w1))
  | _ -> None

let encode_and_put addr i = memory_set_words addr (encode i)

let fetch_and_decode addr m =
  Option.bind (identify (memory_get addr m))
    (fun prototype ->
      decode (memory_get_words addr (size_in_words prototype) m))

let rec encode_and_put_program addr is m =
  match is with
  | [] -> m
  | i :: is -> encode_and_put_program Word.(addr + from_int (size_in_words i)) is (encode_and_put addr i m)