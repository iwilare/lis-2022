open Types

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

type instr =
  | NOP
  | RETI
  | HLT
  | IN of register
  | OUT of register
  | JMP of register
  | JZ of register
  | MOV of register * register
  | MOV_LOAD of register * register
  | MOV_STORE of register * register
  | MOV_IMM of immediate * register
  | NOT of register
  | ADD of register * register
  | SUB of register * register
  | AND of register * register
  | CMP of register * register

(* Length in bytes *)
let size = function
  | MOV_STORE (_, _) -> 4
  | MOV_IMM (_, _) -> 4
  | NOT _ -> 4
  | _ -> 2

let max_instruction_size = 4

let cycles = function
  | RETI -> 5
  | NOP -> 1
  | HLT -> 1
  | IN _ -> 2
  | OUT _ -> 2
  | JMP _ -> 2
  | JZ _ -> 2
  | MOV (_, _) -> 1
  | MOV_LOAD (_, _) -> 2
  | MOV_STORE (_, _) -> 4
  | MOV_IMM (_, _) -> 2
  | NOT _ -> 2
  | ADD (_, _) -> 1
  | SUB (_, _) -> 1
  | AND (_, _) -> 1
  | CMP (_, _) -> 1

(* A simple instruction contains no jumps and does not interact with memory *)
let is_simple_instr = function
  | NOP
  | MOV (_, _)
  | NOT _
  | ADD (_, _)
  | SUB (_, _)
  | AND (_, _)
  | CMP (_, _) -> true
  | _ -> false

let max_cycles = 6

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

let string_of_instr = function
| NOP -> "NOP"
| RETI -> "RETI"
| HLT -> "HLT"
| IN _ -> "IN"
| OUT r -> "OUT " ^ string_of_register r
| JMP r -> "JMP " ^ string_of_register r
| JZ r -> "JZ " ^ string_of_register r
| MOV (r1, r2) -> "MOV " ^ string_of_register r1 ^ " " ^ string_of_register r2
| MOV_LOAD (r1, r2) -> "MOV_LOAD " ^ string_of_register r1 ^ " " ^ string_of_register r2
| MOV_STORE (r1, r2) -> "MOV_STORE " ^ string_of_register r1 ^ " " ^ string_of_register r2
| MOV_IMM (imm, r) -> "MOV_IMM " ^ Word.show imm ^ " " ^ string_of_register r
| NOT r -> "NOT " ^ string_of_register r
| ADD (r1, r2) -> "ADD " ^ string_of_register r1 ^ " " ^ string_of_register r2
| SUB (r1, r2) -> "SUB " ^ string_of_register r1 ^ " " ^ string_of_register r2
| AND (r1, r2) -> "AND " ^ string_of_register r1 ^ " " ^ string_of_register r2
| CMP (r1, r2) -> "CMP " ^ string_of_register r1 ^ " " ^ string_of_register r2
