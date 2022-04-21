type address = int

type immediate = int

type byte = int

type word = int

type register =
  | PC | SP | SR | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11

type instr =
  | RETI
  | NOP
  | HLT

  | IN  of register
  | OUT of register

  | JMP of address
  | JZ  of address

  | MOV       of register * register
  | MOV_LOAD  of address  * register
  | MOV_STORE of register * address
  | MOV_IMM   of register * immediate

  | NOT of register 
  | ADD of register * register
  | SUB of register * register
  | AND of register * register
  | CMP of register * register

let size = function
  | MOV_STORE(_,_) -> 2
  | MOV_IMM(_,_)   -> 2
  | NOT(_)         -> 2
  | _              -> 1

let cycles = function
  | RETI           -> 5
  | NOP            -> 1
  | HLT            -> 1
  | IN(_)          -> 2
  | OUT(_)         -> 2
  | JMP(_)         -> 2
  | JZ(_)          -> 2
  | MOV(_,_)       -> 1
  | MOV_LOAD(_,_)  -> 2
  | MOV_STORE(_,_) -> 4
  | MOV_IMM(_,_)   -> 2
  | NOT(_)         -> 2 
  | ADD(_,_)       -> 1
  | SUB(_,_)       -> 1
  | AND(_,_)       -> 1
  | CMP(_,_)       -> 1
