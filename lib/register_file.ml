open Ast

type register_file = { pc  : word
                     ; sp  : word
                     ; sr  : word
                     ; r3  : word
                     ; r4  : word
                     ; r5  : word
                     ; r6  : word
                     ; r7  : word
                     ; r8  : word
                     ; r9  : word
                     ; r10 : word
                     ; r11 : word       
                     }

let register_file_get reg_file = function
  | PC      -> reg_file.pc
  | SP      -> reg_file.sp
  | SR      -> reg_file.sr
  | R(R3)   -> reg_file.r3
  | R(R4)   -> reg_file.r4
  | R(R5)   -> reg_file.r5
  | R(R6)   -> reg_file.r6
  | R(R7)   -> reg_file.r7
  | R(R8)   -> reg_file.r8
  | R(R9)   -> reg_file.r9
  | R(R10)  -> reg_file.r10
  | R(R11)  -> reg_file.r11      

let register_file_set reg_file v = function
  | PC      -> { reg_file with pc  = v }  
  | SP      -> { reg_file with sp  = v }  
  | SR      -> { reg_file with sr  = v }  
  | R(R3)   -> { reg_file with r3  = v }  
  | R(R4)   -> { reg_file with r4  = v }  
  | R(R5)   -> { reg_file with r5  = v }  
  | R(R6)   -> { reg_file with r6  = v }  
  | R(R7)   -> { reg_file with r7  = v }  
  | R(R8)   -> { reg_file with r8  = v }  
  | R(R9)   -> { reg_file with r9  = v }  
  | R(R10)  -> { reg_file with r10 = v }   
  | R(R11)  -> { reg_file with r11 = v }           

let register_file_init = 
  { pc  = 0
  ; sp  = 0
  ; sr  = 0
  ; r3  = 0
  ; r4  = 0
  ; r5  = 0
  ; r6  = 0
  ; r7  = 0
  ; r8  = 0
  ; r9  = 0
  ; r10 = 0
  ; r11 = 0     
  }