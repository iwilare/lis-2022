open Ast

type memory = byte Array.t

let memory_init : memory = Array.make 65536 0
let memory_get_byte : memory -> address -> byte = Array.get
let memory_get m a = Array.get m a + (Array.get m (a + 1) lsl 8)
let memory_set_byte = Array.set

let memory_set m a v =
  v lsr 8 |> Array.set m a;
  v lsl 8 |> Array.set m (a + 1)
