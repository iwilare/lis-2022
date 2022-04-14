open Ast

type io_state = int 

type io_event = 
  | Epsilon
  | Read  of word
  | Write of word
  | Interrupt

type transition_type =
  | EpsilonTransition
  | InterruptTransition

type io_possibilities = {
   transition_type : transition_type;
   next_state      : io_state;
   next_read_state : io_state option;
}

type io_device = {
  states        : io_state list;
  init_state    : io_state;
  delta         : io_state -> io_possibilities; 
}