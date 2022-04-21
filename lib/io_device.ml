open Ast

type io_state = int
type io_event = Epsilon | Read of word | Write of word | Interrupt

type transition_type =
  | EpsilonTransition of io_state
  | InterruptTransition of io_state

type io_possibilities = {
  (* Either epsilon or interrupt, one of the two *)
  main_transition : transition_type;
  (* Output from the device, an input to the CPU *)
  read_transition : io_state option;
  (* Input to the device, a write to the CPU *)
  write_transitions : word -> io_state option;
}

type io_device = {
  states : io_state list;
  init_state : io_state;
  delta : io_state -> io_possibilities;
}
