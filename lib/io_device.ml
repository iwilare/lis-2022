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
  read_transition : (word * io_state) option;
  (* Input to the device, a write from the CPU *)
  write_transitions : word -> io_state option;
}

type io_device = {
  states : io_state list;
  init_state : io_state;
  delta : io_state -> io_possibilities;
}

let rec advance dev (k : int) ((d0, t, ta) as c) =
  match k with
  | 0 -> c
  | _ -> begin
      match (dev.delta d0).main_transition with
      | EpsilonTransition d -> advance dev (k - 1) (d, t + 1, ta)
      | InterruptTransition d -> (d, t, t)
      end

let get_read_transition dev d = (dev.delta d).read_transition