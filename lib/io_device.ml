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

(**
   - device: current device instance
   - k: TODO
   - io_state: TODO
   - t: TODO
   - arrival_time: TODO
 *)
let rec advance device (k : int) ((io_state, t, arrival_time) as c) =
  match k with
  | 0 -> c
  | _ ->
      let next_config =
        match (device.delta io_state).main_transition with
        | EpsilonTransition d -> (d, t + 1, arrival_time)
        | InterruptTransition d -> (
            match arrival_time with
            | None -> (d, t + 1, Some t)
            | Some _ -> (d, t + 1, arrival_time))
      in
      advance device (k - 1) next_config

let default_io_device =
  {
    states = [];
    init_state = 0;
    delta =
      (fun x ->
        {
          main_transition = EpsilonTransition x;
          read_transition = None;
          write_transitions = (fun w -> Some w);
        });
  }
