open Types

type 'io_state transition_type =
  | EpsilonTransition of 'io_state
  | InterruptTransition of 'io_state

type 'io_state io_possibilities = {
  (* Either epsilon or interrupt, one of the two *)
  main_transition : 'io_state transition_type;
  (* Output from the device, an input to the CPU *)
  read_transition : (word * 'io_state) option;
  (* Input to the device, a write from the CPU *)
  write_transitions : (word * 'io_state) list;
}

type 'io_state io_device = {
  states : 'io_state list;
  init_state : 'io_state;
  delta : ('io_state * 'io_state io_possibilities) list;
}

let io_device_get_possibilities io_device state =
  match List.assoc_opt state io_device.delta with
  | None -> failwith @@ "Delta undefined"
  | Some p -> p

let io_device_write_transitions io_device state word = (fun p -> List.assoc_opt word p.write_transitions) (io_device_get_possibilities io_device state)
let io_device_read_transition io_device state = (fun p -> p.read_transition) (io_device_get_possibilities io_device state)
let io_device_main_transition io_device state = (fun p -> p.main_transition) (io_device_get_possibilities io_device state)

let rec advance device (k : int) ((io_state, t, arrival_time) as c) =
  match k with
  | 0 -> c
  | _ ->
      let next_config =
        match io_device_main_transition device io_state with
        | EpsilonTransition d -> (d, t + 1, arrival_time)
        | InterruptTransition d -> (
            match arrival_time with
            | None -> (d, t + 1, Some t)
            | Some _ -> (d, t + 1, arrival_time))
      in
      advance device (k - 1) next_config

let default_io_device : int io_device =
  let default_max_states = 32 in
  let default_max_words = 32 in
  {
    states = [];
    init_state = 1;
    delta =
      List.init default_max_states succ (* [1..32] *)
        |> List.map (fun d ->
                       (d, {
                             main_transition = EpsilonTransition d;
                             read_transition = None;
                             write_transitions = List.init default_max_words succ
                                              |> List.map (fun w -> (Word.from_int w, w));
                           }));
  }

let string_of_io_possibilities (s, p) =
  "State: " ^ string_of_int s ^ ", " ^
  (match p.main_transition with
  | EpsilonTransition d -> "ε" ^ "(" ^ string_of_int d ^ ")"
  | InterruptTransition d -> "INT" ^ "(" ^ string_of_int d ^ ")")
  ^ ", "
  ^ (Option.fold p.read_transition ~none:"NO_READ" ~some:(fun (w,c) -> "READ(" ^ Word.show w ^ ", goto:" ^  string_of_int c ^ ")"))
  ^ ", write targets: [" ^ String.concat "," (List.map (fun (w,_) -> Word.show w) p.write_transitions) ^ "]"

let string_of_io_device d =
  "States: " ^ String.concat ", " (List.map string_of_int d.states) ^
  "\nInit state: " ^ string_of_int d.init_state ^
  "\nDelta:\n" ^ String.concat "\n" (List.map string_of_io_possibilities d.delta) ^ "\n"


(* Now follow some useful example devices. *)

(*
  Linear automaton in which:
    - each transition is epsilon except for one at time `when_interrupt`
    - each transition points to the next state
    - always outputs the elapsed time from the start as read transition
    - the last state makes the automata crash
*)

let security_relevant_device n_states when_interrupt =
  let states = List.init n_states succ in
  let delta_transitions =
    states
    |> List.map succ
    |> List.mapi (fun i next_state ->
      let current_state = i + 1 in
      {
        main_transition =
          (if current_state == when_interrupt then InterruptTransition next_state
          else EpsilonTransition next_state);
        read_transition = Some (Word.from_int current_state, next_state);
        write_transitions = [];
      }) in
    {
      states;
      init_state = 1;
      delta = List.combine states delta_transitions;
    }

let circular_on_demand_interrupt_io_device interrupt_delay max_post_interrupt_clock_length =
  let special_input_value = Word.from_int 42 in
  let states = List.init ((1 + interrupt_delay + max_post_interrupt_clock_length) + 1) succ in
  let delta_transitions =
    states
    |> List.map (fun i ->
      let next = i + 1 in
      if i == 1 then
        {
          main_transition = EpsilonTransition 1;
          read_transition = None;
          write_transitions = [(special_input_value, 2)]
        }
      else if 2 <= i && i <= interrupt_delay + 2 then
        {
          main_transition = if i == interrupt_delay + 2 then InterruptTransition next else EpsilonTransition next;
          read_transition = None;
          write_transitions = []
        }
      else (* (interrupt_delay + 2) + 1 <= i < ((interrupt_delay + 2) + 1) + max_post_interrupt_clock_length *)
        {
          main_transition = EpsilonTransition next;
          read_transition = Some (Word.from_int (i - (interrupt_delay + 2)), next);
          write_transitions = [(special_input_value, 1)]
        }) in
  {
    states;
    init_state = 1;
    delta = List.combine states delta_transitions
  }
