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

let rec advance device (k : int) ((io_state, t, arrival_time) as c) =
  match k with
  | 0 -> c
  | _ ->
      let next_config =
        match (List.assoc io_state device.delta).main_transition with
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
  ^ (Option.fold p.read_transition ~none:"NO_READ" ~some:(fun (w,c) -> "READ(" ^ Word.show w ^ "->" ^  string_of_int c ^ ")"))
  ^ ", write targets: " ^ String.concat "," (List.map (fun (w,_) -> Word.show w) p.write_transitions)
  ^ "\n"

let string_of_io_device d =
  "States: " ^ String.concat ", " (List.map string_of_int d.states) ^
  "\nInit state: " ^ string_of_int d.init_state ^
  "\nDelta:\n" ^ String.concat "\n" (List.map string_of_io_possibilities d.delta)