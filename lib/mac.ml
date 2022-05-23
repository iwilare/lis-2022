open Memory
open Instr
open Config
open Types
open Layout
open Register_file

type memory_right = X | R | W

let permissions enc f t =
  match (get_memory_type enc f, get_memory_type enc t) with
  | EnclaveCode _, EnclaveCode _ -> [ R; X ]
  | EnclaveCode _, EnclaveData -> [ R; W ]
  | EnclaveCode _, Unprotected -> [ X ]
  | _, EnclaveCode { is_entry_point = true } -> [ X ]
  | _, EnclaveData -> []
  | _, EnclaveCode _ -> []
  | _ -> [ R; W; X ]

let mac enc f right t = List.mem right (permissions enc f t)

let rec mac_region enc f right t bytes =
  match bytes with
  | 0 -> true
  | _ ->
      mac enc f right t
      && mac_region enc f right (Word.inc t) (bytes - 1)

let mac_word enc f right w = mac_region enc f right w 2

let rec mac_valid i c =
  let rget r = register_get r c.r in
  let pc_old = c.pc_old in
  let pc = c.r.pc in
  let sp = c.r.sp in
  match c.b with
  | Some _ ->
      (match i with
      | RETI -> true
      | _ -> mac_valid i { c with b = None } && not (flag_gie c))
      && not (is_enclave_entry_point c.layout pc)
  | None -> (
      (* Check all current instruction bytes *)
      mac_region c.layout pc_old X pc (size i)
      &&
      match i with
      | IN _ | OUT _ -> cpu_mode c = Some UM
      | MOV_LOAD (r1, _) ->
          (not (is_touching_last_word_address (rget r1)))
          && mac_word c.layout pc R (rget r1)
      | MOV_STORE (_, r2) ->
          (not (is_touching_last_word_address (rget r2)))
          && mac_word c.layout pc W (rget r2)
      | RETI ->
          (not (is_touching_last_word_address sp))
          && (not (is_touching_last_word_address (Word.inc sp)))
          && mac_word c.layout pc R sp
          && mac_word c.layout pc R (Word.inc sp)
          (* Check that we can read PC and SP from the stack *)
      | _ -> true (* HALT should always be executable *))
